/*
 * Dovetail JCAM
 *
 * Copyright 2014 Peter Calvert <prc33@cam.ac.uk>
 *                University of Cambridge Computer Laboratory
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License
 */

#define _GNU_SOURCE
#define GC_THREADS

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <pthread.h>
#include <sched.h>

#include "dovetail-common.h"
#include "gc.h"

/*
 * Threshold factor for entering fast mode.
 */
#ifndef FAST_FACTOR
  #define FAST_FACTOR 1
#endif

/*
 * Reasonable default for maximum number of work items that can be placed on an
 * individual worker queue (especially given that fast mode does not use these
 * queues).
 */
#ifndef WORK_LIMIT
  #define WORK_LIMIT 16384 // MUST BE A POWER OF 2
#endif

/*
 * Reasonable default for size of matching structure.
 */
#ifndef MATCH_SIZE
  #define MATCH_SIZE 128
#endif

typedef struct {
  char pad[MATCH_SIZE];
} match_t;

/*
 * Event counting for each worker.
 */
typedef struct {
  int match_pushes;
  int match_pops;
  int match_victim;
  int match_steals;
} counter_t;

/*
 * Individual Worker Data
 */
typedef struct {
  // 1st Cache Line (written by owner, only read by stealers)
  match_t  *array;
  const int index;
  uint32_t  bottom;
  uint32_t  cached_top;
  uint32_t  fast_threshold;
  counter_t counters;
  char      pad1[PAD(4*sizeof(uint32_t) + sizeof(counter_t) + sizeof(match_t *),CACHE_LINE)];

  // 2nd Cache Line (written by stealers as well as owner)
  uint32_t  top;
  char      pad2[PAD(sizeof(uint32_t), CACHE_LINE)];
} worker_t;

/*
 * LLVM Trampoline (always inlined) to allow non-standard and faster calling
 * conventions to be used.
 */
void worker_trampoline(worker_t *, match_t *);

/*
 * Main Constructor
 */
void construct_main(worker_t *, int);

/*
 * Global Worker Data
 */
static int worker_count;
static worker_t *workers;

static volatile int more = 1;

static volatile int result;

/*
 * Gives the next match location on the queue (without enqueuing). This allows
 * it to be populated in-place without an extra copy operation.
 */
__attribute__((always_inline)) match_t *match_alloc(worker_t *self) {
  return &(self->array[self->bottom & (WORK_LIMIT - 1)]);
}

/*
 * Pushes the next match location onto the bottom of the queue (only called by
 * the worker that owns the queue).
 *
 * see: Chase & Lev, "Dynamic Circular Work-Stealing Deque" (SPAA 2005)
 *      Le et al.,   "Correct and Efficient Work-Stealing for Weak Memory
 *                    Models" (PPoPP 2013)
 */
__attribute__((always_inline)) void match_push(worker_t *self) {
  int b = llvm_atomic_load(&self->bottom);
  
  if((b - self->cached_top) > (WORK_LIMIT - 1)) {
    int t = llvm_atomic_load_acquire(&self->top);
  
    self->cached_top = t;

    if((b - t) > (WORK_LIMIT - 1)) {
      fprintf(stderr, "Work queue overflow (size = %d)\n", WORK_LIMIT);
      exit(-1);
    }
  }

  llvm_atomic_store_release(&self->bottom, b + 1);

#if DOVETAIL_DEBUG
  self->counters.match_pushes++;
#endif
}

/*
 * Returns a pointer to the match that was previously at the bottom of the
 * queue. This location will be the same as returned by the next call to
 * match_alloc so all data that will be needed must be stored beforehand.
 * This function is only called by the worker that owns the queue.
 * 
 * see: Chase & Lev, "Dynamic Circular Work-Stealing Deque" (SPAA 2005)
 *      Le et al.,   "Correct and Efficient Work-Stealing for Weak Memory
 *                    Models" (PPoPP 2013)
 */
__attribute__((always_inline)) match_t *match_pop(worker_t *self) {
  int b = llvm_atomic_load(&self->bottom) - 1;
  llvm_atomic_store(&self->bottom, b);

  llvm_atomic_fence();
  
  int t = llvm_atomic_load(&self->top);

  if(t < b) {
#if DOVETAIL_DEBUG
    self->counters.match_pops++;
#endif
    return &(self->array[b & (WORK_LIMIT - 1)]);
  } else if(t == b) {
    if(!llvm_cas(&self->top, t, t + 1)) {
      llvm_atomic_store(&self->bottom, b + 1);
      return 0;
    } else {
      llvm_atomic_store(&self->bottom, b + 1);
#if DOVETAIL_DEBUG
      self->counters.match_pops++;
#endif
      return &(self->array[b & (WORK_LIMIT - 1)]);
    }
  } else {
    llvm_atomic_store(&self->bottom, b + 1);
    return 0;
  }
}

/*
 * Removes the match from the top of the queue and copies it into the given
 * memory. This function can be called by any worker, not just the owner. The
 * copy is required because after the compare-and-swap the memory could be
 * allocated by a concurrent call to match_alloc by the owner.
 * 
 * see: Chase & Lev, "Dynamic Circular Work-Stealing Deque" (SPAA 2005)
 *      Le et al.,   "Correct and Efficient Work-Stealing for Weak Memory
 *                    Models" (PPoPP 2013)
 */
__attribute__((always_inline)) match_t *match_steal(worker_t *self, match_t *dest) {
  int t = llvm_atomic_load_acquire(&self->top);

  llvm_atomic_fence();

  int b = llvm_atomic_load_acquire(&self->bottom);

  if(t < b - 1) {
    memcpy(dest, &(self->array[t & (WORK_LIMIT - 1)]), sizeof(match_t));

    if(!llvm_cas(&self->top, t, t + 1)) {
      return 0;
    } else {
#if DOVETAIL_DEBUG
//      self->counters.match_victim++;
#endif
      return dest;
    }
  } else {
    return 0;
  }
}

/*
 * 
 */
__attribute__((always_inline)) bool dovetail_go_fast(worker_t *self) {
#if DOVETAIL_DISABLEFAST
  return false;
#else
  return (self->bottom - self->top) > self->fast_threshold;
#endif
}

/*
 * Should be called at completion of program execution to terminate workers.
 */
__attribute__((always_inline)) void dovetail_end(int x) {
  more = 0;
  result = x;
}

/*
 * Main loop for each individual worker. Runs until the 'more' flag indicates
 * the end of execution.
 */
static void *worker(void *arg) {
  worker_t * const self = (worker_t *) arg;
  const int wc = worker_count;

  // Assign this worker to the same numbered core.
  cpu_set_t set;

  CPU_ZERO( &set );
  CPU_SET( self->index, &set );
  sched_setaffinity(0, sizeof(cpu_set_t), &set );

  // Try to give all workers the chance to assign themselves to cores before
  // we start real work.
  sched_yield();

  // Main worker loop.
  while(more) {
    match_t tmp;
    match_t *m = 0;

    // Try to find local work.
    m = match_pop(self);

    // Execute via LLVM stub that will be inlined and uses 'fastcc'.
    if(m) {
      worker_trampoline(self, m);
      continue;
    }

    // Otherwise steal from another worker.
    for(int victim = (self->index + 1) % wc; more; victim = (victim + 1) % wc) {
      m = match_steal(&workers[victim], &tmp);

      if(m) {
#if DOVETAIL_DEBUG
        self->counters.match_steals++;
#endif

        worker_trampoline(self, m);
        break;
      }
    }
  }

#if DOVETAIL_DEBUG
  fprintf(stderr, "Worker #%d terminating.\n", self->index);
#endif
  
  return 0;
}

int main(int argc, char* argv[]) {
  worker_count = atoi(argv[1]);
  int n = atoi(argv[2]);
  
#if DOVETAIL_DEBUG
  fprintf(stderr, "Number of threads/workers = %d\n", worker_count);
#endif

  GC_INIT();
  
  // Allocate workers to cacheline aligned memory. The size of worker_t should
  // be 2 cachelines.
  workers = (worker_t *) aligned_alloc(CACHE_LINE, worker_count * sizeof(worker_t));

  // Initialise worker queues to be empty.
  for(int i = 0; i < worker_count; i++) {
    worker_t init = {
      .index = i,
      .bottom = 0,
      .cached_top = 0,
      .top = 0,
      .fast_threshold = worker_count * FAST_FACTOR,
      .counters = { .match_pushes = 0, .match_pops = 0, .match_steals = 0 },
      .array = (match_t *) GC_MALLOC_UNCOLLECTABLE(WORK_LIMIT * sizeof(match_t))
    };

    memcpy(&workers[i], &init, sizeof(worker_t));
  }

  // Allow threads system (rather than process) scope
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM);

  // Create space for threads
  pthread_t *threads = (pthread_t *) malloc(worker_count * sizeof(pthread_t));

  // Start workers 1 to (N-1)
  for(int i = 1; i < worker_count; i++) {
    pthread_create(&threads[i], &attr, worker, &workers[i]);
  }

  // Launch 'main' constructor (MUST be slow case)
  construct_main(&workers[0], n);
  
  // Start worker 0
  worker(&workers[0]);

  // Join on other workers.
  for(int i = 1; i < worker_count; i++) {
    pthread_join(threads[i], NULL);
  }
  
#if DOVETAIL_DEBUG
  fprintf(stderr, "------------------------------------------\n");
  fprintf(stderr, "Worker   Pushes     Pops   Victim   Steals\n");
  fprintf(stderr, "------------------------------------------\n");
  
  for(int i = 0; i < worker_count; i++) {
    fprintf(
      stderr, "%6d %8d %8d %8d %8d\n", i,
      workers[i].counters.match_pushes,
      workers[i].counters.match_pops,
      workers[i].counters.match_victim,
      workers[i].counters.match_steals
    );
  }

  fprintf(stderr, "------------------------------------------\n");
#endif

  return result;
}
