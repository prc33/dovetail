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

#define GC_THREADS
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include "gc.h"

#include "dovetail-common.h"

/*
 * Valid states for messages in a signal queue.
 */
#define CONSUMED  0
#define PENDING   1
#define CLAIMED   2

/*
 * Cell for status and either value or pointer to value.
 */
typedef volatile struct {
  uint32_t  status;
  char      pad[PAD(sizeof(uint32_t),sizeof(void*))];
} cell_t;

void llvm_atomic_store_ptr(cell_t **, cell_t *);
bool llvm_cas_ptr(cell_t **, cell_t *, cell_t *);

/*
 * Wrapper for cell. This indirection is required to prevent the ABA problem.
 */
typedef volatile struct {
  cell_t   *cell;
} queue_t;

/*
 * Creates a new queue with no cell yet attached.
 */
__attribute__((always_inline)) void slow_cell_init(queue_t *queue, size_t size) {
  // Not required as instance allocated with GC_MALLOC which zeroes memory.
  //queue->cell = NULL;
}

/*
 * Returns a memory address for a cell.
 */
__attribute__((always_inline)) cell_t *slow_cell_allocate(queue_t *queue, size_t size) {
  // Note that GC_MALLOC zeroes memory.
  cell_t *cell = (cell_t *) GC_MALLOC(sizeof(cell_t) + size);

  if(cell == NULL) {
    fprintf(stderr, "Out of memory (slow_cell_allocate)\n");
    exit(-1);
  }
  
  cell->status = PENDING;

  return cell;
}

/*
 * Returns the address within the cell that should be used for data.
 */
__attribute__((always_inline)) void *slow_cell_data(cell_t *cell, size_t size) {
  return cell + 1;
}

/*
 * Enqueues the given cell. This most only be performed on an empty cell
 * otherwise the existing value is overwritten.
 */
__attribute__((always_inline)) void slow_cell_enqueue(queue_t *queue, cell_t *cell) {
  queue->cell = cell;
}

/*
 * If the cell is still pending, this becomes the identity function. Otherwise it
 * returns NULL (setting retry to true if in the claimed state).
 */
__attribute__((always_inline)) cell_t *slow_cell_find(queue_t *queue, bool *retry) {
  cell_t *cell = queue->cell;

  if(cell == NULL) {
    return NULL;
  }

  switch(cell->status) {
    case PENDING:
      return cell;
    case CLAIMED:
      *retry = true;
      return NULL;
    // If cell is consumed, try to stop future checks from doing this indirection.
    // It doesn't matter if the CAS fails, as that only occurs with another enqueue.
    case CONSUMED:
      llvm_cas_ptr(&queue->cell, cell, NULL);
    default:
      return NULL;
  }
}

/*
 * Tries to claim the given message.
 */
__attribute__((always_inline)) bool slow_cell_try_claim(cell_t *cell) {
  return llvm_cas(&cell->status, PENDING, CLAIMED);
}

/*
 * Reverts a claimed message to pending.
 */
__attribute__((always_inline)) void slow_cell_revert(cell_t *cell) {
  cell->status = PENDING;
}

/*
 * Marks the cell as empty.
 */
__attribute__((always_inline)) void slow_cell_consume(cell_t *cell) {
  cell->status = CONSUMED;
}

/*
 * Checks whether the given message has been used up.
 */
__attribute__((always_inline)) bool slow_cell_is_consumed(cell_t *cell) {
  return (cell->status == CONSUMED);
}
