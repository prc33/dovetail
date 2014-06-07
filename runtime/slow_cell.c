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
 * Cell version of a queue. The version is used to prevent the ABA problem occuring.
 */
typedef volatile struct {
  union {
    void *value;
    struct {
      uint32_t version;
      uint32_t status;
    };
  };
} queue_t;

bool llvm_cas_ptr(void **, void *, void *);

/*
 * Creates a new queue with no cell yet attached.
 */
__attribute__((always_inline)) void slow_cell_init(queue_t *queue, size_t size) {
  // Not required as instance allocated with GC_MALLOC which zeroes memory
  // queue->version = 0;
  // queue->status  = CONSUMED;
}

/*
 * Returns the new version for the cell.
 */
__attribute__((always_inline)) int32_t slow_cell_allocate(queue_t *queue, size_t size) {
  return queue->version + 1;
}

/*
 * Returns the address within the cell that should be used for data.
 */
__attribute__((always_inline)) void *slow_cell_data(queue_t *queue, int32_t version) {
  return queue + 1;
}

/*
 * Enqueues the given cell. This must only be performed on an empty cell
 * otherwise the existing value is overwritten.
 */
__attribute__((always_inline)) void slow_cell_enqueue(queue_t *queue, int32_t version) {// count=snapshot (assert snapshot=count+1), set as pending
  // asset queue.version == version - 1
  queue->version = version;
  queue->status = PENDING;
}

/*
 * If the cell is still pending, this becomes the identity function. Otherwise it
 * returns NULL (setting retry to true if in the claimed state).
 */
__attribute__((always_inline)) int32_t slow_cell_find(queue_t *queue, bool *retry) {
  while(true) {
    int32_t version = queue->version;
    int32_t status = queue->status;
 
    // Make sure we get a consistent read.
    if(version == queue->version) {
      switch(queue->status) {
        case PENDING:
          return version;
        case CLAIMED:
          *retry = true;
          return NULL;
        case CONSUMED:
        default:
          return NULL;
      }
    }
  }
}

/*
 * Tries to claim the given message.
 */
__attribute__((always_inline)) bool slow_cell_try_claim(queue_t *queue, int32_t version) {
  queue_t old = (queue_t) { .version = version, .status = PENDING };
  queue_t new = (queue_t) { .version = version, .status = CLAIMED };

  return llvm_cas_ptr(&queue->value, old.value, new.value);
}

/*
 * Reverts a claimed message to pending.
 */
__attribute__((always_inline)) void slow_cell_revert(queue_t *queue, int32_t version) {
  queue->status = PENDING; // assert version == version
}

/*
 * Marks the cell as empty.
 */
__attribute__((always_inline)) void slow_cell_consume(queue_t *queue, int32_t version) {
  queue->status = CONSUMED; // assert version == version
}

/*
 * Checks whether the given message has been used up.
 */
__attribute__((always_inline)) bool slow_cell_is_consumed(queue_t *queue, int32_t version) {
  // Double check ensures consistent read.
  return (queue->version != version) || (queue->status == CONSUMED) || (queue->version != version);
}
