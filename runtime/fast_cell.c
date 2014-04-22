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
#define EMPTY  0
#define FULL   1

/*
 * Cell for status and either value or pointer to value.
 */
typedef struct {
  uint32_t  status;
  char      pad[PAD(sizeof(uint32_t),sizeof(void*))];
} cell_t;

/*
 * Creates a new queue with a single 'dummy' node.
 */
__attribute__((always_inline)) void fast_cell_init(cell_t *cell, size_t size) {
  cell->status = EMPTY;
}

/*
 * Returns a memory address for the cell. For a cell, this is effectively the
 * identity function. Note that should only ever be called when the cell is
 * empty, as it always gives the same cell.
 */
__attribute__((always_inline)) cell_t *fast_cell_allocate(cell_t *cell, size_t size) {
  return cell;
}

/*
 * Returns the address within the cell that should be used for data.
 */
__attribute__((always_inline)) void *fast_cell_data(cell_t *cell, size_t size) {
  return cell + 1;
}

/*
 * Marks the cell as full.
 */
__attribute__((always_inline)) void fast_cell_enqueue(cell_t *cell, cell_t *msg) {
  cell->status = FULL;
}

/*
 * If the cell is full, this becomes the identity function. Otherwise it
 * returns NULL.
 */
__attribute__((always_inline)) cell_t *fast_cell_find(cell_t *cell) {
  if(cell->status == FULL) {
    return cell;
  } else {
    return NULL;
  }
}

/*
 * Marks the cell as empty.
 */
__attribute__((always_inline)) void fast_cell_consume(cell_t * cell, cell_t *msg, size_t size) {
  msg->status = EMPTY;
}

