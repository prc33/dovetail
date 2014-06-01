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
 * No work to do when the queue is simply used as a memory location.
 */
__attribute__((always_inline)) void fast_mem_init(void *ptr, size_t size) {
  // Nothing
}

/*
 * Returns a memory address for the memory location. For a single item memory
 * queue, this is therefore the identity function. Note that it should only
 * ever be called once in a transition, and that transition must have matched
 * on the channel (otherwise the fixed size, length 1 queue assumption is
 * broken). Enqueuing a value is also a no-op since the location always has a
 * value. Remember that this implementation is for fast-mode so we don't need
 * to worry about data races.
 */
__attribute__((always_inline)) void *fast_mem_enqueue(void *ptr, size_t size) {
  return ptr;
}

/*
 * Returns the address of the memory location (i.e. identity)
 */
__attribute__((always_inline)) void *fast_mem_data(void *ptr, void *ptr_dup) {
  return ptr;
}

/*
 * Since the location always has a value, this is the identity function.
 */
__attribute__((always_inline)) void *fast_mem_find(void *ptr, size_t size) {
  return ptr;
}

/*
 * Is only ever called when the location is about to be filled again, so is a
 * no-op (like enqueue).
 */
__attribute__((always_inline)) void fast_mem_consume(void *ptr, void *ptr2, size_t size) {
  // Nothing
}
