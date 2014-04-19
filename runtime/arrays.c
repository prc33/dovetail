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
 * Representation of arrays
 */
typedef struct {
  uint32_t  length;
  char     *data;
} array_t;

/*
 * Splits an array into parts based on a 'segment descriptor'.
 */
__attribute__((always_inline)) void arrays_split(array_t *segments, array_t array, array_t segdes, size_t size) {
  char     *data    = array.data;
  uint32_t *lengths = (uint32_t *) segdes.data;

  for(int i = 0; i < segdes.length; i++) {
    segments[i].length = lengths[i];
    segments[i].data   = data;

    data = data + (lengths[i] * size);
  }
}

/*
 * Merges multiple arrays back into one - provided that they are contiguous.
 */
__attribute__((always_inline)) array_t arrays_merge(array_t arrays, size_t size) {
  array_t  result;
  array_t *segments = (array_t *) arrays.data;

  // TODO: implement merge

  return arrays;
}
