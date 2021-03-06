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
  // TODO: check that segments are actually contiguous and if not do allocation/copying etc.
  array_t  result;
  array_t *segments = (array_t *) arrays.data;

  result.length = 0;

  if(arrays.length > 0) {
    result.data = segments[0].data;

    for(int i = 0; i < arrays.length; i++) {
      result.length += segments[i].length;
    }
  }

  return result;
}

int print_array(array_t array) {
  int *data = (int *) array.data;
  int len = array.length;

  printf("[ ");

  if(len <= 15) {
    for(int i = 0; i < len; i++) {
      if(i != 0) {
        printf(", ");
      }

      printf("%d", data[i]);
    }
  } else {
    printf("%d, %d, %d, %d, %d ... %d, %d, %d, %d, %d",
           data[0], data[1], data[2], data[3], data[4],
           data[len-5], data[len-4], data[len-3], data[len-2], data[len-1]);
  }

  printf(" ] (len=%d)\n", len);
  return 0;
}
