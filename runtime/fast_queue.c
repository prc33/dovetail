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


__attribute__((always_inline)) void fast_queue_init(void *ptr, size_t size) {
  // TODO
}

__attribute__((always_inline)) void *fast_queue_allocate(void *ptr, size_t size) {
  printf("UNIMPLEMENTED fast_queue IS BEING USED. DISASTER LIKELY!\n");
  // TODO
  return ptr;
}

__attribute__((always_inline)) void *fast_queue_data(void *ptr, size_t size) {
  // TODO
  return ptr;
}

__attribute__((always_inline)) void fast_queue_enqueue(void *ptr, void *ptr2) {
  // TODO
}

__attribute__((always_inline)) void *fast_queue_find(void *ptr) {
  // TODO
  return ptr;
}

__attribute__((always_inline)) void fast_queue_consume(void *ptr) {
  // TODO
}
