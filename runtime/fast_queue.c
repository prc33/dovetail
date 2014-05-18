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

typedef struct {
  void   *data;
  void   *top;
  size_t  capacity;
} queue_t;

#define INITIAL_CAPACITY 32

__attribute__((always_inline)) void fast_queue_init(queue_t *queue, size_t size) {
  // Initial buffer is allocated within instance.
  queue->data = queue + 1;
  queue->top = queue->data;
  queue->capacity = INITIAL_CAPACITY;
}

__attribute__((always_inline)) void *fast_queue_allocate(queue_t *queue, size_t size) {
  // TODO: check capacity
  return queue->top;
}

__attribute__((always_inline)) void *fast_queue_data(void *msg, size_t size) {
  return msg;
}

__attribute__((always_inline)) void fast_queue_enqueue(queue_t *queue, void *msg) {
  queue->top = msg;
}

__attribute__((always_inline)) void *fast_queue_find(queue_t *queue) {
  if(queue->top > queue->data) {
    return queue->top;
  } else {
    return NULL;
  }
}

__attribute__((always_inline)) void fast_queue_consume(queue_t *queue, void *msg, size_t size) {
  queue->top = msg - size;
}
