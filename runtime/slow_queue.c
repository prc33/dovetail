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

struct node;

/*
 * Message node.
 */
struct node {
  struct node  *next;
  uint32_t      status;
  uint32_t      dummy;// for alignment purposes
};

typedef struct node node_t;

bool llvm_cas_ptr(node_t **, node_t *, node_t *);

/*
 * Queue along with list of associated transitions.
 */
typedef struct {
  struct node  *head;
  struct node  *tail;
} queue_t;

/*
 * Creates a new queue with a single 'dummy' node.
 */
__attribute__((always_inline)) void slow_queue_init(queue_t *queue, size_t size) {
  // Note that GC_MALLOC zeroes memory, so sentinel node doesn't need further setup (CONSUMED must be 0).
  queue->head = (node_t *) GC_MALLOC(sizeof(node_t));
  queue->tail = queue->head;
  llvm_atomic_fence();

  if(queue->head == NULL) {    
    fprintf(stderr, "Out of memory (slow_queue_init)\n");
    exit(-1);
  }
}

/*
 * Allocates a new garbage-collected node, setting the fields to default
 * values. The pointer returned is to the space for values. This allows more
 * generic code in the emission functions.
 */
__attribute__((always_inline)) node_t *slow_queue_allocate(queue_t *queue, size_t size) {
  // Note that GC_MALLOC zeroes memory.
  node_t *node = (node_t *) GC_MALLOC(sizeof(node_t) + size);

  if(node == NULL) {
    fprintf(stderr, "Out of memory (slow_queue_allocate)\n");
    exit(-1);
  }
  
  node->status = PENDING;
  llvm_atomic_fence();

  return node;
}

/*
 * Returns the data pointer for this node.
 */
__attribute__((always_inline)) void *slow_queue_data(queue_t *queue, node_t *node) {
  return node + 1;
}

/*
 * This places the message in the queue.
 */
__attribute__((always_inline)) void slow_queue_enqueue(queue_t *queue, node_t *node) {
  // Keep retrying as necessary until the node is in the queue.
  while(true) {
    node_t *tail = llvm_atomic_load_ptr_seqcst(&queue->tail);
    node_t *next = llvm_atomic_load_ptr_seqcst(&tail->next);
    
    // Only proceed if the reads of 'tail' and 'next' were consistent.
    if(tail == llvm_atomic_load_ptr_seqcst(&queue->tail)) {
      // Is 'tail' the real tail?
      if(next == NULL) {
        // Try to link in the new node (exit on success).
        if(llvm_cas_ptr(&tail->next, next, node)) {
          // Update the queue tail pointer as well (if not done elsewhere).
          llvm_cas_ptr(&queue->tail, tail, node);
          break;
        }
      // 'tail' has fallen behind, try to update it.
      } else {
        llvm_cas_ptr(&queue->tail, tail, next);
      }
    }
  }
}

/*
 * Checks whether the given message has been used up.
 */
__attribute__((always_inline)) bool slow_queue_is_consumed(queue_t *queue, node_t *node) {
  return (llvm_atomic_load_seqcst(&node->status) == CONSUMED);
}

/*
 * Looks through the given queue looking for a message tagged as 'pending',
 * returning one if it does. Any nodes marked as logically 'deleted' that are
 * seen during the traversal are removed (except the last which is used as the
 * queue's dummy node). If no 'pending' messages are found, but a 'claimed'
 * message is seen, then the 'retry' flag is set to true.
 */
__attribute__((always_inline)) void *slow_queue_find(queue_t *queue, bool *retry) {
  // 'head' is always a dummy value, so we can ignore it for the search.
  node_t *node = llvm_atomic_load_ptr_seqcst(&queue->head);

  while(true) {
    node_t *next = llvm_atomic_load_ptr_seqcst(&node->next);
    node_t *tail = llvm_atomic_load_ptr_seqcst(&queue->tail);

    // Check that the reads were consistent.
    if(next == llvm_atomic_load_ptr_seqcst(&node->next)) {
      // Check we're not working beyond 'tail' ('tail' will never move back).
      if(node == tail) {
        // We've searched the whole queue, not found a pending message.
        if(next == NULL) {
          return 0;
        // 'tail' must have fallen beyond, try to correct.
        } else {
          llvm_cas_ptr(&queue->tail, tail, next);
        }
      //
      } else {
        switch(llvm_atomic_load_seqcst(&next->status)) {
          // Found a PENDING message.
          case PENDING:
            return next;
          // Saw a CLAIMED message (match search will need to retry).
          case CLAIMED:
            *retry = true;
            node = next;
            break;
          // Delete any removed messages.
          case CONSUMED:
            // When the list starts with a consumed item, we should advance the
            // head. This allows the list to reach the empty state (rather than
            // always having a single CONSUMED node).
            llvm_cas_ptr(&queue->head, node, next);
        
            // We can't touch the tail as this would interfere with inserts.
            if(next != tail) {
              llvm_cas_ptr(&node->next, next, next->next);    
            // Only advance 'node' if we didn't delete 'next' (to ensure that
            // 'next->next' is considered).
            } else {
              node = next;
            }
        }
      }
    }
  }
}

/*
 * Tries to claim the given message.
 */
__attribute__((always_inline)) bool slow_queue_try_claim(queue_t *queue, node_t *node) {
  return llvm_cas(&node->status, PENDING, CLAIMED);
}

/*
 * Reverts a claimed message to pending.
 */
__attribute__((always_inline)) void slow_queue_revert(queue_t *queue, node_t *node) {
  llvm_atomic_store_seqcst(&node->status, PENDING);
}


/*
 * Reverts a claimed message to pending.
 */
__attribute__((always_inline)) void slow_queue_consume(queue_t *queue, node_t *node) {
  llvm_atomic_store_seqcst(&node->status, CONSUMED);
}

