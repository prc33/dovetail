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

#ifndef DOVETAILCOMMON
#define DOVETAILCOMMON

#include <stdint.h>
#include <stdbool.h>

//#define DOVETAIL_DEBUG 1

#define CACHE_LINE    64

// Taken from Wool - need for the "& ((b)-1)" is unclear...
#define PAD(x,b) ( ( (b) - ((x)%(b)) ) & ((b)-1) ) /* b must be power of 2 */

uint32_t llvm_atomic_load(uint32_t *);
uint32_t llvm_atomic_load_acquire(uint32_t *);
void llvm_atomic_store(uint32_t *, uint32_t);
void llvm_atomic_store_release(uint32_t *, uint32_t);
void llvm_atomic_fence();
bool llvm_cas(uint32_t *, uint32_t, uint32_t);

#endif

