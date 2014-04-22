/*
 * Code originally from the Cilk project (by Keith Randall)
 *   Copyright (c) 2000 Massachusetts Institute of Technology
 *   Copyright (c) 2000 Matteo Frigo
 * 
 * Changed to Cilk++ to fit multi-model BOTS
 *   Artur Podobas, Royal Institute of Technology, 2010
 *
 * Part of the Barcelona OpenMP Tasks Suite
 *   Copyright (c) 2009 Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
 *   Copyright (c) 2009 Universitat Politecnica de Catalunya
 *
 * Adapted for use as Dovetail benchmark
 *   Copyright (c) 2014 Peter Calvert
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

#ifdef WOOL
#include "wool.h"
#endif

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <alloca.h>

#if 0
// Checking information
static int solutions[] = {
        1,
        0,
        0,
        2,
        10, /* 5 */
        4,
        40,
        92,
        352,
        724, /* 10 */
        2680,
        14200,
        73712,
        365596,
};
#endif

/*
 * The position of queens is described throughout by an array a, where a[i]
 * gives the row of the queen in column i.
 */

#ifdef WOOL
TASK_DECL_4(int, nqueens_next, int, int, char*, int);
#endif


/*
 * Checks whether the first <n> queen positions in <a> conflict or not. Returns
 * true if the positions are valid.
 */
bool ok(int n, char *a) {
  for(int i = 0; i < n; i++) {
    char p = a[i];

    for(int j = i + 1; j < n; j++) {
      char q = a[j];

      if((q == p) || (q == p - (j - i)) || (q == p + (j - i))) {
        return false;
      }
    }
  }

  return true;
}

/*
 * Solves the n-queens problem, assuming that the first <j> positions of <a>
 * are already set and valid. Returns the number of possible solutions from
 * that state.
 */
#ifdef WOOL
TASK_3(int, nqueens, int, n , int, j, char*, a) {
#else
int nqueens(int n, int j, char *a) {
#endif
  // No more queens to place.
  if(n == j) {
    return 1;
  }

  // Try each possible position for queen <j>
  int solutions = 0;
  
  for(int i = 0; i < n; i++) {
#ifdef WOOL
    SPAWN(nqueens_next, n, j, a, i);
#else
    a[j] = i;

    if(ok(j + 1, a)) {
      solutions += nqueens(n, j + 1, a);
    }
#endif
  }

#ifdef WOOL
  for(int i = 0; i < n; i++) {
    solutions += SYNC(nqueens_next);
  }
#endif

  return solutions;
}


#ifdef WOOL
TASK_IMPL_4(int, nqueens_next, int, n, int, j, char*, a, int, i) {
  char *b = (char *) alloca((j + 1) * sizeof(char));

  memcpy(b, a, j * sizeof(char));
  b[j] = i;

  if(ok(j + 1, b)) {
    return CALL(nqueens, n, j + 1, b);
  } else {
    return 0;
  }
}
#endif

int main(int argc, char **argv) {
#ifdef WOOL
  argc = wool_init(argc, argv);
#endif

  if(argc < 2) {
#ifdef WOOL
    fprintf(stderr, "Usage: nqueens <woolopt> <n>\n");
#else
    fprintf(stderr, "Usage: nqueens <n>\n");
#endif
    return 1;
  }

  int   n = atoi(argv[1]);
  char *a = (char *) malloc(n * sizeof(char));

#ifdef WOOL
  printf("nqueens(%dx%d) = %d\n", n, n, CALL(nqueens, n, 0, a));
  wool_fini();
#else
  printf("nqueens(%dx%d) = %d\n", n, n, nqueens(n, 0, a));
#endif

  return 0;
}
