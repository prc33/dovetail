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

import java.util.Arrays;
import java.util.concurrent.Callable;

/*
 * The position of queens is described throughout by an array a, where a[i]
 * gives the row of the queen in column i.
 */
public class nqueens {
  /*
   * Checks whether the first <n> queen positions in <a> conflict or not. Returns
   * true if the positions are valid.
   */
  private static boolean ok(int n, int[] a) {
    for(int i = 0; i < n; i++) {
      int p = a[i];

      for(int j = i + 1; j < n; j++) {
        int q = a[j];

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
  public static int nqueens(final int n, final int j, final int[] a) {
    // No more queens to place.
    if(n == j) {
      return 1;
    }

    // Try each possible position for queen <j>
    int solutions = 0;
    
    for(int i = 0; i < n; i++) {
      final int position = i;

      Callable<Integer> next = new Callable<Integer>() {
        public Integer call() {
          int[] b = Arrays.copyOf(a, j + 1);

          b[j] = position;

          if(ok(j + 1, b)) {
            return nqueens(n, j + 1, b);
          } else {
            return 0;
          }
        }
      };

      try {
        solutions += next.call();
      } catch(Exception e) {
        System.err.println("Recursive call failed.");
      }
    }

    return solutions;
  }

  public static void main(String[] args) {
    if(args.length < 1) {
      System.err.println("Usage: java nqueens <n>\n");
      return;
    }

    int   n = Integer.parseInt(args[0]);
    int[] a = new int[n];

    System.out.println("nqueens(" + n + "x" + n + ") = " + nqueens(n, 0, a));
  }
}

