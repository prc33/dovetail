#ifdef WOOL
#include "wool.h"
#endif

#include <stdio.h>
#include <stdlib.h>

#ifdef WOOL

TASK_1(long, fib, int, n) {
	if(n < 2) {
		return n;
	} else {
    SPAWN(fib, n - 1);
    int b = CALL(fib, n - 2);
    int a = SYNC(fib);
    return a + b;
  }
}

#else

long fib(int n) {
	if(n < 2) {
		return n;
	} else {
		return fib(n - 1) + fib(n - 2);
	}
}

#endif

int main(int argc, char **argv) {
#ifdef WOOL
  argc = wool_init(argc, argv);
#endif

  if(argc < 2) {
#ifdef WOOL
    fprintf(stderr, "Usage: fib <woolopt> <n>\n");
#else
    fprintf(stderr, "Usage: fib <n>\n");
#endif
    return 1;
  }

  int n = atoi(argv[1]);

#ifdef WOOL
  printf("fib(%d) = %ld\n", n, CALL(fib, n));
  wool_fini();
#else
  printf("fib(%d) = %ld\n", n, fib(n));
#endif

  return 0;
}
