
#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <pthread.h>
#include <sched.h>

#define ITERATIONS 1000000

pthread_barrier_t barrier;

void work() {
  for(int i = 0; i < ITERATIONS; i++) {
    int rc = pthread_barrier_wait(&barrier);
    if(rc != 0 && rc != PTHREAD_BARRIER_SERIAL_THREAD) {
        printf("Could not wait on barrier\n");
        exit(-1);
    }
  }
}


int main(int argc, char* argv[]) {
#ifdef WOOL
  fprintf(stderr, "ERROR: barrier benchmark is not appropriate to wool.\n");
  return 1;
#else
  if(argc < 2) {
    fprintf(stderr, "Usage: barrier <n>\n");
    return 1;
  }

  int n = atoi(argv[1]);
  
  // Allow threads system (rather than process) scope
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM);

  // Create space for threads
  pthread_t *threads = (pthread_t *) malloc(n * sizeof(pthread_t));

  // Initialise barrier
  pthread_barrierattr_t b_attr;
  pthread_barrierattr_setpshared(&b_attr, PTHREAD_PROCESS_SHARED);

  if(pthread_barrier_init(&barrier, &b_attr, n)) {
      printf("Could not initialise barrier\n");
      return -1;
  }

  // Start workers 1 to (N-1)
  for(int i = 1; i < n; i++) {
    pthread_create(&threads[i], &attr, work, 0);
  }

  work();

  // Join on other threads.
  for(int i = 1; i < n; i++) {
    pthread_join(threads[i], NULL);
  }

  return 0;
#endif
}
