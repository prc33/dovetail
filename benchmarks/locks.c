
#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <pthread.h>
#include <sched.h>

#define ITERATIONS 1000000

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void work() {
  int i = 0;

  while(i < ITERATIONS) {
    pthread_mutex_lock(&mutex);
    i++;
    pthread_mutex_unlock(&mutex);
  }
}


int main(int argc, char* argv[]) {
#ifdef WOOL
  fprintf(stderr, "ERROR: locks benchmark is not appropriate to wool.\n");
  return 1;
#else
  if(argc < 2) {
    fprintf(stderr, "Usage: locks <n>\n");
    return 1;
  }

  int n = atoi(argv[1]);
  
  // Allow threads system (rather than process) scope
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM);

  // Create space for threads
  pthread_t *threads = (pthread_t *) malloc(n * sizeof(pthread_t));

  // Start workers 1 to (N-1)
  pthread_mutex_lock(&mutex);
  for(int i = 1; i < n; i++) {
    pthread_create(&threads[i], &attr, work, 0);
  }
  pthread_mutex_unlock(&mutex);

  work();

  // Join on other threads.
  for(int i = 1; i < n; i++) {
    pthread_join(threads[i], NULL);
  }

  return 0;
#endif
}
