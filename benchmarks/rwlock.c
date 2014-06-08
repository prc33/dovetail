
#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <pthread.h>
#include <sched.h>

#define ITERATIONS 1000000

pthread_rwlock_t rwlock = PTHREAD_RWLOCK_INITIALIZER;

void *work(void *arg) {
  int          i = 0;
  unsigned int seed = (unsigned int) arg;

  while(i < ITERATIONS) {
    if((rand_r(&seed) % 4) == 0) {
      pthread_rwlock_wrlock(&rwlock);
    } else {
      pthread_rwlock_rdlock(&rwlock);
    }
    
    i++;
    pthread_rwlock_unlock(&rwlock);
  }

  return NULL;
}


int main(int argc, char* argv[]) {
#ifdef WOOL
  fprintf(stderr, "ERROR: rwlock benchmark is not appropriate to wool.\n");
  return 1;
#else
  if(argc < 2) {
    fprintf(stderr, "Usage: rwlock <n>\n");
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
  pthread_rwlock_wrlock(&rwlock);
  for(int i = 1; i < n; i++) {
    pthread_create(&threads[i], &attr, work, (void *) ((long) i));
  }
  pthread_rwlock_unlock(&rwlock);

  work((void *) 0);

  // Join on other threads.
  for(int i = 1; i < n; i++) {
    pthread_join(threads[i], NULL);
  }

  return 0;
#endif
}
