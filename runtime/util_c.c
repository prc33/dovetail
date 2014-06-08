#define MAX_RAND   2014

static int rand_ranges[MAX_RAND];
static unsigned int rand_seeds[MAX_RAND];
static int rand_length = 0;

int rand_init(int range) {
  int id;

  do {
    id = rand_length;
  } while(!llvm_cas(&rand_length, id, id + 1));

  rand_ranges[id] = range;
  rand_seeds[id] = id;

  return id;
}

int rand_next(int id) {
  return rand_r(&rand_seeds[id]) % rand_ranges[id];
}
