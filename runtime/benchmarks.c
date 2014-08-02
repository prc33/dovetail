#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "dovetail-common.h"

FILE *input;

typedef struct {
  uint32_t  length;
  char     *data;
} array_t;

typedef struct {
  uint32_t  length;
  double   *data;
} darray_t;


int blackscholes_count(int dummy) {
  int option_count;

  input = fopen("benchmarks/bs_input", "r");

  if(input == NULL) {
    fprintf(stderr, "Unable to open input file.\n");
    exit(-1);
  }

  if(fscanf(input, "%i", &option_count) != 1) {
    fprintf(stderr, "Unable to read option count from input.\n");
    fclose(input);
    exit(-1);
  }

  return option_count;
}

int blackscholes_read(darray_t s, darray_t strike, darray_t r, darray_t v, darray_t t, array_t type, darray_t ref) {
  for(int i = 0; i < s.length; i++) {
    double dummy1, dummy2;
    char ctype;
    if(fscanf(input, "%lf %lf %lf %lf %lf %lf %c %lf %lf", &s.data[i], &strike.data[i], &r.data[i], &dummy1, &v.data[i], &t.data[i], &ctype, &dummy2, &ref.data[i]) != 9) {
      fprintf(stderr, "Unable to read option data %d from input.\n", i);
      fclose(input);
      exit(-1);
    }

    type.data[i] = (ctype == 'P') ? 1 : 0;
  }

  fclose(input);

  return s.length;
}

char blackscholes_compare(double price, double ref) {
  double priceDelta = price - ref;

  if(fabs(priceDelta) >= 1e-4){
    fprintf(stderr, "Error. Computed=%.5lf, Ref=%.5lf, Delta=%.5lf\n", price, ref, priceDelta);
  }

  return 0;
}
