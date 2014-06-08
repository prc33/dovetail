/*
 * Black-Scholes
 * Analytical method for calculating European Options
 *
 * Code based on parsec benchmark suite.
 *   Copyright (c) 2007 Intel Corp.
 *
 * Adapted for use as Dovetail benchmark - vastly simplified to ease
 * translation to JCAM.
 *   Copyright (c) 2014 Peter Calvert
 *
 * Reference Source:
 *   "Options, Futures, and Other Derivatives", 3rd Edition,
 *   Prentice Hall, John C. Hull
 */

#ifdef WOOL
#include "wool.h"
#else
#include <pthread.h>
#include <sched.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#define fptype        double

#define NUM_RUNS      5000
#define INV_SQRT_2PI  0.39894228040143270286

typedef struct {
  fptype s;          // spot price
  fptype strike;     // strike price
  fptype r;          // risk-free interest rate
  fptype divq;       // dividend rate
  fptype v;          // volatility
  fptype t;          // time to maturity or option expiration in years 
                     //     (1yr = 1.0, 6mos = 0.5, 3mos = 0.25, ..., etc)  
  char OptionType;   // Option type.  "P"=PUT, "C"=CALL
  fptype divs;       // dividend vals (not used in this test)
  fptype DGrefval;   // DerivaGem Reference Value
} option_t;

int       option_count;
option_t *data;
fptype   *prices;

#ifndef WOOL
int thread_count;
#endif

// Cumulative Normal Distribution Function (see Hull, Section 11.8, P.243-244)
#ifdef WOOL
TASK_1(fptype, cndf, fptype, InputX) {
#else
fptype cndf(fptype InputX) {
#endif
  int sign;

  fptype xNPrimeofX;
  fptype xK2;
  fptype xK2_2, xK2_3;
  fptype xK2_4, xK2_5;
  fptype xLocal, xLocal_1;
  fptype xLocal_2, xLocal_3;

  // Check for negative value of InputX
  if (InputX < 0.0) {
    InputX = -InputX;
    sign = 1;
  } else {
    sign = 0;
  }

  // Compute NPrimeX term common to both four & six decimal accuracy calcs
  xNPrimeofX = exp(-0.5f * InputX * InputX) * INV_SQRT_2PI;

  xK2 = 1.0 / ((0.2316419 * InputX) + 1.0);
  xK2_2 = xK2 * xK2;
  xK2_3 = xK2_2 * xK2;
  xK2_4 = xK2_3 * xK2;
  xK2_5 = xK2_4 * xK2;
  
  xLocal_1 = xK2 * 0.319381530;
  xLocal_2 = xK2_2 * (-0.356563782);
  xLocal_3 = xK2_3 * 1.781477937;
  xLocal_2 = xLocal_2 + xLocal_3;
  xLocal_3 = xK2_4 * (-1.821255978);
  xLocal_2 = xLocal_2 + xLocal_3;
  xLocal_3 = xK2_5 * 1.330274429;
  xLocal_2 = xLocal_2 + xLocal_3;

  xLocal_1 = xLocal_2 + xLocal_1;
  xLocal   = xLocal_1 * xNPrimeofX;

  return sign ? xLocal : (1.0 - xLocal);
} 

// Black Scholes computation
#ifdef WOOL
TASK_6(fptype, BlkSchlsEqEuroNoDiv, fptype, sptprice, fptype, strike, fptype, riskFreeRate, fptype, volatility, fptype, time, int, otype) {
#else
fptype BlkSchlsEqEuroNoDiv(fptype sptprice, fptype strike, fptype riskFreeRate, fptype volatility, fptype time, int otype) {
#endif
  fptype logValues = log( sptprice / strike );
  
  fptype xPowerTerm = 0.5 * (volatility * volatility);
  fptype xDen = volatility * sqrt(time);

  fptype d1;
  fptype d2;

  d1 = riskFreeRate + xPowerTerm;
  d1 = d1 * time;
  d1 = d1 + logValues;
  d1 = d1 / xDen;

  d2 = d1 - xDen;
  
#ifdef WOOL
  SPAWN(cndf, d1);
  fptype NofXd2 = CALL(cndf, d2);
  fptype NofXd1 = SYNC(cndf);
#else
  fptype NofXd1 = cndf( d1 );
  fptype NofXd2 = cndf( d2 );
#endif

  fptype FutureValueX = strike * ( exp( -(riskFreeRate)*(time) ) );        

  if (otype == 0) {            
    return (sptprice * NofXd1) - (FutureValueX * NofXd2);
  } else { 
    fptype NegNofXd1 = (1.0 - NofXd1);
    fptype NegNofXd2 = (1.0 - NofXd2);
    return (FutureValueX * NegNofXd2) - (sptprice * NegNofXd1);
  }
}

#ifdef WOOL
VOID_TASK_1(work, int, i) {
#else
void *work(void *tid_ptr) {
  int tid = *(int *)tid_ptr;
  int start = tid * (option_count / thread_count);
  int end = start + (option_count / thread_count);

  for(int i = start; i < end; i++) {
#endif
    for(int j = 0; j < NUM_RUNS; j++) {
#ifdef WOOL
      prices[i] = CALL(BlkSchlsEqEuroNoDiv, data[i].s, data[i].strike, data[i].r, data[i].v, data[i].t, data[i].OptionType == 'P' ? 1 : 0);
#else
      prices[i] = BlkSchlsEqEuroNoDiv(data[i].s, data[i].strike, data[i].r, data[i].v, data[i].t, data[i].OptionType == 'P' ? 1 : 0);
#endif
      fptype priceDelta = data[i].DGrefval - prices[i];

      if(fabs(priceDelta) >= 1e-4){
        fprintf(stderr, "Error on %d. Computed=%.5lf, Ref=%.5lf, Delta=%.5lf\n", i, prices[i], data[i].DGrefval, priceDelta);
      }
    }
#ifndef WOOL
  }

  return NULL;
#endif
}

#ifdef WOOL
VOID_TASK_1(wrapper, int, foo) {
  for(int i = 0; i < option_count; i++) {
    SPAWN(work, i);
  }

  for(int i = 0; i < option_count; i++) {
    SYNC(work);
  }
}
#endif

int main(int argc, char **argv) {
#ifdef WOOL
  argc = wool_init(argc, argv);
#endif

#ifdef WOOL
  if(argc < 2) {
    fprintf(stderr, "Usage: blackscholes [woolopt] <input_file> [output_file]\n");
#else
  if(argc < 3) {
    fprintf(stderr, "Usage: blackscholes <threads> <input_file> [output_file]\n");
#endif
    return 1;
  }

#ifndef WOOL
  thread_count = atoi(argv[1]);
  argv++;
  argc--;
#endif

  FILE *input = fopen(argv[1], "r");

  if(input == NULL) {
    fprintf(stderr, "Unable to open file `%s'.\n", argv[1]);
    exit(-1);
  }

  if(fscanf(input, "%i", &option_count) != 1) {
    fprintf(stderr, "Unable to read option count from `%s'.\n", argv[1]);
    fclose(input);
    exit(-1);
  }

#ifndef WOOL
  if(thread_count > option_count) {
    printf("WARNING: Not enough work, reducing number of threads to match number of options.\n");
    thread_count = option_count;
  }
#endif

  // Allocate space for the option data
  data = (option_t *) malloc(option_count * sizeof(option_t));
  prices = (fptype *) malloc(option_count * sizeof(fptype));

  for(int i = 0; i < option_count; i++) {
    if(fscanf(input, "%lf %lf %lf %lf %lf %lf %c %lf %lf", &data[i].s, &data[i].strike, &data[i].r, &data[i].divq, &data[i].v, &data[i].t, &data[i].OptionType, &data[i].divs, &data[i].DGrefval) != 9) {
      fprintf(stderr, "Unable to read option data %d from `%s'.\n", i, argv[1]);
      fclose(input);
      exit(-1);
    }
  }

  fclose(input);

  printf("blackscholes(option_count=%d, runs=%d):\n", option_count, NUM_RUNS);

#ifdef WOOL
  CALL(wrapper, 0);
  wool_fini();
#else
  int *tids;
  tids = (int *) malloc(thread_count * sizeof(int));

  // Allow threads system (rather than process) scope
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM);

  // Create space for threads
  pthread_t *threads = (pthread_t *) malloc(thread_count * sizeof(pthread_t));

  // Start workers 1 to (N-1)
  for(int i = 1; i < thread_count; i++) {
    tids[i] = i;
    pthread_create(&threads[i], &attr, &work, &tids[i]);
  }

  tids[0] = 0;
  work(&tids[0]);

  // Join on other threads.
  for(int i = 1; i < thread_count; i++) {
    pthread_join(threads[i], NULL);
  }

  free(tids);
#endif

  // Write prices to output file
  if(argc > 2) {
    FILE *output = fopen(argv[2], "w");

    if(output == NULL) {
      fprintf(stderr, "Unable to open output file `%s'.\n", argv[2]);
      exit(-1);
    }

    for(int i = 0; i < option_count; i++) {
      if(fprintf(output, "%.4lf\n", prices[i]) < 0) {
        fprintf(stderr, "Unable to write price %d to file `%s'.\n", i, argv[2]);
        fclose(output);
        exit(-1);
      }
    }

    fclose(output);
  }

  free(data);
  free(prices);

  return 0;
}

