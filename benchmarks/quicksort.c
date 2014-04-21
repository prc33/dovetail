#ifdef WOOL
#include "wool.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>

#ifdef WOOL
VOID_TASK_2(quicksort, int *, array, int, length) {
#else
void quicksort(int *array, int length) {
#endif
  if(length < 2) {
    return;
  }

  int pivot = array[length / 2];
  int left = 0;
  int right = length - 1;

  while(left <= right) {
    int left_data = array[left];

    if(left_data < pivot) {
      left++;
      continue;
    }

    int right_data = array[right];

    if(pivot < right_data) {
      right--;
      continue;
    }

    array[left++] = right_data;
    array[right--] = left_data;
  }

#ifdef WOOL
  SPAWN(quicksort, array, right + 1);
  CALL(quicksort, array + left, length - left);
  SYNC(quicksort);
#else
  quicksort(array, right + 1);
  quicksort(array + left, length - left);
#endif
}

int main(int argc, char **argv) {
#ifdef WOOL
  argc = wool_init(argc, argv);
#endif

  if(argc < 2) {
#ifdef WOOL
    fprintf(stderr, "Usage: quicksort <woolopt> <n>\n");
#else
    fprintf(stderr, "Usage: quicksort <n>\n");
#endif
    return 1;
  }

  int  n    = atoi(argv[1]);
  int *data = (int *) malloc(n * sizeof(int));

  for(int i = 0; i < n; i++) {
    data[i] = rand();
  }

#ifdef WOOL
  CALL(quicksort, data, n);
  wool_fini();
#else
  quicksort(data, n);
#endif

  printf("[ %d, %d, %d, %d, %d ... %d, %d, %d, %d, %d ] (len=%d)\n",
         data[0], data[1], data[2], data[3], data[4],
         data[n-5], data[n-4], data[n-3], data[n-2], data[n-1],
         n);

  return 0;
}
