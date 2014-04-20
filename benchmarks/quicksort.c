#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>

#define LENGTH 15000000

void quicksort(int *array, int length) {
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

  quicksort(array, right + 1);
  quicksort(array + left, length - left);
}
 
int main() {
  int* data = (int *) malloc(LENGTH * sizeof(int));

  for(int i = 0; i < LENGTH; i++) {
    data[i] = rand();
  }

  quicksort(data, LENGTH);

  printf("[ %d, %d, %d, %d, %d ... %d, %d, %d, %d, %d ] (len=%d)\n",
         data[0], data[1], data[2], data[3], data[4],
         data[LENGTH-5], data[LENGTH-4], data[LENGTH-3], data[LENGTH-2], data[LENGTH-1],
         LENGTH);

  return 0;
}
