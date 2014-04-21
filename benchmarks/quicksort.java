import java.util.Random;

public class quicksort {
  public static void quicksort(int[] array, int start, int end) {
    if(start >= end) {
      return;
    }

    int pivot = array[(start + end) / 2];
    int left  = start;
    int right = end;

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

    quicksort(array, start, right);
    quicksort(array, left,  end);
  }

  public static void main(String[] args) {
    Random rand = new Random(1);

    if(args.length < 1) {
      System.err.println("Usage: java quicksort <n>\n");
      return;
    }

    int   n    = Integer.parseInt(args[0]);
    int[] data = new int[n];
    
    for(int i = 0; i < n; i++) {
      data[i] = rand.nextInt();
    }

    quicksort(data, 0, n - 1);

    System.out.printf("[ %d, %d, %d, %d, %d ... %d, %d, %d, %d, %d ] (len=%d)\n",
                      data[0], data[1], data[2], data[3], data[4],
                      data[n-5], data[n-4], data[n-3], data[n-2], data[n-1],
                      n);
  }
}
