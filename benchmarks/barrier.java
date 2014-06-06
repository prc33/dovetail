import java.util.concurrent.CyclicBarrier;

final public class barrier implements Runnable {
  private static CyclicBarrier barrier;

  private int i = 0;

  final public void run() {
    while(i < 1000000) {
      try {
        barrier.await();
      } catch(Exception e) {
        System.err.println("?!?");
        System.exit(-1);
      }

      i++;
    }
  }

  public static void main(String[] args) throws Exception {
    if(args.length < 1) {
      System.err.println("Usage: java locks <n>\n");
      return;
    }

    Thread[] threads = new Thread[Integer.parseInt(args[0])];

    barrier = new CyclicBarrier(threads.length);

    for(int i = 0; i < threads.length; i++) {
      threads[i] = new Thread(new barrier());
    }

    for(int i = 0; i < threads.length; i++) {
      threads[i].start();
    }

    for(int i = 0; i < threads.length; i++) {
      threads[i].join();
    }
  }
}
