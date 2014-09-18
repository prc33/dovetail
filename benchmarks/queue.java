import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

final public class queue implements Runnable {
  final private static BlockingQueue queue = new LinkedBlockingQueue();

  final private boolean taker;

  public queue(boolean taker) {
    this.taker = taker;
  }

  final public void run() {
    try {
      if(!taker) {
        for(int i = 0; i < 1000; i++) {
          queue.put(this);
        }
      } else {
        for(int i = 0; i < 1000; i++) {
          queue.take();
        }
      }
    } catch(InterruptedException e) {
      throw new RuntimeException(":(");
    }
  }

  public static void main(String[] args) throws Exception {
    if(args.length < 1) {
      System.err.println("Usage: java queue <n>\n");
      return;
    }

    Thread[] putters = new Thread[Integer.parseInt(args[0])];
    Thread[] takers = new Thread[putters.length];

    for(int i = 0; i < putters.length; i++) {
      putters[i] = new Thread(new queue(false));
      takers[i] = new Thread(new queue(true));
    }

    for(int i = 0; i < putters.length; i++) {
      putters[i].start();
      takers[i].start();
    }

    for(int i = 0; i < putters.length; i++) {
      putters[i].join();
      takers[i].join();
    }
  }
}
