import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import java.util.Random;

final public class rwlock implements Runnable {
  final private static ReadWriteLock rwlock = new ReentrantReadWriteLock();

  private int i = 0;
  final private Random rand;

  public rwlock(long seed) {
    rand = new Random(seed);
  }

  final public void run() {
    while(i < 1000000) {
      Lock l = choose();

      l.lock();
      i++;
      l.unlock();
    }
  }

  // Reader with 0.75 probability, writer 0.25
  private Lock choose() {
    return rand.nextInt(4) == 0 ? rwlock.writeLock() : rwlock.readLock();
  }

  public static void main(String[] args) throws Exception {
    if(args.length < 1) {
      System.err.println("Usage: java rwlock <n>\n");
      return;
    }

    Thread[] threads = new Thread[Integer.parseInt(args[0])];

    for(int i = 0; i < threads.length; i++) {
      threads[i] = new Thread(new rwlock(i));
    }

    for(int i = 0; i < threads.length; i++) {
      threads[i].start();
    }

    for(int i = 0; i < threads.length; i++) {
      threads[i].join();
    }
  }
}
