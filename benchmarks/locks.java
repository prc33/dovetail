final public class locks implements Runnable {
  private int i = 0;

  final public void run() {
    while(i < 1000000) {
      synchronized(locks.class) {
        i++;
      }
    }
  }

  public static void main(String[] args) throws Exception {
    if(args.length < 1) {
      System.err.println("Usage: java locks <n>\n");
      return;
    }

    Thread[] threads = new Thread[Integer.parseInt(args[0])];

    for(int i = 0; i < threads.length; i++) {
      threads[i] = new Thread(new locks());
    }

    for(int i = 0; i < threads.length; i++) {
      threads[i].start();
    }

    for(int i = 0; i < threads.length; i++) {
      threads[i].join();
    }
  }
}
