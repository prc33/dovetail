public class fib {
  public static long fib(int n) {
    if(n < 2) {
	    return n;
    } else {
	    return fib(n - 1) + fib(n - 2);
    }
  }

  public static void main(String[] args) {
    if(args.length < 1) {
      System.err.println("Usage: java fib <n>\n");
      return;
    }

    int n = Integer.parseInt(args[0]);

    System.out.println("fib(" + n + ") = " + fib(n));
  }
}
