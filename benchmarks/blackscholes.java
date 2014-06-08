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

import java.util.concurrent.*;
import java.io.*;

public final class blackscholes {
  public static final int NUM_RUNS = 5000;
  public static final double INV_SQRT_2PI = 0.39894228040143270286;

  // Cumulative Normal Distribution Function (see Hull, Section 11.8, P.243-244)
  private static double cndf(double InputX) {
    boolean sign;

    double xNPrimeofX;
    double xK2;
    double xK2_2, xK2_3;
    double xK2_4, xK2_5;
    double xLocal, xLocal_1;
    double xLocal_2, xLocal_3;

    // Check for negative value of InputX
    if (InputX < 0.0) {
      InputX = -InputX;
      sign = true;
    } else {
      sign = false;
    }

    // Compute NPrimeX term common to both four & six decimal accuracy calcs
    xNPrimeofX = Math.exp(-0.5 * InputX * InputX) * INV_SQRT_2PI;

    xK2 = 1.0 / ((0.2316419 * InputX) + 1.0);
    xK2_2 = xK2 * xK2;
    xK2_3 = xK2_2 * xK2;
    xK2_4 = xK2_3 * xK2;
    xK2_5 = xK2_4 * xK2;
    
    xLocal_1 = xK2 * 0.319381530;
    xLocal_2 = xK2_2 * (-0.356563782);
    xLocal_3 = xK2_3 * 1.781477937f;
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
  private static double BlkSchlsEqEuroNoDiv(double sptprice, double strike, double riskFreeRate, double volatility, double time, int otype) {
    double logValues = Math.log( sptprice / strike );
    
    double xPowerTerm = 0.5 * (volatility * volatility);
    double xDen = volatility * Math.sqrt(time);

    double d1;
    double d2;

    d1 = riskFreeRate + xPowerTerm;
    d1 = d1 * time;
    d1 = d1 + logValues;
    d1 = d1 / xDen;

    d2 = d1 - xDen;
    
    double NofXd1 = cndf( d1 );
    double NofXd2 = cndf( d2 );

    double FutureValueX = strike * ( Math.exp( -(riskFreeRate)*(time) ) );        

    if (otype == 0) {            
      return (sptprice * NofXd1) - (FutureValueX * NofXd2);
    } else { 
      double NegNofXd1 = (1.0 - NofXd1);
      double NegNofXd2 = (1.0 - NofXd2);
      return (FutureValueX * NegNofXd2) - (sptprice * NegNofXd1);
    }
  }

  public static void main(String[] args) throws Exception {
    if(args.length < 2) {
      System.err.println("Usage: java blackscholes <threads> <input_file> [output_file]\n");
      return;
    }

    ExecutorService executor = Executors.newFixedThreadPool(Integer.parseInt(args[0]));
    BufferedReader input = new BufferedReader(new FileReader(args[1]));

    int option_count = Integer.parseInt(input.readLine());

    final double[] prices = new double[option_count];

    for(int i = 0; i < option_count; i++) {
      final String line = input.readLine();
      final int i_copy = i;

      executor.submit(new Runnable() {
        public void run() {
          String[] parts = line.split(" ");

          double  s      = Double.parseDouble(parts[0]);
          double  strike = Double.parseDouble(parts[1]);
          double  r      = Double.parseDouble(parts[2]);
          double  divq   = Double.parseDouble(parts[3]);
          double  v      = Double.parseDouble(parts[4]);
          double  t      = Double.parseDouble(parts[5]);
          String type   = parts[6];
          double  divs   = Double.parseDouble(parts[7]);
          double  refval = Double.parseDouble(parts[8]);
          
          for(int j = 0; j < NUM_RUNS; j++) {
            prices[i_copy] = BlkSchlsEqEuroNoDiv(s, strike, r, v, t, type.equals("P") ? 1 : 0);
            double priceDelta = refval - prices[i_copy];

            if(Math.abs(priceDelta) >= 1e-4){
              System.err.format("Error on %d. Computed=%.5lf, Ref=%.5lf, Delta=%.5lf\n", i_copy, prices[i_copy], refval, priceDelta);
            }
          }
        }
      });
    }

    // Waits for existing submitted tasks to be completed.
    input.close();
    executor.shutdown();
    executor.awaitTermination(1, TimeUnit.DAYS);

    // Write prices to output file
    if(args.length > 2) {
      PrintStream output = new PrintStream(args[2]);

      for(int i = 0; i < option_count; i++) {
        output.format("%.4f\n", prices[i]);
      }

      output.close();
    }
  }
}
