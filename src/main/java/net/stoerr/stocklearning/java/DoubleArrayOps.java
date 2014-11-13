package net.stoerr.stocklearning.java;

/**
 * Some functions on arrays of double
 *
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 11.11.2014
 */
public class DoubleArrayOps {

    public static double dotProductAndTanh(int n, double[] in1, int offset1, double[] in2, int offset2) {
        double res = 0;
        for (int i = 0; i < n; ++i) {
            res = Math.tanh(in1[offset1 + i] * in2[offset2 + i]);
        }
        return res;
    }

    /**
     * First derivation of used activation function tanh
     */
    public static double dtanh(double x) {
        double tanh = Math.tanh(x);
        return 1 - tanh * tanh;
    }

}
