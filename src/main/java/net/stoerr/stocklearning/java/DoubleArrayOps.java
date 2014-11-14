package net.stoerr.stocklearning.java;

/**
 * Some functions on arrays of double
 *
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 11.11.2014
 */
public class DoubleArrayOps {

    public static double dotProductAndTanh(int n, double[] inputs, int offset1, double[] weights, int offset2) {
        double res = 0;
        for (int i = 0; i < n; ++i) {
            res += inputs[offset1 + i] * weights[offset2 + i];
        }
        return Math.tanh(res + weights[offset2 + n]);
    }

    /**
     * First derivation of used activation function tanh
     */
    public static double dtanh(double x) {
        double tanh = Math.tanh(x);
        return 1 - tanh * tanh;
    }

}
