package net.stoerr.stocklearning.java;

/**
 * Some functions on arrays of double
 *
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 11.11.2014
 */
public final class DoubleArrayOps {

    public static double dotProductAndTanh(int n, double[] inputs, int offset1, double[] weights, int offset2) {
        double res = 0;
        for (int i = 0; i < n; ++i) {
            res += inputs[offset1 + i] * weights[offset2 + i];
        }
        return Math.tanh(res + weights[offset2 + n]);
    }

    public static void assignMultiplied(int n, double src[], int srcOffset, double dst[], int dstOffset, double factor) {
        for (int i = 0; i < n; ++i) {
            dst[dstOffset + i] = factor * src[srcOffset + i];
        }
    }

    public static void addMultiplied(int n, double src[], int srcOffset, double dst[], int dstOffset, double factor) {
        for (int i = 0; i < n; ++i) {
            dst[dstOffset + i] += factor * src[srcOffset + i];
        }
    }

}
