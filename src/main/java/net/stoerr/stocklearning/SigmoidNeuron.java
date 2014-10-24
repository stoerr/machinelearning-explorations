package net.stoerr.stocklearning;

import java.lang.Math;

/**
 * Simple artificial neuron with sigmoid activation function. Done in low level Java for performance reasons: since this is the innermost loop, we don't want any boxing / unboxing of double.
 */
public class SigmoidNeuron {

    public final int n;

    /** Weights */
    public final double w[];

    public SigmoidNeuron(int n) {
        this.n = n;
        w = new double[n];
    }

    /** tanh from -1 to +1 */
    private static double activation(double x) {
        return Math.tanh(x);
    }

    /** d/dx activation */
    private static double activationDerivation(double x) {
        double th = Math.tanh(x);
        return 1-th*th;
    }

}
