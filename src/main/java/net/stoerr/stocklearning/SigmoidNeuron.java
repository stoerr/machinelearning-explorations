package net.stoerr.stocklearning;

import java.io.Serializable;
import java.util.Arrays;

/**
 * <p>Simple artificial neuron with sigmoid activation function. Done in low level Java for performance reasons:
 * since this is the innermost loop, we don't want any boxing / unboxing of double.</p>
 * <p>The neuron must be used in a two step process. First one calls #lastOutput to get the lastOutput, then one calls #adapt
 * to learn. </p>
 * <p>We use tanh as activation function. Thus, its derivation is 1-lastOutput*lastOutput.</p>
 */
public class SigmoidNeuron implements Serializable {

    public final int n;

    public final double weight[];

    /**
     * Input neurons, if there are any - if the neuron is not connected to another neuron there, the value is null.
     */
    public final SigmoidNeuron inputNeurons[];

    /**
     * Additional offset; can be seen as a weight connected to a always 1.0 input
     */
    public double offset;

    /**
     * Always in (-1,1)
     */
    public transient double lastOutput;

    private transient double lastInput[];

    public SigmoidNeuron(int n) {
        this.n = n;
        weight = new double[n];
        for (int i = 0; i < n; ++i) weight[i] = 2 * Math.random() - 1;
        inputNeurons = new SigmoidNeuron[n];
    }

    /**
     * Calculates the activation for input values i and saves intermediate values for following call to #adapt .
     */
    public double output(double input[]) {
        lastInput = input;
        double sum = offset;
        for (int i = 0; i < n; ++i) sum += weight[i] * input[i];
        lastOutput = Math.tanh(sum);
        return lastOutput;
    }

    /**
     * Adapts the weigths with backpropagation algorithm by strength reinforcement for inputs in last step
     */
    public void adapt(double reinforcement) {
        double factor = (1 - lastOutput * lastOutput) * reinforcement;
        offset += factor;
        for (int i = 0; i < n; ++i) {
            weight[i] += factor * lastInput[i];
            SigmoidNeuron inputNeuron = inputNeurons[i];
            if (null != inputNeuron) {
                inputNeuron.adapt(factor * weight[i]);
            }
        }
    }

    @Override
    public String toString() {
        return "SigmoidNeuron{" + " n=" + n + ", weight=" + Arrays.toString(weight) +
                ", offset=" + offset + '}';
    }
}
