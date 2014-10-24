package net.stoerr.stocklearning;

/**
 * <p>Simple artificial neuron with sigmoid activation function. Done in low level Java for performance reasons:
 * since this is the innermost loop, we don't want any boxing / unboxing of double.</p>
 * <p>The neuron must be used in a two step process. First one calls #output to get the output, then one calls #adapt
 * to learn. </p>
 * <p>We use tanh as activation function. Thus, its derivation is 1-lastOutput*lastOutput.</p>
 */
public class SigmoidNeuron {

    public final int n;

    /**
     * Weights
     */
    public final double weight[];

    /**
     * Input neurons, if there are any - if it's a input number the value is null.
     */
    public final SigmoidNeuron inputNeurons[];

    private double lastInput[];

    public double lastOutput;

    public SigmoidNeuron(int n) {
        this.n = n;
        weight = new double[n];
        inputNeurons = new SigmoidNeuron[n];
    }

    /**
     * Calculates the activation for input values i and saves intermediate values for following call to #adapt .
     */
    public double output(double input[]) {
        double sum = 0;
        for (int i = 0; i < n; ++i) sum += weight[i] * input[i];
        lastOutput = Math.tanh(sum);
        return lastOutput;
    }

    /**
     * Adapts the weigths with backpropagation algorithm by strength reinforcement for inputs in last step
     */
    public void adapt(double reinforcement) {
        double factor = (1-lastOutput * lastOutput) * reinforcement;
        for (int i = 0; i < n; ++i) {
            weight[i] += factor * lastInput[i];
            SigmoidNeuron inputNeuron = inputNeurons[i];
            if (null != inputNeuron) {
                inputNeuron.adapt(factor * weight[i]);
            }
        }
    }

}
