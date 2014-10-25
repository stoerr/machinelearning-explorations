package net.stoerr.stocklearning;

import java.util.Arrays;

/**
 * <p>A simple two layer neural network with SigmoidNeuron and back propagation learning algorithm.</p>
 * <p>The network must be used in a two step process. First one calls #output to get the output, then one calls #adapt
 * to learn. </p>
 */
public class BackpropagatedNeuralNetwork {

    public final int inputSize;

    public final SigmoidNeuron firstLayer[];
    public final SigmoidNeuron secondLayer[];
    public final double outputs[];

    private final double firstLayerResults[];

    public BackpropagatedNeuralNetwork(int inputSize, int firstLayerSize, int secondLayerSize) {
        this.inputSize = inputSize;
        firstLayer = new SigmoidNeuron[firstLayerSize];
        for (int i = 0; i < firstLayer.length; ++i) firstLayer[i] = new SigmoidNeuron(inputSize);
        secondLayer = new SigmoidNeuron[secondLayerSize];
        for (int i = 0; i < secondLayer.length; ++i) {
            SigmoidNeuron n = new SigmoidNeuron(firstLayerSize);
            secondLayer[i] = n;
            for (int j = 0; j < firstLayer.length; ++j) n.inputNeurons[j] = firstLayer[j];
        }
        firstLayerResults = new double[firstLayerSize];
        outputs = new double[secondLayerSize];
    }

    /**
     * Calculates the activation for input values i and saves intermediate values for following call to #adapt .
     */
    public void calculate(double[] inputs) {
        for (int i = 0; i < firstLayer.length; ++i) firstLayerResults[i] = firstLayer[i].output(inputs);
        double[] res = new double[secondLayer.length];
        for (int i = 0; i < secondLayer.length; ++i) outputs[i] = secondLayer[i].output(firstLayerResults);
    }

    /**
     * Adapts the weigths with backpropagation algorithm by strength reinforcement for inputs in last step
     */
    public void adapt(double reinforcement) {
        for (int i = 0; i < secondLayer.length; ++i) secondLayer[i].adapt(reinforcement);
    }

    @Override
    public String toString() {
        return "BackpropagatedNeuralNetwork{" + "inputSize=" + inputSize +
                ", firstLayer=\n" + Arrays.toString(firstLayer) +
                ", secondLayer=\n" + Arrays.toString(secondLayer) +
                '}';
    }
}
