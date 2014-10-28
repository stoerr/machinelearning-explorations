package net.stoerr.stocklearning;

import java.io.*;
import java.util.Arrays;

/**
 * <p>A simple two layer neural network with SigmoidNeuron and back propagation learning algorithm.</p>
 * <p>The network must be used in a two step process. First one calls #output to get the output, then one calls #adapt
 * to learn. </p>
 */
public class BackpropagatedNeuralNetwork implements Serializable {

    public final int inputSize;

    public final SigmoidNeuron firstLayer[];
    public final SigmoidNeuron lastLayer[];

    // kept to avoid garbage collection; not transient since it probably wouldn't get initialized otherwise
    private final double firstLayerResults[];

    public BackpropagatedNeuralNetwork(int inputSize, int firstLayerSize, int secondLayerSize) {
        this.inputSize = inputSize;
        firstLayer = new SigmoidNeuron[firstLayerSize];
        for (int i = 0; i < firstLayer.length; ++i) firstLayer[i] = new SigmoidNeuron(inputSize);
        lastLayer = new SigmoidNeuron[secondLayerSize];
        for (int i = 0; i < lastLayer.length; ++i) {
            SigmoidNeuron n = new SigmoidNeuron(firstLayerSize);
            lastLayer[i] = n;
            for (int j = 0; j < firstLayer.length; ++j) n.inputNeurons[j] = firstLayer[j];
        }
        firstLayerResults = new double[firstLayerSize];
    }

    /**
     * Calculates the activation for input values i and saves intermediate values for following call to #adapt .
     */
    public void calculate(double[] inputs) {
        for (int i = 0; i < firstLayer.length; ++i) firstLayerResults[i] = firstLayer[i].output(inputs);
        double[] res = new double[lastLayer.length];
        for (int i = 0; i < lastLayer.length; ++i) lastLayer[i].output(firstLayerResults);
    }

    /**
     * Adapts the weigths with backpropagation algorithm by strength reinforcement for inputs in last step
     */
    public void adapt(double reinforcement) {
        for (int i = 0; i < lastLayer.length; ++i) lastLayer[i].adapt(reinforcement);
    }

    /** Copies the whole network using serialization. */
    public BackpropagatedNeuralNetwork deepCopy() {
        try {
            ByteArrayOutputStream bout = new ByteArrayOutputStream();
            ObjectOutputStream oout = new ObjectOutputStream(bout);
            oout.writeObject(this);
            oout.close();
            ObjectInputStream oin = new ObjectInputStream(new ByteArrayInputStream(bout.toByteArray()));
            return (BackpropagatedNeuralNetwork) oin.readObject();
        } catch (IOException | ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public String toString() {
        return "BackpropagatedNeuralNetwork{" + "inputSize=" + inputSize +
                ", firstLayer=\n" + Arrays.toString(firstLayer) +
                ", lastLayer=\n" + Arrays.toString(lastLayer) +
                '}';
    }
}
