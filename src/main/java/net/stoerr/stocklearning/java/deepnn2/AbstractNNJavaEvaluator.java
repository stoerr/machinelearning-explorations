package net.stoerr.stocklearning.java.deepnn2;

/**
 * Base class for evaluation of neural networks.
 */
public abstract class AbstractNNJavaEvaluator extends PseudoKernel {

    public double allInputs[][];
    public double allOutputs[][];
    public double w[];
    public double allRes[][];

    /**
     * Required size of local memory mem[]
     */
    public int memLength;

}
