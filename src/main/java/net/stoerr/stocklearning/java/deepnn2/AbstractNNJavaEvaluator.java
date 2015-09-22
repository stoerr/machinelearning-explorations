package net.stoerr.stocklearning.java.deepnn2;

import com.amd.aparapi.Kernel;

/**
 * Base class for evaluation of neural networks.
 */
public abstract class AbstractNNJavaEvaluator extends Kernel {

    public double allInputs[][];
    public double allOutputs[][];
    public double w[];
    public double allRes[][];

    /**
     * Required size of local memory mem[]
     */
    public int memLength;

}
