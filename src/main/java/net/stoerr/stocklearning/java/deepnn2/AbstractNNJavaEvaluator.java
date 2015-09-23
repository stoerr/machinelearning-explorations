package net.stoerr.stocklearning.java.deepnn2;

import com.amd.aparapi.Kernel;

/**
 * Base class for evaluation of neural networks.
 */
public abstract class AbstractNNJavaEvaluator extends Kernel {

    public float allInputs[][];
    public float allOutputs[][];
    public float w[];
    public float allRes[][];
    public float allMem[][];

}
