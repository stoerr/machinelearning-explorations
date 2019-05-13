package net.stoerr.learning.gpunn.java;

/**
 * Created by hps on 21.09.2015.
 */
public class ExampleNNJavaEvaluator extends AbstractNNJavaEvaluator {

    public void run() {
        final int id = getGlobalId();
        final int inOffset = id*inSubSize;
        final int outOffset = id*outSubSize;
        final int resOffset = id*resSubSize;

    }

}
