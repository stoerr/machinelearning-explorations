package net.stoerr.stocklearning.java.deepnn2;

import java.lang.Math.*;

/**
 * Created by hps on 21.09.2015.
 */
public class ExampleNNJavaEvaluator extends AbstractNNJavaEvaluator {

    @Override
    public void run() {
        int id = getGlobalId();
        double in[] = allInputs[id];
        double out[] = allOutputs[id];
        double res[] = allRes[id];
        double mem[] = allMem[id];

        // now comes the generated code
        mem[0] = in[0] + in[1];
        mem[1] = mem[0] * out[0];
        mem[2] = in[1] * out[0];
        res[0] = mem[1] + in[0] + in[1];
        res[1] = in[0] + mem[2];
    }
}
