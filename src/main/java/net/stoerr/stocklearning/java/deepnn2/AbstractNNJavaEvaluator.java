package net.stoerr.stocklearning.java.deepnn2;

import com.amd.aparapi.Kernel;
import com.amd.aparapi.Range;

/**
 * Base class for evaluation of neural networks.
 */
public abstract class AbstractNNJavaEvaluator extends Kernel {

    @OpenCLMapping(mapTo = "tanh")
    protected float tanh(float _f) {
        float ex = exp(2 * _f);
        return (ex - 1) / (ex + 1);
    }

    public int inSubSize;
    @Constant
    public float in[];
    public int outSubSize;
    @Constant
    public float out[];
    @Constant
    public float w[];
    public int resSubSize;
    public float res[];

    /**
     * Sanity check of array sizes. Call before execute with the same argument.
     */
    public void sanityCheck(int range) {
        if (in.length != inSubSize * range) throw new IllegalStateException("in length broken.");
        if (out.length != outSubSize * range) throw new IllegalStateException("out length broken.");
        if (res.length != resSubSize * range) throw new IllegalStateException("res length broken.");
    }

    @Override
    public synchronized Kernel execute(String _entrypoint, Range _range, int _passes) {
        setExplicit(true);
        long begin = System.nanoTime();
        this.put(in).put(w).put(out);
        super.execute(_entrypoint, _range, _passes);
        get(res);
        float time = 1e-9f * (System.nanoTime() - begin);
        System.out.println("Evaluator " + in.length + " in, " + out.length + " out, " + w.length + " w, " + res.length + " res, " + time + " s");
        return this;
    }

}
