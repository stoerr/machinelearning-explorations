package net.stoerr.stocklearning.java.deepnn2;

import com.amd.aparapi.Kernel;

/**
 * Base class for evaluation of neural networks.
 */
public abstract class AbstractNNJavaEvaluator extends Kernel {

    @OpenCLMapping(mapTo = "tanh")
    protected float tanh(float _f) {
        return (float) Math.tanh(_f);
    }

    @Constant public int inSubSize;
    public float in[];
    public int outSubSize;
    public float out[];
    @Constant public float w[];
    public int resSubSize;
    public float res[];
    public int memSubSize;
    public float mem[];

    /**
     * Sanity check of array sizes. Call before execute with the same argument.
     */
    public void sanityCheck(int range) {
        if (in.length != inSubSize * range) throw new IllegalStateException("in length broken.");
        if (out.length != outSubSize * range) throw new IllegalStateException("out length broken.");
        if (res.length != resSubSize * range) throw new IllegalStateException("res length broken.");
        if (mem.length != memSubSize * range) throw new IllegalStateException("mem length broken.");
    }

}
