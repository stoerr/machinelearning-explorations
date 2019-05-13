package net.stoerr.learning.gpunn.java.aparapi;

import com.amd.aparapi.Kernel;

/*
We try to find out the actual speedup the GPU brings on some easily parallelizable task.
 */
public class ParallelizationSpeedCheck extends Kernel {

    final int length = 1024 * 1024;
    final int rounds = 50;
    int size;
    final float[] a = new float[length];
    final float[] b = new float[length];

    private void init() {
        for (int i = 0; i < length; ++i) {
            a[i] = 1.0f;
            b[i] = 0.0f;
        }
    }

    public static void main(String[] _args) throws Exception {
        new ParallelizationSpeedCheck().test();
    }

    @Override
    public void run() {
        int gid = getGlobalId();
        int low = round(floor((length * 1.0f * gid) / size));
        int high = round(floor((length * 1.0f * (gid + 1)) / size));
        for (int i = low; i < high; ++i) {
            a[i] = a[i] + 1;
            for (int j = 0; j < rounds; ++j) {
                a[i] = a[i] + 1.0f;
                b[i] = b[i] + a[i];
            }
        }
    }

    @Override
    public String toString() {
        float max = -1e9f;
        float min = 1e9f;
        for (int i = 0; i < b.length; ++i) {
            max = Math.max(max, b[i]);
            min = Math.min(min, b[i]);
        }
        return " " + min + " " + max;
    }

    public void test() {

        size = 64;
        init();
        execute(size);

        do {
            init();
            long begin = System.nanoTime();
            execute(size);
            float duration = (System.nanoTime() - begin) * 1e-9f;
            System.out.println(size + "\t" + duration + "\t" + this);
            size = size * 2;
        } while (size <= length);

    }

}
