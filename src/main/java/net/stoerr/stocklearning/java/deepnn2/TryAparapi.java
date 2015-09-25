package net.stoerr.stocklearning.java.deepnn2;

import com.amd.aparapi.Kernel;
import com.amd.aparapi.Range;

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 22.09.2015
 */
public class TryAparapi {

    public static void main(String[] _args) {

        final int size = 512;

        final float[] a = new float[size];
        final float[] b = new float[size];

        for (int i = 0; i < size; i++) {
            a[i] = (float) (Math.random() * 100);
            b[i] = (float) (Math.random() * 100);
        }

        final float[] sum = new float[size];

        Kernel kernel = new Kernel(){
            @Override public void run() {
                int gid = getGlobalId();
                sum[gid] = a[gid] + b[gid];
            }
        };

        kernel.execute(Range.create(512));

        for (int i = 0; i < size; i++) {
            System.out.printf("%6.2f + %6.2f = %8.2f\n", a[i], b[i], sum[i]);
        }

        System.out.println("Execution mode = " + kernel.getExecutionMode());

        kernel.dispose();

        ExampleNNJavaEvaluator ev = new ExampleNNJavaEvaluator();
        ev.in = new float[100];
        ev.mem = new float[100];
        ev.out = new float[100];
        ev.res = new float[100];
        ev.w = new float[100];
        ev.execute(1);
        System.out.println("Execution mode = "+kernel.getExecutionMode());
        ev.dispose();
    }

}
