package net.stoerr.stocklearning.java.deepnn2.aparapi;

import com.amd.aparapi.Kernel;
import com.amd.aparapi.Range;
import com.amd.aparapi.device.Device;
import com.amd.aparapi.device.OpenCLDevice;
import com.amd.aparapi.opencl.OpenCL;
import net.stoerr.stocklearning.java.deepnn2.ExampleNNJavaEvaluator;

/**
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 22.09.2015
 */
public class TryAparapi {

    @OpenCL.Resource("cl/tryaparapi.cl")
    interface Summing extends OpenCL<Summing> {
        public Summing sum(//
                           Range _range,//
                           @GlobalReadOnly("a") float[] a,//
                           @GlobalReadOnly("b") float[] b,//
                           @GlobalWriteOnly("sum") float[] sum);
    }

    public static void main(String[] _args) {

        final int size = 512;

        final float[] a = new float[size];
        final float[] b = new float[size];

        for (int i = 0; i < size; i++) {
            a[i] = (float) (Math.random() * 100);
            b[i] = (float) (Math.random() * 100);
        }

        final float[] sum = new float[size];

        Kernel kernel = new Kernel() {
            @Override
            public void run() {
                int gid = getGlobalId();
                sum[gid] = a[gid] + b[gid];
            }
        };

        kernel.execute(Range.create(size));

        for (int i = 0; i < 10; i++) {
            System.out.printf("%6.2f + %6.2f = %8.2f\n", a[i], b[i], sum[i]);
        }

        System.out.println("Execution mode = " + kernel.getExecutionMode());

        kernel.dispose();

        ExampleNNJavaEvaluator ev = new ExampleNNJavaEvaluator();
        ev.in = new float[100];
        ev.out = new float[100];
        ev.res = new float[100];
        ev.w = new float[100];
        ev.execute(1);
        System.out.println("Execution mode = " + kernel.getExecutionMode());
        ev.dispose();

        final OpenCLDevice device = (OpenCLDevice) Device.firstGPU();
        Summing summing = device.bind(Summing.class);
        {
            final float[] otherSum = new float[size];
            summing.sum(Range.create(size), a, b, otherSum);
            for (int i = 0; i < 10; i++) {
                System.out.printf("%6.2f + %6.2f = %8.2f\n", a[i], b[i], sum[i]);
            }
        }
    }

}
