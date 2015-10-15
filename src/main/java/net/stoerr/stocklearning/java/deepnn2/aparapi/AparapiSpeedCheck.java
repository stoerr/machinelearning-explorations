package net.stoerr.stocklearning.java.deepnn2.aparapi;

import com.amd.aparapi.Kernel;
import com.amd.aparapi.ProfileInfo;
import com.amd.aparapi.Range;
import com.amd.aparapi.device.OpenCLDevice;
import com.amd.aparapi.internal.opencl.OpenCLPlatform;

import java.util.List;

/*
Run with -Djava.library.path=lib -Dcom.amd.aparapi.enableProfiling=true
===========localTest===========
JTP	Duration: 1.9782374 s /	1973	conv 0	JTP
JTP	Duration: 2.0753102 s /	2075	conv 0	JTP
GPU	Duration: 0.9123408 s /	912	conv 169	Intel(R) OpenCL|GPU
GPU	Duration: 0.60201055 s /	602	conv 169	Intel(R) OpenCL|GPU
GPU	Duration: 0.58698887 s /	587	conv 169	NVIDIA CUDA|GPU
GPU	Duration: 0.60282314 s /	603	conv 169	NVIDIA CUDA|GPU
CPU	Duration: 0.58664614 s /	587	conv 169	AMD Accelerated Parallel Processing|CPU
CPU	Duration: 0.60254055 s /	603	conv 169	AMD Accelerated Parallel Processing|CPU
===========Sum===========
JTP	Duration: 1.1917992 s /	1192	conv 0	JTP
JTP	Duration: 1.1325016 s /	1132	conv 0	JTP
GPU	Duration: 0.38252094 s /	383	conv 65	Intel(R) OpenCL|GPU
GPU	Duration: 0.29265815 s /	293	conv 65	Intel(R) OpenCL|GPU
GPU	Duration: 0.29327065 s /	293	conv 65	NVIDIA CUDA|GPU
GPU	Duration: 0.2965773 s /	296	conv 65	NVIDIA CUDA|GPU
CPU	Duration: 0.2911956 s /	291	conv 65	AMD Accelerated Parallel Processing|CPU
CPU	Duration: 0.29992723 s /	300	conv 65	AMD Accelerated Parallel Processing|CPU
 */
public class AparapiSpeedCheck {

    public static void main(String[] _args) throws Exception {
        localTest();
        sumTest();
    }

    private static void localTest() throws Exception {
        int size = 1024;
        int rounds = 10240000;
        final float[] res = new float[size];

        Kernel kernel = new Kernel() {
            @Override
            public void run() {
                int gid = getGlobalId();
                float sum = 0;
                float add = 1.0f;
                for (int i = 0; i < rounds; ++i) {
                    add = add * 1.0001f;
                    sum = sum + add;
                }
                res[gid] = sum;
            }
        };

        multipleExecution("localTest", kernel, size);
    }

    private static void sumTest() throws Exception {
        final int size = 1024 * 100;

        final float[] a = new float[size];
        final float[] b = new float[size];

        for (int i = 0; i < size; i++) {
            a[i] = i * 0.1f;
        }

        Kernel kernel = new Kernel() {
            @Override
            public void run() {
                int gid = getGlobalId();
                float sum = 0;
                for (int i = 0; i < size; ++i) {
                    sum = sum + a[i];
                }
                b[gid] = sum;
            }
        };

        multipleExecution("Sum", kernel, size);
    }

    private static void multipleExecution(String description, Kernel kernel, int size) throws Exception {
        System.out.println("===========" + description + "===========");
        try {
            executeAndPrintStatusOnAllDevices(kernel, size);
            // System.out.println("-----------------");
            // executeAndPrintStatusOnAllDevices(kernel, size);
        } finally {
            kernel.dispose();
        }
    }

    private static void executeAndPrintStatusOnAllDevices(Kernel kernel, int range) throws Exception {
        kernel.setExecutionMode(Kernel.EXECUTION_MODE.JTP);
        executeAndPrintStatus("JTP", kernel, Range.create(range));
        executeAndPrintStatus("JTP", kernel, Range.create(range));
        kernel.setExecutionMode(null);
        for (OpenCLPlatform platform : (new OpenCLPlatform()).getOpenCLPlatforms()) {
            for (OpenCLDevice device : platform.getOpenCLDevices()) {
                String deviceDescr = platform.getName() + "|" + device.getType();
                kernel.setExecutionMode(Kernel.EXECUTION_MODE.valueOf(device.getType().toString()));
                executeAndPrintStatus(deviceDescr, kernel, Range.create(device, range));
                executeAndPrintStatus(deviceDescr, kernel, Range.create(device, range));
            }
        }
    }

    private static void executeAndPrintStatus(String deviceDescr, Kernel kernel, Range range) throws Exception {
        System.out.flush();
        long begin = System.nanoTime();
        kernel.execute(range);
        float duration = (System.nanoTime() - begin) * 1e-9f;
        Kernel.KernelState state = kernel.getKernelState();
        System.out.println(kernel.getExecutionMode() +
                "\tDuration: " + duration + " s /\t" + kernel.getExecutionTime() +
                "\tconv " + kernel.getConversionTime() + "\t" + deviceDescr);
        List<ProfileInfo> profileInfos = kernel.getProfileInfo();
        if (null != profileInfos) for (ProfileInfo info : profileInfos) {
            System.out.println(info.toString());
        }
        System.out.flush();
    }

}
