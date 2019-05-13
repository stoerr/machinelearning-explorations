package net.stoerr.learning.gpunn.java.aparapi;

import com.amd.aparapi.Kernel;
import com.amd.aparapi.ProfileInfo;
import com.amd.aparapi.Range;
import com.amd.aparapi.device.OpenCLDevice;
import com.amd.aparapi.internal.opencl.OpenCLPlatform;

import java.util.List;

/*
Run with -Djava.library.path=lib -Dcom.amd.aparapi.enableProfiling=true

Home:
===========localTest===========
JTP	Duration: 1.6609131 s /	1651	conv 0	JTP	99594.53
JTP	Duration: 1.5943855 s /	1595	conv 0	JTP	99594.53
GPU	Duration: 1.2865618 s /	1287	conv 473	AMD Accelerated Parallel Processing|GPU	99594.53
GPU	Duration: 0.7956818 s /	796	conv 473	AMD Accelerated Parallel Processing|GPU	99594.53
CPU	Duration: 0.7965397 s /	797	conv 473	AMD Accelerated Parallel Processing|CPU	99594.53
CPU	Duration: 0.796763 s /	797	conv 473	AMD Accelerated Parallel Processing|CPU	99594.53
===========Sum===========
JTP	Duration: 2.2842138 s /	2284	conv 0	JTP	5242828.5
JTP	Duration: 2.0898325 s /	2090	conv 0	JTP	5242828.5
GPU	Duration: 0.24368921 s /	244	conv 101	AMD Accelerated Parallel Processing|GPU	5242828.5
GPU	Duration: 0.14074527 s /	141	conv 101	AMD Accelerated Parallel Processing|GPU	5242828.5
CPU	Duration: 0.14161092 s /	142	conv 101	AMD Accelerated Parallel Processing|CPU	5242828.5
CPU	Duration: 0.14095102 s /	141	conv 101	AMD Accelerated Parallel Processing|CPU	5242828.5

Thinkpad
===========Empty===========
JTP	Duration: 0.016863229 s /	13	conv 0	JTP	3.14
JTP	Duration: 0.0010256129 s /	1	conv 0	JTP	3.14
GPU	Duration: 0.26051077 s /	261	conv 260	Intel(R) OpenCL|GPU	3.14
GPU	Duration: 2.1296699E-4 s /	0	conv 260	Intel(R) OpenCL|GPU	3.14
GPU	Duration: 2.22131E-4 s /	0	conv 260	NVIDIA CUDA|GPU	3.14
GPU	Duration: 3.03872E-4 s /	1	conv 260	NVIDIA CUDA|GPU	3.14
CPU	Duration: 2.78947E-4 s /	0	conv 260	AMD Accelerated Parallel Processing|CPU	3.14
CPU	Duration: 2.46689E-4 s /	0	conv 260	AMD Accelerated Parallel Processing|CPU	3.14
===========localTest===========
JTP	Duration: 0.83901614 s /	839	conv 0	JTP	99594.53
JTP	Duration: 0.7645075 s /	764	conv 0	JTP	99594.53
GPU	Duration: 0.4021018 s /	402	conv 49	Intel(R) OpenCL|GPU	99594.53
GPU	Duration: 0.24113226 s /	241	conv 49	Intel(R) OpenCL|GPU	99594.53
GPU	Duration: 0.24070928 s /	241	conv 49	NVIDIA CUDA|GPU	99594.53
GPU	Duration: 0.23666254 s /	237	conv 49	NVIDIA CUDA|GPU	99594.53
CPU	Duration: 0.24514529 s /	245	conv 49	AMD Accelerated Parallel Processing|CPU	99594.53
CPU	Duration: 0.23713538 s /	237	conv 49	AMD Accelerated Parallel Processing|CPU	99594.53
===========Sum===========
JTP	Duration: 1.2237009 s /	1223	conv 0	JTP	5242828.5
JTP	Duration: 1.1594028 s /	1160	conv 0	JTP	5242828.5
GPU	Duration: 0.40725005 s /	407	conv 68	Intel(R) OpenCL|GPU	5242828.5
GPU	Duration: 0.29914904 s /	299	conv 68	Intel(R) OpenCL|GPU	5242828.5
GPU	Duration: 0.305659 s /	306	conv 68	NVIDIA CUDA|GPU	5242828.5
GPU	Duration: 0.29692075 s /	297	conv 68	NVIDIA CUDA|GPU	5242828.5
CPU	Duration: 0.2930155 s /	293	conv 68	AMD Accelerated Parallel Processing|CPU	5242828.5
CPU	Duration: 0.2946489 s /	295	conv 68	AMD Accelerated Parallel Processing|CPU	5242828.5

 */
public class AparapiSpeedCheck {

    public static void main(String[] _args) throws Exception {
        emptyTest();
        localTest();
        sumTest();
    }

    private static void localTest() throws Exception {
        int size = 1024;
        int rounds = 4000000;
        final float[] res = new float[size];

        Kernel kernel = new Kernel() {
            @Override
            public void run() {
                int gid = getGlobalId();
                float sum = 0;
                float add = 1.0f;
                for (int i = 0; i < rounds; ++i) {
                    add = add * 0.99999f;
                    sum = sum + add;
                }
                res[gid] = sum;
            }

            @Override
            public String toString() {
                return "" + res[0];
            }
        };

        multipleExecution("localTest", kernel, size);
    }

    private static void sumTest() throws Exception {
        final int size = 1024 * 100;

        final float[] a = new float[size];
        final float[] b = new float[size];

        for (int i = 0; i < size; i++) {
            a[i] = i * 0.001f;
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

            @Override
            public String toString() {
                return "" + b[0];
            }
        };

        multipleExecution("Sum", kernel, size);
    }

    private static void emptyTest() throws Exception {
        int size = 128;
        final float[] a = new float[size];

        Kernel kernel = new Kernel() {
            @Override
            public void run() {
                int gid = getGlobalId();
                a[gid] = 3.14f;
            }

            @Override
            public String toString() {
                return "" + a[0];
            }
        };

        multipleExecution("Empty", kernel, size);
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
                "\tconv " + kernel.getConversionTime() + "\t" + deviceDescr + "\t" + kernel.toString());
        List<ProfileInfo> profileInfos = kernel.getProfileInfo();
        if (null != profileInfos) for (ProfileInfo info : profileInfos) {
            System.out.println(info.toString());
        }
        System.out.flush();
    }

}
