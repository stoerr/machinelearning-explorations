package net.stoerr.stocklearning.java.deepnn2;

/**
 * Created by hps on 21.09.2015.
 */
public class ExampleNNJavaEvaluator extends AbstractNNJavaEvaluator {


    public void run() {
        final int id = getGlobalId();
        final int inOffset = id * inSubSize;
        final int outOffset = id * outSubSize;
        final int memOffset = id * memSubSize;
        final int resOffset = id * resSubSize;

        mem[memOffset + 0] = 1.0f;
        mem[memOffset + 1] = in[inOffset + 0] * w[1] + in[inOffset + 1] * w[2] + mem[memOffset + 0] * w[0];
        mem[memOffset + 2] = in[inOffset + 0] * w[4] + in[inOffset + 1] * w[5] + mem[memOffset + 0] * w[3];
        mem[memOffset + 3] = in[inOffset + 0] * w[7] + in[inOffset + 1] * w[8] + mem[memOffset + 0] * w[6];
        mem[memOffset + 4] = in[inOffset + 0] * w[10] + in[inOffset + 1] * w[11] + mem[memOffset + 0] * w[9];
        mem[memOffset + 5] = in[inOffset + 0] * w[13] + in[inOffset + 1] * w[14] + mem[memOffset + 0] * w[12];
        mem[memOffset + 6] = mem[memOffset + 1] / (1 + abs(mem[memOffset + 1]));
        mem[memOffset + 7] = mem[memOffset + 2] / (1 + abs(mem[memOffset + 2]));
        mem[memOffset + 8] = mem[memOffset + 3] / (1 + abs(mem[memOffset + 3]));
        mem[memOffset + 9] = mem[memOffset + 4] / (1 + abs(mem[memOffset + 4]));
        mem[memOffset + 10] = mem[memOffset + 5] / (1 + abs(mem[memOffset + 5]));
        mem[memOffset + 1] = mem[memOffset + 6] * w[16] + mem[memOffset + 7] * w[17] + mem[memOffset + 8] * w[18] + mem[memOffset + 9] * w[19] + mem[memOffset + 10] * w[20] + mem[memOffset + 0] * w[15];
        mem[memOffset + 2] = mem[memOffset + 6] * w[22] + mem[memOffset + 7] * w[23] + mem[memOffset + 8] * w[24] + mem[memOffset + 9] * w[25] + mem[memOffset + 10] * w[26] + mem[memOffset + 0] * w[21];
        mem[memOffset + 3] = mem[memOffset + 6] * w[28] + mem[memOffset + 7] * w[29] + mem[memOffset + 8] * w[30] + mem[memOffset + 9] * w[31] + mem[memOffset + 10] * w[32] + mem[memOffset + 0] * w[27];
        mem[memOffset + 4] = mem[memOffset + 6] * w[34] + mem[memOffset + 7] * w[35] + mem[memOffset + 8] * w[36] + mem[memOffset + 9] * w[37] + mem[memOffset + 10] * w[38] + mem[memOffset + 0] * w[33];
        mem[memOffset + 5] = mem[memOffset + 6] * w[40] + mem[memOffset + 7] * w[41] + mem[memOffset + 8] * w[42] + mem[memOffset + 9] * w[43] + mem[memOffset + 10] * w[44] + mem[memOffset + 0] * w[39];
        mem[memOffset + 6] = mem[memOffset + 1] / (1 + abs(mem[memOffset + 1]));
        mem[memOffset + 7] = mem[memOffset + 2] / (1 + abs(mem[memOffset + 2]));
        mem[memOffset + 8] = mem[memOffset + 3] / (1 + abs(mem[memOffset + 3]));
        mem[memOffset + 9] = mem[memOffset + 4] / (1 + abs(mem[memOffset + 4]));
        mem[memOffset + 10] = mem[memOffset + 5] / (1 + abs(mem[memOffset + 5]));
        mem[memOffset + 1] = mem[memOffset + 6] * w[46] + mem[memOffset + 7] * w[47] + mem[memOffset + 8] * w[48] + mem[memOffset + 9] * w[49] + mem[memOffset + 10] * w[50] + mem[memOffset + 0] * w[45];
        mem[memOffset + 2] = -1.0f;
        mem[memOffset + 0] = mem[memOffset + 2] * out[outOffset + 0];
        mem[memOffset + 6] = mem[memOffset + 1] / (1 + abs(mem[memOffset + 1]));
        mem[memOffset + 1] = mem[memOffset + 0] + mem[memOffset + 6];
        res[resOffset + 0] = mem[memOffset + 1] * mem[memOffset + 1];
    }
}
