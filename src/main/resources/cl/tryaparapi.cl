//   a, b input, sum output
__kernel void sum(__global const float *a, __global const float *b,__global float *sum) {
    size_t gid = get_global_id(0);
    sum[gid] = a[gid] + b[gid];
}
