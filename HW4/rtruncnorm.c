#include <stdio.h>
#include <stdlib.h>

#include <cuda.h>
#include <curand_kernel.h>
#include <math_constants.h>

extern "C"
{

__global__ void 
rtruncnorm_kernel(float *vals, int n, 
                  float *mu, float *sigma, 
                  float *lo, float *hi,
                  int mu_len, int sigma_len,
                  int lo_len, int hi_len,
		  int rng_seed_a, int rng_seed_b, int rng_seed_c,
                  int maxtries)
{
    bool accepted = false;
    int numtries = 0;
    float x;
    float u;
    float alpha;
    float psi;
    float z;
    float mu_minus;
    bool left_trunc = false;

    // Figure out which thread and block you are in and map these to a single index, "idx"
    // Usual block/thread indexing...
    int myblock = blockIdx.x + blockIdx.y * gridDim.x;
    int blocksize = blockDim.x * blockDim.y * blockDim.z;
    int subthread = threadIdx.z*(blockDim.x * blockDim.y) + threadIdx.y*blockDim.x + threadIdx.x;
    int idx = myblock * blocksize + subthread;

    // Check: if index idx < n generate a sample, else in unneeded thread 
    if(idx<n){

          

	    // Setup the RNG:
	    curandState rng;
	    curand_init(rng_seed_a + idx*rng_seed_b, rng_seed_c, 0, &rng);

	    // Sample the truncated normal
	    // i.e. pick off mu and sigma corresponding to idx and generate a random sample, x
	    // if that random sample, x, is in the truncation region, update the return value to x, i.e. vals[idx]=x
	    // if x is not in the trunc region, try again until you get a sample in the trunc region or if more than maxtries,
	    // move on to Robert's approx method




	    while(!accpeted && numtries < maxtries){
		x = mu[idx] + sigma[idx]*curand_normal(&rng);
		if(x >= lo[idx] && x <= hi[idx]){
			accepted = true;
			vals[idx] = x;
		}
		numtries = numtries + 1;
	    }

	    // Robert's approx method
            // We don't want to write both truncation algos for left and right tail truncations, just use right tail trancation.
            // If we want to sample from Y~N(mu, sigma, -Inf, b), we transform first X~N(mu, sigma, -b+2*mu, Inf), use only right truncation,
            // sample from the right tail to get a X, then transform back Y=2*mu-X to get left truncation sample if needed in Robert.  

	    if(lo[idx] < mu[idx]) {			// then left truncation
		left_trunc = true;
		a = -1*hi[idx] + 2*mu[idx];		// flip up to right tail  
	    }
	    else {
		a = lo[idx];				// right truncation from a=lo[idx] to infinity
	    }


	    mu_minus = (a-mu[idx])/sigma[idx];
	    while(!accepted){
		// need to find mu_minus but that depends on if lower trunc or upper trunc
		alpha = (mu_minus + sqrtf(mu_minus*mu_minus + 4))/2;

		// Need random expon for Robert no curand_expon function so do inverse CDF
		// F(x) = 1-exp(-alpha*x) --> F^1(x) = -log(U)/alpha where U~Unif[0,1]
		u = curand_uniform(&rng);
		x = -1 * log(u)/alpha;  // x is not random expon by inverse CDF 
		z = mu_minus + x;

		// Compute Psi = blah
	        if(mu_minus < alpha){
        	        psi = expf( -1/2*(alpha-z)*(alpha-z)); 
		}
        	else {
                	psi = expf(  -1/2*( (mu_minus-alpha)*(mu_minus-alpha) + (alpha-z)*(alpha-z) ) ); 
		}
	
		// Check if Random Unif[0,1] < Psi, if so accept, else reject and try again
		u = curand_uniform(&rng);
	
		if (u < psi){
			accepted = true;
			if (left_trunc){  // undo transform
				vals[idx] = mu[idx] - sigma[idx]*z;
			}
			else {   // right truncation originally so we're done
				vals[idx] = mu[idx] + sigma[idx]*z;
			}
		}

	    }

    }
    return;
}

} // END extern "C"

