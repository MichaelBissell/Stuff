# Clear out everything in memory for a nice clean run and easier debugging
rm(list=ls())

# Load necessary libraries
library(RCUDA)
library(mvtnorm)
library(truncnorm)

#######################
# Function Definitions
#######################

"compute_grid" <- function(N,sqrt_threads_per_block=16L,grid_nd=1){
  # if...
  # N = 1,000,000
  # => 1954 blocks of 512 threads will suffice
  # => (62 x 32) grid, (512 x 1 x 1) blocks
  # Fix block dims:
  block_dims <- c(as.integer(sqrt_threads_per_block), as.integer(sqrt_threads_per_block), 1L)
  threads_per_block <- prod(block_dims)
  if (grid_nd==1){
    grid_d1 <- as.integer(max(1L,ceiling(N/threads_per_block)))
    grid_d2 <- 1L
  } else {
    grid_d1 <- as.integer(max(1L, floor(sqrt(N/threads_per_block))))
    grid_d2 <- as.integer(ceiling(N/(grid_d1*threads_per_block)))
  }
  grid_dims <- c(grid_d1, grid_d2, 1L)
  return(list("grid_dims"=grid_dims,"block_dims"=block_dims))
} # end compute grid

"probit_mcmc_gpu" = function(y, X, beta_0, Sigma_0_inv, niter, burnin, n, p){
  
  z = rep(0,n)
  beta_mat = matrix(0, nrow=(burnin+niter), ncol=p)
  beta_t = beta_0 
  lo = ifelse(y>0,0,-Inf)
  hi = ifelse(y>0,Inf,0)
  sigma = matrix(1,nrow=n, ncol=1)  
  mu_len = n
  sigma_len = n
  lo_len = n
  hi_len = n
  rng_seed_a = 1234L
  rng_seed_b = 1423L
  rng_seed_c = 1842L
  maxtries = 2000L
  
  # Setup static pieces of posterior mean and variance that do not need to be inside the loop
  #     Beta^(t+1)|X,Y,Z^(t) 
  #         ~ MVN(  (Sigma_0_inv+X'X)^(-1)*(Sigma_0_inv*beta_0+X'Z^(t))  ,   Sigma_0_inv+X'X )
  SXX_inv = solve(Sigma_0_inv + t(X)%*%X)     # Static so compute here
  SXX = Sigma_0_inv + t(X)%*%X                # Static so compute here
  Sb0 = Sigma_0_inv %*% beta_0                # Static so compute here
  
  # Setup grid
  grid = compute_grid(n)
  grid_dims = grid$grid_dims
  block_dims = grid$block_dims
  
  cat("Grid size:\n")
  print(grid_dims)
  cat("Block size:\n")
  print(block_dims)
  
  nthreads <- prod(grid_dims)*prod(block_dims)
  cat("Total number of threads to launch = ",nthreads,"\n \n")
  if (nthreads < n){
    stop("Grid is not large enough...!")
  }
  
  cat("Copying to device...\n")
  z_dev = copyToDevice(z)
  sigma_dev = copyToDevice(sigma)
  hi_dev = copyToDevice(hi)
  lo_dev = copyToDevice(lo)
  cat("done. Copying to device...\n \n")
  
  cat("Main loop of Gibbs Sampler on GPU...\n")
  for (i in 1:(burnin+niter)){
    
    if (i %% 500 == 0){
      cat("          GPU iteration =", i, "\n")
    }
    
    # Get Z^(t)
    #     Z^(t)|X,Y,Beta^(t) ~ TN(X*Beta^(t), I; [0,Inf)) if y=1
    #                        ~ TN(X*Beta^(t), I; (-Inf,0]) if y=0
    mu = X%*%beta_t  
    mu_dev = copyToDevice(mu)
    
    # Get Z^(t) from the GPU
    .cuda(k, z_dev, n, mu_dev, sigma_dev, lo_dev, hi_dev, mu_len, sigma_len, 
          lo_len, hi_len, rng_seed_a, rng_seed_b, rng_seed_c, maxtries, 
          gridDim = grid_dims, blockDim = block_dims)
    
    z = copyFromDevice(obj=z_dev,nels=z_dev@nels,type="float")      
    
    # Get Beta^(t+1)
    #     Beta^(t+1)|X,Y,Z^(t) 
    #         ~ MVN(  (Sigma_0_inv+X'X)^(-1)*(Sigma_0_inv*beta_0+X'Z^(t))  ,   Sigma_0_inv+X'X )
    
    # Calc the dynamic piece of the posterior mean which depends on Z^(t)
    mu = SXX_inv%*%(Sb0+t(X)%*%as.matrix(z))
    
    # sample from MVN to get Beta^(t+1)    
    beta_t = t(rmvnorm(1, mu, SXX_inv))
    beta_mat[i,] = beta_t
  } # end burnin+niter for loop
  cat("done. Main loop of Gibbs Sampler on GPU...\n")
  
  return(beta_mat)
  
} # end probit_mcmc_gpu

"probit_mcmc_cpu" = function(y, X, beta_0, Sigma_0_inv, niter, burnin, n, p){
  
  beta_mat = matrix(0,nrow=(burnin+niter), ncol = p)
  beta_t = beta_0 
  
  # Setup static pieces of posterior mean and variance that do not need to be inside the loop
  #     Beta^(t+1)|X,Y,Z^(t) 
  #         ~ MVN(  (Sigma_0_inv+X'X)^(-1)*(Sigma_0_inv*beta_0+X'Z^(t))  ,   Sigma_0_inv+X'X )
  SXX_inv = solve(Sigma_0_inv + t(X)%*%X)     # Static so compute here
  SXX = Sigma_0_inv + t(X)%*%X                # Static so compute here
  Sb0 = Sigma_0_inv %*% beta_0                # Static so compute here
  
  cat("Main loop of Gibbs Sampler on CPU...\n")
  for (i in 1:(burnin+niter)){
    
    if (i %% 500 == 0){
      cat("          CPU iteration =", i, "\n")
    }
    
    # Get Z^(t)
    #     Z^(t)|X,Y,Beta^(t) ~ TN(X*Beta^(t), I; [0,Inf)) if y=1
    #                        ~ TN(X*Beta^(t), I; (-Inf,0]) if y=0
    # Get Z^(t) from the CPU using rtruncnorm
    z = ifelse(y>0, rtruncnorm(1,0,Inf,X%*%beta_t,1), rtruncnorm(1,-Inf,0,X%*%beta_t,1))
    
    # Get Beta^(t+1)
    #     Beta^(t+1)|X,Y,Z^(t) 
    #         ~ MVN(  (Sigma_0_inv+X'X)^(-1)*(Sigma_0_inv*beta_0+X'Z^(t))  ,   Sigma_0_inv+X'X )
    
    # Calc the dynamic piece of the posterior mean which depends on Z^(t)
    mu = SXX_inv%*%(Sb0+t(X)%*%as.matrix(z))
    
    # sample from MVN to get Beta^(t+1) 
    beta_t = t(rmvnorm(1, mu, SXX_inv))
    beta_mat[i,] = beta_t

  } # end burnin+niter for loop
  cat("done. Main loop of Gibbs Sampler on CPU...\n")
  
  return(beta_mat[(burnin+1):(burnin+niter),])
} # end probit_mcmc_cpu


#######################
# Read in Data and Setup inputs
#######################
extension = "05"
burnin = 500
niter = 2000

pars = read.table(paste0("pars_", extension, ".txt"), header=T, quote="\"")
names(pars) = "params"
pars = round(pars,5)
dat = read.table(paste0("data_", extension, ".txt"), header=T, quote="\"")
y = as.matrix(dat[,1])
X = as.matrix(dat[,-1])

n = dim(X)[1]
p = dim(X)[2]

# Setup priors given in problem
beta_0 = matrix(0, nrow=p, ncol=1)
Sigma_0_inv = matrix(0, nrow=p, ncol=p)

# Initialize matrix for beta estimate results 
beta_est_gpu = matrix(0, nrow=(burnin+niter), ncol=p)

# Setup GPU
cat("\nSetting cuGetContext(TRUE)...\n ")
cuGetContext(TRUE)
cat("done. Profiling CUDA code.\n \n")

cat("Loading module...\n")
m = loadModule("rtruncnorm.ptx")
cat("done. Loading module.\n \n")

cat("Extracting kernelkernel...\n")
k = m$rtruncnorm_kernel
cat("done. Extracting kernelkernel.\n \n")


#######################
# Do actual function calls and get results
#######################
gpu_time = system.time({
  beta_est_gpu = probit_mcmc_gpu(y=y, X=X, beta_0=beta_0, Sigma_0_inv=Sigma_0_inv, 
                                   niter=niter, burnin=burnin, n=n, p=p)
})

write.csv(gpu_time, paste0("times_", extension, "-gpu.csv"))
beta_summary = rbind(round(apply(beta_est_gpu, MARGIN=2,FUN=mean), 5),
                     round(t(pars), 5))
write.csv(beta_summary, paste0("beta_summary_", extension, "-gpu.csv"))


# cpu_time = system.time({
#   beta_est_cpu = probit_mcmc_cpu(y=y, X=X, beta_0=beta_0, Sigma_0_inv=Sigma_0_inv, 
#                                  niter=niter, burnin=burnin, n=n, p=p)
# })
# write.csv(cpu_time, paste0("times_", extension, "-cpu.csv"))
# beta_summary = rbind(round(apply(beta_est_cpu, MARGIN=2,FUN=mean), 5),
#                      round(t(pars), 5))
# write.csv(beta_summary, paste0("beta_summary_", extension, "-cpu.csv"))



# times = rbind(gpu_time,cpu_time)
# write.csv(times, paste0("times_", extension, ".csv"))
# 
# beta_summary = rbind(round(apply(beta_est_gpu, MARGIN=2,FUN=mean), 5),
#                      round(apply(beta_est_cpu, MARGIN=2,FUN=mean), 5),
#                      round(t(pars), 5))
# write.csv(beta_summary, paste0("beta_summary_", extension, ".csv"))

q("no")



