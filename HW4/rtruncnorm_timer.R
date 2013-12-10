# Clear out everything in memory for a nice clean run and easier debugging
rm(list=ls())

# Load necessary libraries
library(truncnorm)

mu = 2
sd = 1
lo = 0
hi = 1.5
rm(rtruncnorm_time)
t_k = 1:8
for (i in 1:length(t_k)){
  N = as.integer(10^t_k[i])
  rtruncnorm_time = system.time({
    rtnorm = rtruncnorm(N,lo,hi,mu,sd)
  })
  cat(10^i, " ", rtruncnorm_time, "\n")
  #write.csv(rtruncnorm_time, paste0("times_rtruncnorm", i, ".csv"))
  # cat(N, "\n")
}


CPU_time = matrix(c(10, 0, 0, 0, NA, NA, 100, 0, 0, 0, NA, NA, 1000, 0, 0, 0.01, NA, NA, 
                    10000, 0.03, 0, 0.05, NA, NA, 100000,  0.33, 0, 0.42, NA, NA, 1000000, 
                    3.01, 0, 3.12, NA, NA, 10000000,30.6, 0.16, 31.28, NA, NA, 1000000000, 
                    308.2, 1.76, 324.29, NA, NA), nrow=8, ncol=6, byrow=TRUE )
GPU_time = c(0.047,0.05,0.049,0.053,0.083,0.369,3.253,32.446)
n = 10^(1:8)

pdf("plot-1e.pdf")
plot(x=log(n), GPU_time, type="l", col="blue", lwd=2, main="Generation Time by log(n)", ylab="time in sec")
lines(x=log(n), y=CPU_time[,4], type="l", col="green",lwd=2)
legend("topleft", inset=.05, legend=c("GPU", "CPU"), lty=c("solid", "solid"), lwd=c(2,2), col=c("blue","green"))
dev.off()

times = matrix(0, nrow=8, ncol=4)
for (i in 1:8){
  dat = read.csv(paste0("times_", i, ".csv"), header=T, quote="\"")
  times[i,] = cbind(10^i, t(dat[,4]))
}
xtable(times)