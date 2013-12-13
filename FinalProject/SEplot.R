dir = "C:/Users/Michael/Documents/Michael UC Davis/STA 250 Adv Stat Computing/HW2/BLB/"

s = 50
r = 5
gamma = 0.70


g = as.character(gamma*100)
args = paste0("s", s, "_r", r, "_g", g)

infile = paste0("blb_lin_reg_data_", args, "_SE.txt")
head(x)
x = read.table(paste0(dir,infile),header=TRUE)
dim(x)
y = x[,1]

pdf(paste0("index_plot_se_", args, ".pdf"))
plot(1:1000,y, main=expression(paste("Estimated Standard Errors for ", hat(beta))),
     xlab="Index", ylab="Std Err")
dev.off()


outfile=paste0("summary_se_", args, ".csv")
write.table(x=rbind(summary(x), sapply(x, sd)),file=outfile, sep=",", col.names=TRUE, row.names=FALSE)





gamma_final = read.table("summary_gamma.txt", header=FALSE)
gamma_final[2:9,2:10] = round(gamma_final[2:9,2:10], 5)

library(xtable)
print(xtable(gamma_final, digits = 5, ), table.placement="H", include.rownames=FALSE)

s_r_final = read.table("summary_s_r.txt", header=FALSE)
print(xtable(s_r_final, digits = 5, ), table.placement="H", include.rownames=FALSE)


b = c(1000,  1995,	3981,	7943,	15849,	31623,	63096,	125893,	251189)
time = c(4.185, 7.71,	12.79,	25.09,	47.01,	47.09,	176.04,	412.19,	856.89)
logb = c(6.90776,  7.59853,	8.28931,	8.98008,	9.67086,	10.36163,	11.05241,	11.74318,	12.43396)
pdf("runtimes.pdf")
  plot(logb, time, type="l", lwd=2, col="blue", main="Runtime versus log(b)")
  points(x=logb[5], y=time[5], col="red", pch=19, cex=1.5)
dev.off()



s = 5
r = 50

gam_seq = seq(from=0.50, to=0.90, by=0.05)
for (i in 1:length(gam_seq)){

  g = as.character(gam_seq[i]*100)
  args = paste0("s", s, "_r", r, "_g", g)
  
  infile = paste0("blb_lin_reg_data_", args, "_SE.txt")
  head(x)
  x = read.table(paste0(dir,infile),header=TRUE)
  dim(x)
  y = x[,1]
  lo = quantile(y, 0.05)
  hi = quantile(y, 0.95)
  
  pdf(paste0("index_plot_se_", args, ".pdf"))
  plot(1:1000,y, main=paste("Estimated Standard Errors for Gamma=", gam_seq[i]),
       xlab="Index", ylab="Std Err", ylim=c(0.0082, 0.01175))
      abline(h=lo, lty="dashed", lwd=2, col="red")
      abline(h=hi, lty="dashed", lwd=2, col="red")
  dev.off()
}