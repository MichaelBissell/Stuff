dir = "C:/Users/Michael/Documents/Michael UC Davis/STA 250 Adv Stat Computing/HW2/BLB/"

file = "blb_lin_reg_data_s5_r50_SE.txt"


head(x)
x = read.table(paste0(dir,file),header=TRUE)
dim(x)
y = x[,1]

pdf("index_plot_se.pdf")
plot(1:1000,y, main=expression(paste("Estimated Standard Errors for ", hat(beta))),
     xlab="Index", ylab="Std Err")
dev.off()


