data = read.table('hive_results.txt', header=FALSE,sep="\001")

head(data)

mat = data[,2:3]
pdf("within_group_mean_var.pdf")
plot(mat, main="Within Group Mean & Variance Scatter", xlab="Within Group Mean",
     ylab="Within Group Variance")
dev.off()