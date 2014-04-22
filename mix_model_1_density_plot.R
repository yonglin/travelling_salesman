## plot and compare the normal distributions

x <- seq(-5, 15, length=500)
norm_density_1 <- dnorm(x,mean=0,sd=1)
norm_density_2 <- dnorm(x,mean=5,sd=2)
norm_density_3 <- dnorm(x,mean=2,sd=2) 

density_list <- cbind(norm_density_1,norm_density_2,norm_density_3)

colors <- c(2, 4, 6)
labels <- c("N(5,2)", "N(0,1)", "N(2,2)")

plot(x, density_list[,1], type="l", xlab="x value",
     ylab="Density", main="Comparison of t Distributions",las=1,col=colors[1])
for (i in 2:ncol(density_list)){
  lines(x, density_list[,i], lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, col=colors)


