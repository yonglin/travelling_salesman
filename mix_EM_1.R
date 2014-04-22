## get the normal mixture EM model
Normal_Mix <- function(par_1,par_2) {  
  library(mixtools)
  sample_1 <- rnorm(n=par_1[1],mean=par_1[2],sd=par_1[3])
  sample_2 <- rnorm(n=par_2[1],mean=par_2[2],sd=par_2[3])
  mix_vector <- c(sample_1,sample_2)

  output_mixEM <- normalmixEM(mix_vector)
  plot(output_mixEM$posterior[,1],output_mixEM$posterior[,2],las=1,xlab='Component 1',ylab='Component 2',col=4)
  Mu <- output_mixEM$mu
  Sigma <- output_mixEM$sigma
  return(cbind(Mu,Sigma))
  #return(Mu)
}





