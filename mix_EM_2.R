Generate_Data <- function(nbr_observations, nbr_variables, real_nbr_variables){
  x<-matrix(rnorm(nbr_observations*nbr_variables),ncol=nbr_variables)
  y <- rnorm(nbr_observations,sd=1)
  if (real_nbr_variables <= nbr_variables){
    for (i in 1:real_nbr_variables){
      y <- y + x[,i]
    }
  }
  else{
    break
  }
  data_set <- list('response' = y, 'predictor' = x)
  return(data_set)

}

Generate_Data_new <- function(nbr_observations, nbr_variables, real_nbr_variables){
  x<-matrix(rnorm(nbr_observations*nbr_variables),ncol=nbr_variables)
  y <- rnorm(nbr_observations,sd=1)-x[,5]-x[,10]
  if (real_nbr_variables <= nbr_variables){
    for (i in 1:(real_nbr_variables-2)){
      y <- y + x[,i]
    }
  }
  else{
    break
  }
  data_set <- list('response' = y, 'predictor' = x)
  return(data_set)
  
}


## get the normal mixture EM model
Normal_Mix <- function(mix_vector,k ) {  
  library(mixtools)
  
  output_mixEM <- normalmixEM(mix_vector,k)
  plot(output_mixEM$posterior[,1],output_mixEM$posterior[,2],las=1,xlab='Component 1',ylab='Component 2',col=4)
  #Mu <- output_mixEM$mu
  #Sigma <- output_mixEM$sigma
  #return(cbind(Mu,Sigma))
  #return(Mu)
  return(output_mixEM$posterior)
}
data_set <- Generate_Data_new(1000,50,6)
x <- data_set$predictor
y <- data_set$response

coef_select_EM <- function(x,y){
  library(mixtools)
  mod <- lm.fit(x,y)
  coef_lm <- mod$coefficients
  Normal_Mix(coef_lm,k=2)
}