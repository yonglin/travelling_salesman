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

fitness_AIC <- function(string) {
  inc <- which(string == 1)
  X <- cbind(1, x[,inc])
  mod <- lm.fit(X,y)
  class(mod) <- "lm"
  -AIC(mod)
}

fitness_BIC <- function(string) {
  inc <- which(string == 1)
  X <- cbind(1, x[,inc])
  mod <- lm.fit(X,y)
  class(mod) <- "lm"
  -BIC(mod)
}



#GA_Varaibel_Select <- function(criteria, nbr_observations, nbr_variables, real_nbr_variables) {
GA_Varaibel_Select <- function(criteria, data_set) {  
  library(GA)
  #data_set <- Generate_Data(nbr_observations, nbr_variables, real_nbr_variables)
  x <<- data_set$predictor
  y <<- data_set$response
  
  
  if (criteria == 'AIC'){
    GA <- ga("binary", fitness = fitness_AIC, nBits = ncol(x), maxiter=1000, names = colnames(x), monitor = plot)
  }
  else{
    GA <- ga("binary", fitness = fitness_BIC, nBits = ncol(x), maxiter= 1000, names = colnames(x), monitor = plot)
  }
  #plot(GA)
  summary(GA)
}


#mod2 <- lm(body.fat.siri ~ ., data = data.frame(body.fat.siri = y, x[,GA@solution == 1]))
#summary(mod2)
