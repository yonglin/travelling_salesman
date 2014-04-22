library(maps)
library(fossil)
## import the packages of maps and fossil
## world.cities in maps contains the geography data of cities
## earth.dist in fossil can be used for caculating the distance between two places
Generate.Cities <- function(city_name_List) {
  ## city_name_List <- c('Amsterdam','Berlin','Brussels','Copenhagen','Dublin','Helsinki','London','Oslo','Paris','Prague','Stockholm','Warsaw','Vienna')
  ## input the name list of cities
  ## output a matrix of cities which has 3 columns name, lattitude, and longitude
  data(world.cities)
  
  Euro_Cities <- world.cities[is.element(world.cities$name,city_name_List)&world.cities$capital==1,]
  ## select the target cities we need. We have to use a logical expression to filter the cities which have overlapping names, 
  ## we just keep the capital cities
  
  cities <- subset(Euro_Cities,select=c('name','lat','long'))
  ## simplify the structure of the cities
  return(cities)
}

Generate.Dis_Matrx <- function(city_name_List) {
  ## generate the distance matrix for the set of cities
  cities <- Generate.Cities(city_name_List)
  cordinates <- subset(cities, select=c('lat','long'))
  ## substract the cordinates, cause the earth.dist needs only pure cordinates
  Dis_Matrx <- earth.dist(cordinates,dis=FALSE)
  ## dis=FALSE means we need keep a whole symmetric matrix not just a triangle matrix
  return(Dis_Matrx)
}



Path.Length <- function(city_list,Dis_Matrx) {
  ## Path.Length computes the path length of the path connecting
  
  total.distance <- 0
  for (i in 1:(length(city_list)-1)) {
    total.distance <- total.distance + Dis_Matrx[city_list[i],city_list[i+1]]
  }
  total.distance <- total.distance + Dis_Matrx[city_list[length(city_list)],city_list[1]] 
  return(total.distance)
}

Pro_accept <- function(new, old, tempature) {
  ## Metropolis-Hastings ratio
  ## Probability of accepting the current route
  return(min(1, exp((old - new) / tempature)))
}


Random_neighbor_index <- function(numbers_cities) {
  ## 2-neighborhood permutation proposal 
  ## step 1: get the head and the tail of a segment which needs to be reversed
  head <- sample(1:(numbers_cities-2),1)
  if (head==numbers_cities-2){
    ## if head equals numbers_cities-2, then the tail can only choose number_cities
    tail <- numbers_cities
  }
  else{
    tail <- sample((head+2):(numbers_cities),1)
  }
  
  index <- c(head,tail)
  return (index)
}

Generate_neighbor <- function(city_list) {
  ## 2-neighborhood permutation proposal 
  ## step 2: reverse the segment
  neighbor_index <- Random_neighbor_index(length(city_list))
  head <- neighbor_index[1]
  tail <- neighbor_index[2]
  reverse_segment <- rev(city_list[head:tail])
  city_list[head:tail] <- reverse_segment
  return (city_list)
}

Simulated_Anneal <- function(city_name_List,init_temperature,decreasing_rate) {
  ## it seems that the parameters (10000000,0.7) is a comparatively better set of paramters
  dist <- c()
  
  temp <- init_temperature
  a <- decreasing_rate
  
  Dis_Matrx <- Generate.Dis_Matrx(city_name_List)
  initial.config <- sample(1:13)
  cities <- Generate.Cities(city_name_List)
  
  initial.distance <- Path.Length(initial.config, Dis_Matrx)
  dist[1] <- initial.distance
  
  ## the following codes is the kernel of the Stimulated Annealling Algorithm for Travelling Salesman Problem 
  for (j in 1:100) {
    for (i in 1:j) {
      next.config <- Generate_neighbor(initial.config)
      next.distance <- Path.Length(next.config, Dis_Matrx)
      if (runif(1) < Pro_accept(next.distance, initial.distance, temp)) {
        initial.config <- next.config
        initial.distance <- next.distance
        dist <- c(dist, initial.distance)
      }
    }
    temp <- a*temp
  }
  ## the above codes is the kernel of the Stimulated Annealling Algorithm for Travelling Salesman Problem 
  par(mfrow=c(1,2))
  plot(dist, t="l", main="Path length vs. iterations", ylab="Path length",las=1,col=4)
  Plot.Cities(cities, initial.config)
  ##return(cities$name[initial.config])
  ##return(initial.config)
  return(Path.Length(initial.config, Dis_Matrx))
}

Plot.Cities <- function(cities, city_list) {
  new_cities <- cities[city_list,]
  
  x <- new_cities[,3]
  y <- new_cities[,2]
  ## the head and the tail have not connected
  
  rout_x <- c(x, new_cities[1,][3])
  rout_y <- c(y, new_cities[1,][2])
  ## a closed route(hamilton recycling), the head and the tail will be connected
  
  
  plot(rout_x,rout_y,xlim=c(-15,30), t="l", cex=1,xlab="Latitude",ylab="Longitude",las=1)
  text(x-2,y,new_cities[,1],cex=0.7,col="blue")
  points(x,y,pch=19,col=2)
  title(main="Positions of 13 European Capital Cities",col.main="red", cex.main=1,font.main=4)
}
