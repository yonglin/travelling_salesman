library(maps)
library(fossil)
## city_name_List <- c('Amsterdam','Berlin','Brussels','Copenhagen','Dublin','Helsinki','London','Oslo','Paris','Prague','Stockholm','Warsaw','Vienna')
Generate.Cities <- function(city_name_List) {
  data(world.cities)
  Euro_Cities <- world.cities[is.element(world.cities$name,city_name_List)&world.cities$capital==1,]
  cities <- subset(Euro_Cities,select=c('name','lat','long'))
  return(cities)
}

Generate.Dis_Matrx <- function(city_name_List) {
  cities <- Generate.Cities(city_name_List)
  cordinates <- subset(cities, select=c('lat','long'))
  Dis_Matrx <- earth.dist(cordinates,dis=FALSE)
  return(Dis_Matrx)
}



Path.Length <- function(city_list,Dis_Matrx) {
  # Path.Length computes the path length of the path connecting
  # the cities passed in.
  total.distance <- 0
  for (i in 1:(length(city_list)-1)) {
    total.distance <- total.distance + Dis_Matrx[city_list[i],city_list[i+1]]
  }
  total.distance <- total.distance + Dis_Matrx[city_list[length(city_list)],city_list[1]] 
  return(total.distance)
}

Pro_accept <- function(new, old, tempature) {
  # Metropolis-Hastings ratio
  return(min(1, exp((old - new) / tempature)))
}


Random_neighbor_index <- function(numbers_cities) {
  head <- sample(1:(numbers_cities-2),1)
  if (head==numbers_cities-2){
    tail <- numbers_cities
  }
  else{
    tail <- sample((head+2):(numbers_cities),1)
  }
  
  index <- c(head,tail)
  return (index)
}

Generate_neighbor <- function(city_list) {
  #  city_list_core <- city_list
  neighbor_index <- Random_neighbor_index(length(city_list))
  head <- neighbor_index[1]
  tail <- neighbor_index[2]
  reverse_segment <- rev(city_list[head:tail])
  city_list[head:tail] <- reverse_segment
  return (city_list)
}

Anneal <- function(city_name_List,init_temperature,decreasing_rate) {
  ## (10000000,0.7)
  dist <- c()
  init_temperature <- init_temperature
  temp <- init_temperature
  a <- decreasing_rate
  
  Dis_Matrx <- Generate.Dis_Matrx(city_name_List)
  initial.config <- sample(1:13)
  cities <- Generate.Cities(city_name_List)
  ##Plot.Cities(cities, initial.config)
  
  initial.distance <- Path.Length(initial.config, Dis_Matrx)
  dist[1] <- initial.distance
  
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
  ##plot(dist, t="l", main="Path length vs. time", ylab="Path length",las=1)
  Plot.Cities(cities, initial.config)
  ##return(cities$name[initial.config])
  ##return(initial.config)
  return(Path.Length(initial.config, Dis_Matrx))
}

Plot.Cities <- function(cities, city_list) {
  new_cities <- cities[city_list,]
  x <- new_cities[,3]
  y <- new_cities[,2]
  
  rout_x <- c(x, new_cities[1,][3])
  rout_y <- c(y, new_cities[1,][2])
  plot(rout_x,rout_y,xlim=c(-15,30), t="l", cex=1,xlab="Latitude",ylab="Longitude",las=1)
  ## plot(x,y,xlim=c(-15,30),type='n',asp=1,xlab="Latitude",ylab="Longitude",las=1)
  text(x-2,y,new_cities[,1],cex=0.7,col="blue")
  points(x,y,pch=19,col=2)
  title(main="Positions of 13 European Capital Cities",col.main="red", cex.main=1,font.main=4)
}
