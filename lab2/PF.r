#	Решение задачи классификации методом PW

euclidean_distance <- function(u, v){
  #	функция расстояния
  
  sqrt(sum((u - v)^2)) 
}


get_dist <- function(xl, z, metric_function = euclidean_distance){
  #	растояние
  
  l <- nrow(xl)	# размерность выборки по строкам     
  n <- ncol(xl) - 1 	# размерность выборки по столбцам
  distances <- rep(0,l) 
  
  for (i in 1:l)  
  {         
    distances[i] <- metric_function(xl[i, 1:n], z)	# расстояние от каждой точки до классифицируемой
  }  
  
  return (distances);
}