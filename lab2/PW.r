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

ker_gaus <- function(r){
  (2*pi)^(-0.5)*exp(-0.5*(r^2))
}

ker_rec <- function(r){
  (0.5)*(abs(r)<= 1)
}

ker_triangle <- function(r){
  (1-abs(r))*(abs(r) <= 1)
}

ker_kvar <- function(r){
  (15/16)*(1-r^2)^2*(abs(r) <= 1)
}

ker_epanech <- function(r){
  (3/4)*(1-r^2)*(abs(r) <= 1)
}

pw <- function(distances, h, xl, ker_function) {	  
  #	возвращает класс объекта чаще всего встречающейся
  
  n <- ncol(xl)
  classes <- xl[,n] 
  table <- table(classes)
  table[1:length(table) ] <- 0
  for(i in 1:nrow(xl)){
    r <- distances[i]/h
    class_i <- xl[i,n]
    table[class_i] = table[class_i] + ker_function(r) 
  }
  if(max(table) != 0){
    class <- names(which.max(table)) 
    return (class)	  
  }
  return (0)
}


loo <- function(xl, ker_function) {
  #	функция возвращает массив средних ошибок
  
  l <- nrow(xl)
  n <- ncol(xl)
  value_h <- seq(0.1, 2, 0.1)
  Sum <- rep(0, length(value_h))
  for (i in 1:l){
    z <- xl[i, 1 : (n-1)]
    xl1 <- xl[-i,]
    distances <- get_dist(xl1, z)
    cnt <- 1
    for(h in value_h){
      class <- pw(distances, h, xl1, ker_function)	
      if(xl[i, n] != class || class == 0) 
        Sum[cnt] <- Sum[cnt] + 1/l 	
      cnt <- cnt +1  
    }
  }
  return(Sum)
}


optimal <- function(loo){
  #	записывает в k индекс минимального значения массива
  h <- which.min(loo)
  return(h/10)
}


grafic <- function(xl, h, Sumerror, ker_function){
  par(mfrow = c(1, 2)) # рисуем график kwnn и loo вместе 
  
  colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue") 
  plot(iris[ , 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], main = "Задача классификации PW", xlab = "длина листа", ylab = "ширина листа", asp = 1) 
  
  OY<-c(seq(0.0, 3.0, 0.1)) # от 0 до 3 с шагом 0.1
  OX<-c(seq(-0.5, 7.0, 0.1))
  
  for(i in OX){
    for(j in OY){
      z <- c(i, j)		
      distances <- get_dist(xl, z)
      class <- pw(distances, h, xl, ker_function)
      if(class != 0)
        points(z[1], z[2], pch = 22, col = colors[class], asp = 1) 
    }
  }
  
  h_int <- h*10
  plot(Sumerror, type = "l", bg = "blue", col = "blue",  main = "График зависимости LOO от h", xlab = "значение h", ylab = "значение LOO")
  points(h_int, Sumerror[which.min(Sumerror)], pch = 21, col = "red", bg = "red")
  txt <- paste("h = ", h, "\n", "Loo =", round(Sumerror[which.min(Sumerror)], 3))
  text(h_int, Sumerror[which.min(Sumerror)], labels = txt, pos = 3)
  
}

main <- function(ker_function){
  xl <- iris[ ,3:5] # выборка
  Sumerror <- loo(xl, ker_function)
  h <- optimal(Sumerror)
  grafic(xl, h, Sumerror, ker_function)
}

main(ker_gaus)

