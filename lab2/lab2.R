#	Решение задачи классификации методом KWNN

euclidean_distance <- function(u, v){
  #	функция расстояния
  
  sqrt(sum((u - v)^2)) 
}


sort_ojects_by_dist <- function(xl, z, metric_function = euclidean_distance){
  #	сортировка
  
  l <- dim(xl)[1]	# размерность выборки по строкам     
  n <- dim(xl)[2] - 1 	# размерность выборки по столбцам
  distances <- matrix(NA, l, 2) 
  
  for (i in 1:l)  
  {         
    distances[i, ] <- c(i, metric_function(xl[i, 1:n], z))	# расстояние от каждой точки до классифицируемой
  }  
  
  orderedXl <- xl[order(distances[, 2]), ]	# сортируем выборку по расстоянию    
  return (orderedXl);
}	


kwnn <- function(xl, z, k) {	  
  #	возвращает класс объекта чаще всего встречающейся
  
  #i <- 1
  #while(i <= k){
  #  xl[i,1] <- (k-i+1)*q*q
  #}
  n <- ncol(xl)
  table <- table(xl[,n])
  table[1:length(table)] <- 0
  for(i in names(table)){
    for(j in 1:k)
    if(i == xl[j,n]) 
      table[i] = table[i] + (k-j+1)/k
  }
  class <- names(which.max(table))
  return (class)	  
}


loo <- function(xl) {
  #	функция возвращает массив средних ошибок
  l <- nrow(xl)
  n <- ncol(xl)
  Sum <- rep(0, (l-1))
  for (i in 1:l){
    z <- xl[i, 1 : (n-1)]
    xl1 <- sort_ojects_by_dist(xl[-i, ], z)		
    for(j in 1:(l-1)){
      class <- kwnn(xl1, z, j)	
      if(xl[i, n] != class) 
        Sum[j] <- Sum[j] + 1/l 	
    }
  }
  return(Sum)
}

optimal <- function(loo){
  #	записывает в k индекс минимального значения 
  k <- which.min(loo)
  return(k)
}

grafic <- function(xl, k, Sumerror){
  par(mfrow = c(1, 2)) # рисуем график knn и loo вместе 
  
  colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue") 
  plot(iris[ , 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], main = "Задача классификации KWNN", xlab = "длина листа", ylab = "ширина листа", asp = 1) 
  
  OY<-c(seq(0.0, 3.0, 0.1)) # от 0 до 3 с шагом 0.1
  OX<-c(seq(0.0, 7.0, 0.1))
  
  
  for(i in OX){
    for(j in OY){
      z <- c(i, j)		
      orderedXl <- sort_ojects_by_dist(xl, z)
      class <- kwnn(orderedXl, z, k) 
      points(z[1], z[2], pch = 22, col = colors[class], asp = 1) 
    }
  }
  
  plot(Sumerror, type = "l", bg = "blue", col = "blue",  main = "График зависимости LOO от k", xlab = "значение k", ylab = "значение LOO")
  points(k, Sumerror[which.min(Sumerror)], pch = 21, col = "red", bg = "red")
  txt <- paste("k = ", k, "\n", "Loo =", round(Sumerror[which.min(Sumerror)], 3))
  text(k, Sumerror[which.min(Sumerror)], labels = txt, pos = 3)
  
}

main <- function(){
  xl <- iris[ ,3:5] # выборка
  Sumerror <- loo(xl)
  k <- optimal(Sumerror)
  grafic(xl, k, Sumerror)
}

main()

