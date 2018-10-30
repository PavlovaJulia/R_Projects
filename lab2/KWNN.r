#	Решение задачи классификации методом KWNN

euclidean_distance <- function(u, v){
  #	функция расстояния
  
  sqrt(sum((u - v)^2)) 
}


sort_ojects_by_dist <- function(xl, z, metric_function = euclidean_distance){
  #	сортировка
  
  l <- nrow(xl)	# размерность выборки по строкам     
  n <- ncol(xl) - 1 	# размерность выборки по столбцам
  distances <- matrix(NA, l, 2) 
  
  for (i in 1:l)  
  {         
    distances[i, ] <- c(i, metric_function(xl[i, 1:n], z))	# расстояние от каждой точки до классифицируемой
  }  
  
  orderedXl <- xl[order(distances[, 2]), ]	# сортируем выборку по расстоянию    
  return (orderedXl);
}	


kwnn <- function(xl, k, q) {	  
  #	возвращает класс объекта чаще всего встречающейся
  
  n <- ncol(xl)
  classes <- xl[1:k, n] 
  table <- table(classes)
  table[1:length(table) ] <- 0
  for(i in names(table))
    for(j in 1:k) # по j-тым соседям
      if(i == xl[j, n]) # i - классы
        table[i] =  table[i] + q^j # добавляем вес
  class <- names(which.max(table)) 
  return (class)	  
}


loo <- function(xl) {
  #	функция возвращает массив средних ошибок
  l <- nrow(xl)
  n <- ncol(xl)
  value_q <- seq(0.1, 1, 0.1) # перебираем q 
  Sum <- matrix(0, l-1, length(value_q)) # матрица, где k это строки а q - столбцы
  for (i in 1:l){
    z <- xl[i, 1 : (n-1)]
    xl1 <- sort_ojects_by_dist(xl[-i, ], z)		
    for(k in 1:(l-1)){
      int_q <- 1
      for(q in value_q){
       class <- kwnn(xl1, k, q)
       if(xl[i, n] != class) 
        Sum[k, int_q] <- Sum[k, int_q] + 1/l 	
       int_q <- int_q + 1 
      }
    }
  }
  return(Sum)
}


optimal <- function(loo){
  #	находит в матрице минимальное значение и в k записывает индеск стоки а в q индекс столбца
  ind_min_k <- 1
  l <- nrow(loo)
  min <- min(loo[1,])
  for(i in 1:l){
    if(min(loo[i,]) < min) {
      min = min(loo[i,])
      ind_min_k <- i
      
    }
  }
  ind_min_q <- (which.min(loo[ind_min_k,]))/10
  opt <- c(ind_min_k,ind_min_q)
  return(opt)
}


grafic <- function(xl, k, Sumerror, q){
  par(mfrow = c(1, 2)) # рисуем график knn и loo вместе 
  
  colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue") 
  plot(iris[ , 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], main = "Задача классификации KWNN", xlab = "длина листа", ylab = "ширина листа", asp = 1) 
  
  OY<-c(seq(0.0, 3.0, 0.1)) # от 0 до 3 с шагом 0.1
  OX<-c(seq(0.0, 7.0, 0.1))
  
  for(i in OX){
    for(j in OY){
      z <- c(i, j)		
      orderedXl <- sort_ojects_by_dist(xl, z)
      class <- kwnn(orderedXl, k, q)
      points(z[1], z[2], pch = 22, col = colors[class], asp = 1) 
    }
  }
  
  int_q <- q*10
  
  
  plot(Sumerror[,int_q], type = "l", bg = "blue", col = "blue",  main = "График зависимости LOO от k", xlab = "значение k", ylab = "значение LOO")
  points(k, Sumerror[k, int_q], pch = 21, col = "red", bg = "red")
  txt <- paste("k = ", k, "\n", "Loo =", round(Sumerror[k, int_q], 3))
  text(k, Sumerror[k, int_q], labels = txt, pos = 3)
  
}

main <- function(){
  xl <- iris[ ,3:5] # выборка
  Sumerror <- loo(xl)
  opt <- optimal(Sumerror)
  opt_k <- opt[1]
  opt_q <- opt[2]
  grafic(xl, opt_k, Sumerror, opt_q)
}

main()

