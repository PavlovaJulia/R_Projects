#	Решение задачи классификации методом PF

require("plotrix")

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

# Функции ядер

ker_gaus <- function(r){
  # гауссовское
  (2*pi)^(-0.5)*exp(-0.5*(r^2))
}

ker_rec <- function(r){
  # прямоугольное
  (0.5)*(abs(r)<= 1)
}

ker_triangle <- function(r){
  # треугольное
  (1-abs(r))*(abs(r) <= 1)
}

ker_kvar <- function(r){
  # квартическое
  (15/16)*(1-r^2)^2*(abs(r) <= 1)
}

ker_epanech <- function(r){
  # епанечниково
  (3/4)*(1-r^2)*(abs(r) <= 1)
}

pf <- function(distances, h, xl, ker_function, g) {	  
  #	возвращает класс объекта чаще всего встречающейся
  
  n <- ncol(xl)
  classes <- xl[,n] 
  table <- table(classes)
  table[1:length(table) ] <- 0
  for(i in 1:nrow(xl)){ # i объект из i строки 
    r <- distances[i]/h[i]
    class_i <- xl[i,n]
    table[class_i] = table[class_i] + g[i]*ker_function(r) 
  }
  if(max(table) != 0){
    class <- names(which.max(table)) 
    return (class)	  
  }
  return ("")
}

get_h <- function(xl){
  # ширина окна  
  h <- rep(0, nrow(xl))
  for(i in 1:nrow(xl)){
#    if(xl[i, ncol(xl)] == "setosa")
 #     h[i] <- 1
  #  else
      h[i] <- 0.3
  }
  return(h)
}

potencial <- function(xl, h, error, ker_function){
  l <- nrow(xl)
  n <- ncol(xl)
  g <- rep(0, l)
  get_error <- error + 1
  cnt <- 1
  distances <- matrix(NA, l, l)
  for(i in 1:l)
  distances[i, ] <- get_dist(xl, xl[i, 1:(n-1)])
  while(get_error > error){
    for(i in 1:l){
      xl1 <- xl[i, 1:(n-1)]
      xl1_class <- pf(distances[i,], h, xl, ker_function, g)
      if(xl1_class != xl[i,n] && g[i] < 3){
        g[i] <- g[i]+1
        break
      }
    }
    get_error <- 0
    for(i in 1:l){
      xl1 <- xl[i, 1:(n-1)]
      xl1_class <- pf(distances[i,], h, xl, ker_function, g)
      if(xl1_class != xl[i,n]){
        get_error <- get_error + 1
      }
    }
    print(get_error)
    print(g)
  }
  return(g)  
} 


grafic <- function(xl, new_xl, h, ker_function, g){
  l <- nrow(xl)
  n <- ncol(xl)
  new_l <- nrow(new_xl)
  new_n <- ncol(new_xl)
  par(mfrow = c(1, 2)) # рисуем график pf и потенциалы вместе 
  
  colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue") 
  red <- col2rgb("red")
  red <- rgb(red[1], red[2], red[3], alpha = 27, max = 255)
  green <- col2rgb("green")
  green <- rgb(green[1], green[2], green[3], alpha = 27, max = 255)
  blue <- col2rgb("blue")
  blue <- rgb(blue[1], blue[2], blue[3], alpha = 27, max = 255)
  
  colors1 <- c("setosa" = red, "versicolor" = green, "virginica" = blue) 
  plot(iris[ , 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], main = "Распределение потенциалов", xlab = "длина листа", ylab = "ширина листа", asp = 1) 
  for(i in 1:l)
    if(g[i] != 0)
      draw.circle(xl[i, 1], xl[i, 2], radius = g[i], border = colors1[xl[i, n]], col = colors1[xl[i, n]])
  
  plot(iris[ , 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], main = "Задача классификации PF", xlab = "длина листа", ylab = "ширина листа", asp = 1) 
  
  OY<-c(seq(0.0, 3.0, 0.1)) # от 0 до 3 с шагом 0.1
  OX<-c(seq(0.0, 7.0, 0.1))
  
  for(i in OX){
    for(j in OY){
      z <- c(i, j)		
      distances <- get_dist(new_xl, z)
      class <- pf(distances, h, new_xl, ker_function, g)
      if(class != 0)
        points(z[1], z[2], pch = 22, col = colors[class], asp = 1) 
    }
  }
  
}

main <- function(ker_function){
  xl <- iris[, 3:5]
  h <- get_h(xl)
  g <- potencial(xl, h, 7, ker_function)
  # new_xl <- xl[which(g > 0),]
  grafic(xl, xl, h, ker_function, g)
}

main(ker_rec)