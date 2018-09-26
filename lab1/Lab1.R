xl<-(iris[,3:5])#наша выборка

euclideanDistance <- function(u, v)#функция расстояния
{     
	sqrt(sum((u - v)^2)) 
}

sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance) #сортировка

{
     l <- dim(xl)[1]#размерность выборки по строкам     
	 n <- dim(xl)[2] - 1 #размерность выборки по столбцам
	 distances <- matrix(NA, l, 2) 
	      for (i in 1:l)  
		  {         
			distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))#расстояние от каждой точки до классифицируемой
		  }  
	 orderedXl <- xl[order(distances[, 2]), ]#сортируем выборку по расстоянию    
	 return (orderedXl);
}	

NN1 <- function(xl, z) {	  
	 orderedXl <- sortObjectsByDist(xl, z)     
	 n <- dim(orderedXl)[2] - 1 
	 class <- orderedXl[1, n + 1] 
	 return (class)#возвращает класс объекта
}

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue") 
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1, main = "Задача классификации 1NN", xlab = "длина листа", ylab = "ширина листа" ) 
 
OY<-c(seq(0.0, 3.0, 0.1)) # от 0 до 3 с шагом 0.1
OX<-c(seq(0.0,7.0,0.1))
for(i in OX){
	for(j in OY){
	z<-c(i,j)
	class <- NN1(xl, z) 
	points(z[1], z[2], pch = 22, col = colors[class], asp = 1) }
}


