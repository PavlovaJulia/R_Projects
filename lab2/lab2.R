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

KNN <- function(xl, z, k) {	  
	 orderedXl <- sortObjectsByDist(xl, z)     
	 n <- dim(orderedXl)[2] - 1 
	 classes <- orderedXl[1:k, n + 1] 
	 counts <- table(classes) 
	 class <- names(which.max(counts)) 
	 return (class)#возвращает класс объекта который чаще всего встречается
}
LOO <- function(xl,k) {
 z<-c(xl[1,1],xl[1,2])
	xl1<-xl[2:dim(xl)[1],]
	class<- KNN(xl1, z, k)
	if(xl[1,3]== class) error=0
	else error=1
		sum<-0
		sum<-sum+error
	for(i in 2:dim(xl)[1]){
	  z<-c(xl[i,1],xl[i,2])
		xl1<- rbind(xl[1:(i-1),], xl[(i+1):dim(xl)[1],])
		class<- KNN(xl1, z, k)
		if(xl[i,3]== class) error=0
	else error=1
		sum<-sum+error
	}
	sum<-(sum/(dim(xl)[1]))
	return(sum)
}

grafic1 <- matrix(NA, 1, 2)
grafic2 <- matrix(NA, 1, 2)


prevSumerror<-1
for(i in 2:7){
	sumerror<-LOO(xl, i)
	if(prevSumerror>sumerror){
		prevSumerror<-sumerror
		k<-i
	}	
}
#colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue") 
#plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1, main = "Задача классификации 1NN", xlab = "длина листа", ylab = "ширина листа" ) 
 
#OY<-c(seq(0.0, 3.0, 0.1)) # от 0 до 3 с шагом 0.1
#OX<-c(seq(0.0,7.0,0.1))
#for(i in OX){
#	for(j in OY){
#	z<-c(i,j)
#	class <- KNN(xl, z, k) 
#	points(z[1], z[2], pch = 22, col = colors[class], asp = 1) }
#}


plot(



