#	������� ������ ������������� ������� KWNN

euclidean_distance <- function(u, v){
  #	������� ����������
  
  sqrt(sum((u - v)^2)) 
}


sort_ojects_by_dist <- function(xl, z, metric_function = euclidean_distance){
  #	����������
  
  l <- dim(xl)[1]	# ����������� ������� �� �������     
  n <- dim(xl)[2] - 1 	# ����������� ������� �� ��������
  distances <- matrix(NA, l, 2) 
  
  for (i in 1:l)  
  {         
    distances[i, ] <- c(i, metric_function(xl[i, 1:n], z))	# ���������� �� ������ ����� �� ����������������
  }  
  
  orderedXl <- xl[order(distances[, 2]), ]	# ��������� ������� �� ����������    
  return (orderedXl);
}	


kwnn <- function(xl, k) {	  
  #	���������� ����� ������� ���� ����� �������������
  
  n <- dim(xl)[2] - 1 
  classes <- xl[1:k, n + 1] 
  table <- table(classes)
  table[1:length(table) ] <- 0
  for(i in names(table))
    for(j in 1:k) # �� j-��� �������
      if(i == xl[j, n + 1]) # i - ������
        table[i] =  table[i] + (k-j+1)/k
  class <- names(which.max(table)) 
  return (class)	  
}


loo <- function(xl) {
  #	������� ���������� ������ ������� ������
  l <- nrow(xl)
  n <- ncol(xl)
  Sum <- rep(0, (l-1))
  for (i in 1:l){
    z <- xl[i, 1 : (n-1)]
    xl1 <- sort_ojects_by_dist(xl[-i, ], z)		
    for(j in 1:(l-1)){
      class <- kwnn(xl1, j)	
      if(xl[i, n] != class) 
        Sum[j] <- Sum[j] + 1/l 	
    }
  }
  return(Sum)
}


optimal <- function(loo){
  #	���������� � k ������ ������������ �������� �������
  k <- which.min(loo)
  return(k)
}


grafic <- function(xl, k, Sumerror){
  par(mfrow = c(1, 2)) # ������ ������ knn � loo ������ 
  
  colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue") 
  plot(iris[ , 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], main = "������ ������������� KNN", xlab = "����� �����", ylab = "������ �����", asp = 1) 
  
  OY<-c(seq(0.0, 3.0, 0.1)) # �� 0 �� 3 � ����� 0.1
  OX<-c(seq(0.0, 7.0, 0.1))
  
  for(i in OX){
    for(j in OY){
      z <- c(i, j)		
      orderedXl <- sort_ojects_by_dist(xl, z)
      class <- kwnn(orderedXl, k)
      points(z[1], z[2], pch = 22, col = colors[class], asp = 1) 
    }
  }
  
  plot(Sumerror, type = "l", bg = "blue", col = "blue",  main = "������ ����������� LOO �� k", xlab = "�������� k", ylab = "�������� LOO")
  points(k, Sumerror[which.min(Sumerror)], pch = 21, col = "red", bg = "red")
  txt <- paste("k = ", k, "\n", "Loo =", round(Sumerror[which.min(Sumerror)], 3))
  text(k, Sumerror[which.min(Sumerror)], labels = txt, pos = 3)
  
}

main <- function(){
  xl <- iris[ ,3:5] # �������
  Sumerror <- loo(xl)
  k <- optimal(Sumerror)
  grafic(xl, k, Sumerror)
}

main()

