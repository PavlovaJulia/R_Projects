# Метрические алгоритмы классификации:

## K ближайших соседей (KNN)

В задаче используется выборка, содержащая 150 объектов-ирисов:
по 50 объектов каждого из трёх классов. В задаче требуется определить класс любого объекта.


Алгоритм:
1. Сортируется выборка по расстоянию.
2. Используется метод KNN который: возвращает класс чаще всего встречается среди k ближайших соседей.
3. Используется метод скользящего контроля:LOO который определят оптимальное значение k. 
Для этого убирается первый объект выборки, и запускаем KNN для отсальных. 
Далее сравнивается класс первого объекта который получился и истиный класс этого же объекта, если они не совпадают, 
то сумма ошибок увеличивается на 1. 
Проделаваются эти же самые действия для всех остальных объектов выборки тем самым накапливая сумму погрешности. 
Далее накопливаемая сумма делится на количество объектов выборки. 
LOO применяется для каждого k, и то k для которого усредненная ошибка минимально и будет наше оптимальное k.
4. Рисуется выборка.
5. Рисуются классифицируемые объекты.

плюсы
* Простота реализации.
минусы
*Неустойчивость к погрешности.
*Отсутствие параметров, которые можно было 

### Программная реализация алгоритма выглядит следующим образом:
    KNN <- function(xl, z, k) {	  
		orderedXl <- sort_ojects_by_dist(xl, z)     
		n <- dim(orderedXl)[2] - 1 
		classes <- orderedXl[1:k, n + 1] 
		counts <- table(classes) 
		class <- names(which.max(counts)) 
		return (class)	  
	}
	
###	График классификации алгоритмом KNN и частный случай при k=1 1NN
![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/KNN.png)
![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab1/1NN.png)

### LOO

Определение оптимального значения k:

    LOO <- function(xl, k) {
		z <- c(xl[1,1], xl[1,2])
		xl1 <- xl[2:dim(xl)[1], ]
		class <- KNN(xl1, z, k)
	
		if(xl[1,3]== class) 
			error=0
		else error=1
	
		sum <- 0
		sum <- sum+error
		
		for(i in 2:dim(xl)[1]){
			z <- c(xl[i,1],xl[i,2])
			xl1 <- rbind(xl[1:(i-1),], xl[(i+1):dim(xl)[1],])
			class <- KNN(xl1, z, k)
	
		if(xl[i,3]== class) 
			error=0
		else error=1
		
		sum <- sum+error
	}	
		sum <- (sum/(dim(xl)[1]))
		return(sum)
	}

### График зависимоти LOO от k
![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/LOO.png)
