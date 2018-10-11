# Метрические алгоритмы классификации:

 Метрические методы обучения - методы основанные на анализе сходства объектов (схожим объектам соответствует схожие ответы)
 
 Для формализации понятия сходства вводится функция расстояния в пространстве.
 
 Метрический алгоритм классификации с обучающей выборкой Xl, относит классифицируемый объкт к тому классу, для которого суммарный вес ближайших обучающих объектов максимален 

 Выбирая весовую функцию можно получать различные метрические классификаторы.
 
## Метод K ближайших соседей (KNN)

В задаче используется выборка, содержащая 150 объектов-ирисов:
по 50 объектов каждого из трёх классов. В задаче требуется определить класс любого объекта.


## Алгоритм:
1. Сортируется выборка по расстоянию.
2. Используется метод KNN который: возвращает класс чаще всего встречается среди k ближайших соседей.

### Программная реализация алгоритма выглядит следующим образом:
    knn <- function(xl, z, k) {	  
	#	функция которая возвращает класс объекта чаще всего встречающейся
	      
		n <- dim(xl)[2] - 1 
		classes <- xl[1:k, n + 1] 
		counts <- table(classes) 
		class <- names(which.max(counts)) 
		return (class)	  
	}

3. Используется метод скользящего контроля: LOO который определят оптимальное значение k. 

* Убирается первый объект выборки, и запускается KNN для остальных. 

* Сравнивается класс первого объекта который получился и истиный класс этого же объекта, если они не совпадают, 
то сумма ошибок увеличивается на 1. 

* Проделаваются эти же самые действия для всех остальных объектов выборки тем самым накапливая сумму погрешности. 

* Накопливаемая сумма делится на количество объектов выборки. 

* LOO применяется для каждого k, и то k для которого усредненная ошибка минимальная и будет наше оптимальное k.

### LOO

Определение оптимального значения k:

    loo <- function(xl) {
	#	функция которая возвращает массив средних ошибок
		l <- nrow(xl)
		n <- ncol(xl)
		Sum <- rep(0, l)
		for (i in 1:l){
			z <- xl[i, 1 : (n-1)]
			xl1 <- sort_ojects_by_dist(xl[-i, ], z)		
			for(j in 1:l){
				class <- knn(xl1, z, j)	
				if(xl[i, n] != class) 
					Sum[j] <- Sum[j] + 1/l 	
			}
		}
		return(Sum)
	}

	optimal <- function(loo){
	#	записываем в k индекс минимального значения 
		k <- which.min(loo)
		return(k)
	}

4. Рисуется knn и Loo.
	
###	График KNN и Loo
![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/KNN.png)
![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/KNNandLOO.png)

5. График 1NN как частный случай алгоритма KNN, при k = 1.
![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab1/1NN.png)
![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/KNN.png)


