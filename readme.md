# Содержание

# Метрические алгоритмы классификации:
* [K ближайших соседей (KNN)](#метод-k-ближайших-соседей-knn)
* [K взвешенных ближайших соседей (KWNN)](#метод-k-взвешанных-соседей-kwnn)
* [Парзеновское окно (PW)](#метод-парзеновского-окна-pw)
* [Потенциальные функции (PF)](#метод-потенциальных-функцийpf)

## Метрические алгоритмы классификации

### Сводная таблица ошибок метрических алгоритмов

Название метода | Параметры                      | Количество ошибок
--------------- | ----------------------------   | -----------------
[1NN](#5-график-1nn-как-частный-случай-алгоритма-knn-при-k-=-1)            |  k = 1                         |         7 
[KNN](#метод-k-ближайших-соседей-knn)             |  k = 6                         |         5
[KWNN](#метод-k-взвешанных-соседей-kwnn)            |  k = 6 q = 1                   |         5
[PW](#метод-парзеновского-окна-pw)              |  гауссовское h = 0.1           |         6       
[PW](#метод-парзеновского-окна-pw)				|  прямоугольное h = 0.4         |         6
[PW](#метод-парзеновского-окна-pw)				|  треугольное h = 0.4           |         6
[PW](#метод-парзеновского-окна-pw)				|  квартическое h = 0.4          |         6
[PW](#метод-парзеновского-окна-pw)				|  епанечниково h = 0.4          |         6


 Метрические методы обучения - методы основанные на анализе сходства объектов (схожим объектам соответствует схожие ответы).
 
 Для формализации понятия сходства вводится функция расстояния в пространстве.
 
 Метрический алгоритм классификации с обучающей выборкой Xl, относит классифицируемый объкт к тому классу, для которого суммарный вес ближайших обучающих объектов максимален.

 Выбирая весовую функцию можно получать различные метрические классификаторы.
 
### Метод K ближайших соседей (KNN)

Алгоритм К ближайших соседей - KNN относит объект *z* к тому классу, элементов которого больше всего среди k ближайших соседей. 

В задаче используется выборка, содержащая 150 объектов-ирисов:
по 50 объектов каждого из трёх классов. В задаче требуется определить класс любого объекта.


### Алгоритм:
 Сортируется выборка по расстоянию.
 Используется метод KNN который: возвращает класс чаще всего встречается среди k ближайших соседей и присваивает этим ближайшим соседям вес равный единицы.

### Программная реализация алгоритма выглядит следующим образом:
```R
knn <- function(xl, z, k) {	  
    #    функция которая возвращает класс объекта чаще всего встречающейся
	      
      n <- dim(xl)[2] - 1 
      classes <- xl[1:k, n + 1] 
      counts <- table(classes) 
      class <- names(which.max(counts)) 
      return (class)	  
    }
```
 Используется метод скользящего контроля: LOO который определят оптимальное значение k. 
 
 Суть метода:

* Убирается первый объект выборки, и запускается KNN для остальных. 

* Сравнивается класс первого объекта который получился и истиный класс этого же объекта, если они не совпадают, 
то сумма ошибок увеличивается на 1. 

* Проделаваются эти же самые действия для всех остальных объектов выборки тем самым накапливая сумму погрешности. 

* Накопливаемая сумма делится на количество объектов выборки. 

* LOO применяется для каждого k, и то k для которого усредненная ошибка минимальная и будет наше оптимальное k.

### LOO

Определение оптимального значения k:

```R
    loo <- function(xl) {
    #    функция которая возвращает массив средних ошибок
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
    #    записываем в k индекс минимального значения 
      k <- which.min(loo)
      return(k)
    }
```
 Рисуется knn и Loo.
	
###	График KNN и Loo
![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/KNNandLOO.png)

 5. График 1NN как частный случай алгоритма KNN, при k = 1.
![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab1/1NN.png)

## Метод k взвешанных соседей (KWNN)

Суть алгоритма: 
Вводится строго убывающая функция весов ![](http://latex.codecogs.com/gif.latex?w%28i%29%20%3D%20q%5E%7Bi%7D) - геометрическая 
прогрессия, где *i* это ранг соседа, а *q* - параметр, который подбирается в LOO.

Тоесть каждому *k*-тому соседу присваивается свой вес таким образом: чем ближе сосед *к* классифицируемому объекту тем больше ему будет присваиваться вес.
А какой именно будет присваиваться вес будет определять параметр *q*. 

Кроме функции веса алгоритм ничем не отличается от алгоритма KNN.

### Программная реализация алгоритма выглядит следующим образом:

```R
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
```
### LOO KWNN 

```R
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
```
	
### График KWNN и Loo 
![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/KWNNandLOO.png)

## Метод парзеновского окна (PW)

Рассматривается весовая функция ![](http://latex.codecogs.com/gif.latex?w%28i%29%20%3D%20K%28%5Cfrac%7B%5Crho%28x_%7Bi%7D%2Cz%29%7D%7Bh%7D%29) не как функция от ранга соседа а как функция от расстояния. 

где *К* - функция ядра, а h -  ширина окна. Ядро - произвольная четная функция невозрастающая на [0, +inf). 

Суть метода: алгоритм для классифицируемой точки *z* строит окружность, радиусом h(ширина окна). Все точки, не попавшие в эту окружность, не рисуются, если функция ядра не является гауссовским. Для остальных, вычисляется вес, суммируется, и класс с наибольшим весом присваивается классифицируемой точке.

Функции ядра бывают 
* Прямоугольное ![](https://camo.githubusercontent.com/a7fd7f2a009cddb610b460b22300a5a9eab873e7/687474703a2f2f6c617465782e636f6465636f67732e636f6d2f7376672e6c617465783f2535436c61726765253230522532387a2532392532302533442532302535436672616325374231253744253742322537442532302535422537437a2537432532302535436c657125323031253544)
* Треугольное ![](https://camo.githubusercontent.com/90e9884466ca65e36d73ed6a717e326123ce573f/687474703a2f2f6c617465782e636f6465636f67732e636f6d2f7376672e6c617465783f2535436c61726765253230542532387a253239253230253344253230253238312532302d2532302537437a25374325323925323025354363646f742532302535422537437a2537432532302535436c657125323031253544)
* Квартическое ![](https://camo.githubusercontent.com/1e5b0ae73cde8fe5b7a3c248bdbddc1639f96404/687474703a2f2f6c617465782e636f6465636f67732e636f6d2f7376672e6c617465783f2535436c61726765253230512532387a2532392532302533442532302535436672616325374231352537442537423136253744253230253238312532302d2532307a253545322532392535453225323025354363646f742532302535422537437a2537432532302535436c657125323031253544)
* Епанечниково ![](https://camo.githubusercontent.com/0cad7a4e41913e389111d61935a76fba1cbff264/687474703a2f2f6c617465782e636f6465636f67732e636f6d2f7376672e6c617465783f2535436c61726765253230452532387a253239253230253344253230253543667261632537423325374425374234253744253230253238312532302d2532307a2535453225323925323025354363646f742532302535422537437a2537432532302535436c657125323031253544)
* Гауссовское ![](http://latex.codecogs.com/gif.latex?%5CLARGE%20%282%5Cpi%29%5E%7B-%5Cfrac%7B1%7D%7B%7D2%7De%5E%7B%28-%5Cfrac%7B1%7D%7B2%7D*r%5E%7B2%7D%29%7D)

## Программная реальзация алгоритма

```R
pw <- function(distances, h, xl, ker_function) {	  
  #	возвращает класс объекта c наибольшим сумарным весом
  
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
  return ("")
}
```
## LOO PW

```R
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
  #	записывает в h индекс минимального значения массива
  h <- which.min(loo)
  return(h/10)
}
```

### Графики PW


#### Гауссовское ядро

![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/PW_Gays.png)

#### Прямоугольное

![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/PW_rec.png)

#### Треугольное

![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/PW_triangle.png)

#### Квартическое

![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/PW_kvar.png)

#### Епанечниково

![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/PW_epanech.png)

## Метод потенциальных функций(PF)

Рассматривается весовая функция ![](http://latex.codecogs.com/gif.latex?w%28x_%7Bi%7D%2C%20z%29%20%3D%20%5Cgamma_%7Bi%7D*K%28r%29), 

где *К* - функция ядра, а ![](http://latex.codecogs.com/gif.latex?%5Cgamma) - потенциал . 

Суть метода: Для каждой точки выборки строится окружность радиусом h который мы сами выбираем и задается сила потенциала, считается вес для каждого объекта. Суммируется вес одинаковых классов  и класс с наибольшим весом присваивается классифицируемому объекту.

Вычисление потенциала: Вначале всем объектам присваевается нулевой потенциал. Для выбранного объекта запускается алгоритм классификации, если полученый класс не совпадает, тогда потенциал этого объекта увеличивается на еденицу. Подсчитывается число ошибок, и если оно больше заанного числа, то беруться следующие объекты и повторяются одни и те же действия.

## Программная реальзация алгоритма

```R
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
```
### Вычисление потенциала 

```R
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
```
### Графики PF

#### Гауссовское ядро

![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/PF-gause.png)

#### Прямоугольное

![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/Rplot.png)

