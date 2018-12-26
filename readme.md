# Содержание

# Метрические алгоритмы классификации:
* [K ближайших соседей (KNN)](#метод-k-ближайших-соседей-knn)
* [K взвешенных ближайших соседей (KWNN)](#метод-k-взвешанных-соседей-kwnn)
* [Парзеновское окно (PW)](#метод-парзеновского-окна-pw)
* [Потенциальные функции (PF)](#метод-потенциальных-функцийpf)

# Байесовские алгоритмы классификации:
* [Линии уровня](#линии-уровня)
* [Баесовский наивный классификатор](#байесовский-наивный-классификатор)
* [Plug-in](#plug-in)
* [LDF](#ldf)

## Метрические алгоритмы классификации

### Сводная таблица ошибок метрических алгоритмов

Название метода | Параметры                      | Количество ошибок
--------------- | ----------------------------   | -----------------
[KNN](#метод-k-ближайших-соседей-knn)             |  k = 6                         |         5
[KWNN](#метод-k-взвешанных-соседей-kwnn)            |  k = 6, q = 1                   |         5
[PW](#метод-парзеновского-окна-pw)              |  гауссовское h = 0.1           |         6       
[PW](#метод-парзеновского-окна-pw)				|  прямоугольное h = 0.4         |         6
[PW](#метод-парзеновского-окна-pw)				|  треугольное h = 0.4           |         6
[PW](#метод-парзеновского-окна-pw)				|  квартическое h = 0.4          |         6
[PW](#метод-парзеновского-окна-pw)				|  епанечниково h = 0.4          |         6
[1NN](#5-график-1nn-как-частный-случай-алгоритма-knn-при-k-=-1)            |  k = 1                         |         7 

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

Вычисление потенциала: Вначале всем объектам присваевается нулевой потенциал. Для выбранного объекта запускается алгоритм классификации, если полученый класс не совпадает, тогда потенциал этого объекта увеличивается на еденицу. Подсчитывается число ошибок, и если оно больше заданного числа, то берутся следующие объекты и повторяются одни и те же действия.

## Программная реализация алгоритма

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
  g <- rep(0, l) # массив потенциалов
  get_error <- error + 1 # количество ошибок на выборке
  distances <- matrix(NA, l, l) # матрица расстояний
  for(i in 1:l)
  distances[i, ] <- get_dist(xl, xl[i, 1:(n-1)]) # получаем расстояние от каждого объекта до каждого 
  while(get_error > error){
    for(i in 1:l){
      xl1 <- xl[i, 1:(n-1)] # берем i объект без класса
      xl1_class <- pf(distances[i,], h, xl, ker_function, g) # классифицируем
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

![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/PF_gaus.png)

#### Прямоугольное

![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/PF_rec.png)

#### Епанечниково

![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/PF_epanech.png)

# Байесовские алгоритмы классификации
 Байесовские алгоритмы классификации основаны на предположении, что *𝑋×𝑌* — вероятностное пространство с неизвестной плотностью распределения (формула)
 из которого случайно и независимо извлекаются *l* наблюдений, где Р- априорная вероятность, р(х/у)= р(х)- функцинал правдоподобия
 
  Если известны априорные вероятности ![](http://latex.codecogs.com/gif.latex?P_y) и функции правдоподобия ![](http://latex.codecogs.com/gif.latex?p_y%28x%29) для всех ![](http://latex.codecogs.com/gif.latex?y%5Cin%20Y), то минимум среднего риска достигается алгоритмом  
  
  ![](http://latex.codecogs.com/gif.latex?%5CLARGE%20a%28x%29%20%3D%20arg%5Cmax_%7By%5Cin%20Y%7D%20%5CLambda_y%20P_y%20p_y%28x%29)
  
 ## Линии уровня
 
 Линии уровня - геометрическое место точек  пространства, для которых значения исследуемой функции одинаковы. 
  
 Дано: ковариационная матрица(2х2) 2 признаков, шаг плотности распределения. 
 Ковариационная матрица - такая матрица которая показывает как  2 признака связаны друг с другом.

 Построить линии уровня для ковариационной матрицы с нормальным распределением
 
 Определение. Вероятностное распределение с плотностью
 
 ![](http://latex.codecogs.com/gif.latex?%5Clarge%20N%28x%2C%5Cmu%20%2C%20%5CSigma%29%20%3D%20%5Cfrac%7B1%7D%7B%5Csqrt%7B%282%5Cpi%29%5E2%7C%5CSigma%7C%7D%7Dexp%5E%7B%28-%5Cfrac%7B1%7D%7B2%7D%28x-%5Cmu%20%29%20%5CSigma%5E%7B-1%7D%28x-%5Cmu%29%5ET%29%7D)
 
 называется n- мерным нормальным (гауссовским) распределением с мат. ожиданием ![](http://latex.codecogs.com/gif.latex?%5Clarge%20%5Cmu%20%5Cin%20R%5En) и ковариационной матрицой ![](http://latex.codecogs.com/gif.latex?%5Clarge%20%5CSigma%20%5Cin%20R%5E%7Bn%20%5Ctimes%20n%7D),
 матрица симетричная невырожденая положительно определенная.
 
 [Здесь](https://pavlovajulia.shinyapps.io/line_level/) можно посмотреть как работает программа.
  
Выводы:

1) Определитель ковариационной матрицы должен быть положительным.

2) Если в ковариационной матрице коэфициенты a и с равны, тогда линии уровня будут выглядеть как круги.

![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/level_circle.PNG)

3) Если в ковариационная матрице коэфициенты а и с не равны, тогда круги будут вытягиваться в виде элипсов зависимости от того какой признак больше.

![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/level2.PNG)

4) Также элементы ковариационной матрицы b1 и b2 отвечают за поворот элипсов

![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/level1.PNG)

## Байесовский наивный классификатор

Дано: ![](http://latex.codecogs.com/gif.latex?%5Cdpi%7B120%7D%20%5Clarge%20%5CLambda) - 1-го признака, ![](http://latex.codecogs.com/gif.latex?%5Cdpi%7B120%7D%20%5Clarge%20%5CLambda) - 2-го признака, длина выборки(количество точек), мат ожидание 
для 1 класса 1 признака, 1 класса 2 признака, 2 класса 1 признака, 2 класса 2 признака и дисперсия соотвественно

Надо простоить карту классификации 

1) Генерируем выборку с нормальным распределение из данных мат ожиданий и дсиперсии.

2) Восстонавливаем мат ожидание и дисперсию для полученной выборки.

3) Классифицируем точки по формуле
 
![](http://latex.codecogs.com/gif.latex?%5Cdpi%7B120%7D%20%5Clarge%20a%28x%29%20%3D%20argmax%28%5Cln%7B%5CLambda_y%7DP_y%20&plus;%20%5Csum%20p_%7Byj%7D%28%5Cxi_%7Bj%7D%29%29)

Формула расчета плотности нормального распределения

![](http://latex.codecogs.com/gif.latex?%5Cdpi%7B120%7D%20%5Clarge%20p%28x%2C%20%5Cmu%20%2C%5Csigma%20%29%20%3D%20%5Cfrac%7B1%7D%7B%5Csigma%20%5Csqrt%7B2%5Cpi%7D%7D%20%5Cexp%5E-%7B%5Cfrac%7B%28x-%5Cmu%292%20%7D%7B2%5Csigma%5E2%20%7D%7D)

4) Рисуем график

[Здесь](https://pavlovajulia.shinyapps.io/lab4/) можно посмотреть как работает программа

Например:

![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/Baes_naiv_norm.PNG)

## Plug-in

Нормальный дискриминантный анализ — это один из вариантов байесовской классификации, в котором в качестве моделей 
восстанавливаемых плотностей рассматривают многомерные нормальные плотности:

![](http://latex.codecogs.com/gif.latex?%5Clarge%20N%28x%2C%5Cmu%20%2C%20%5CSigma%29%20%3D%20%5Cfrac%7B1%7D%7B%5Csqrt%7B%282%5Cpi%29%5E2%7C%5CSigma%7C%7D%7Dexp%5E%7B%28-%5Cfrac%7B1%7D%7B2%7D%28x-%5Cmu%20%29%20%5CSigma%5E%7B-1%7D%28x-%5Cmu%29%5ET%29%7D)

где, ![](http://latex.codecogs.com/gif.latex?%5Cdpi%7B120%7D%20%5Clarge%20%5Cmu%20%5Cin%20R%5En) - математическое ожидание(центр) ![](http://latex.codecogs.com/gif.latex?%5Cdpi%7B120%7D%20%5Clarge%20%5Csum%20%5Cin%20R%5E%7Bn%5Ctimes%20n%7D) - ковариационная матрица. Предполагается, что матрица ![](http://latex.codecogs.com/gif.latex?%5Cdpi%7B120%7D%20%5Csmall%20%5Csum) симметричная, невырожденная, положительно определённая.

Восстанавливая параметры нормального распределения ![](http://latex.codecogs.com/gif.latex?%5Cdpi%7B120%7D%20%5Csmall%20%5Cmu) , 
![](http://latex.codecogs.com/gif.latex?%5Cdpi%7B120%7D%20%5Csmall%20%5Csum) для каждого класса и подставляя 
в формулу оптимального байесовского классификатора восстановленные плотности, получим подстановочный (plug-in) алгоритм классификации
Параметры нормального распределения оценивают согласно принципа максимума правдоподобия: ![](http://latex.codecogs.com/gif.latex?%5Cdpi%7B120%7D%20%5Csmall%20%5Cmu_y%20%3D%20%5Cfrac%7B1%7D%7Bl_y%7D%5Csum_%7Bx_i%3Ay_i%7D%20x_i) , ![](http://latex.codecogs.com/gif.latex?%5Cdpi%7B120%7D%20%5Csmall%20%5Csum_y%20%3D%20%5Cfrac%7B1%7D%7Bl_y-1%7D%5Csum%20%28x_i%20-%20%5Cmu%20_y%29%28x_i%20-%20%5Cmu%20_y%29%5ET)

Пример работы подстановочного алгоритма можно посмотреть [здесь](https://pavlovajulia.shinyapps.io/plug-in/)

пример работы:  ![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/Plug_in.PNG)

## LDF

LDF(линейный дискриминант Фишера) - это частный случай алгоритма Plug-in. Основано на предположении Фишера, что ковариационные матрицы равны для всех классов. 

пример работы можно посмотреть [здесь](https://pavlovajulia.shinyapps.io/ldfisher/)

пример ![](https://github.com/PavlovaJulia/R_Projects/blob/master/lab2/LDF.PNG)

### Линейные классификаторы

Рассматривается задача с двумя классами *Y* = {-1, +1}

Модель алгоритмов выглядит следующим образом: 

![](http://latex.codecogs.com/gif.latex?%5Clarge%20a%28x%2Cw%29%20%3D%20sign%20f%28x%2Cw%29)

где *w* - вектор параметров. Функция *f(x,w)* называется дискриминантной функцией. 

Если *f(x,w)>0*, то алгоритм *а* относит объект *х* к класу +1, иначе к класу -1. Уравнение *f(x,w)=0* задает разделяющую поверхность. 

Велечина ![](http://latex.codecogs.com/gif.latex?%5Clarge%20M_i%28w%29%20%3D%20y_if%28x_i%2Cw%29) называется отступом объекта ![](http://latex.codecogs.com/gif.latex?%5Clarge%20x_i) 
относительно алгоритма классификации ![](http://latex.codecogs.com/gif.latex?%5Clarge%20a%28x%2Cw%29%20%3D%20sign%20f%28x%2Cw%29)

Если М < 0  то алгоритм допускает ошибку на объекте 1[](http://latex.codecogs.com/gif.latex?%5Clarge%20x_i).
Чем больше отступ тем правильнее и надежнее классификация объекта 1[](http://latex.codecogs.com/gif.latex?%5Clarge%20x_i).

Функция потерь *L(M)* - монотонная невозрастающая функция отступа, мажорирующая 
пороговую функцию потерь. Тогда минимизацию суммарных потерь можно рассматривать как приближенный метод 
минимизации эмпирического риска - числа ошибок на обучающей выборке:  

![](http://latex.codecogs.com/gif.latex?%5Clarge%20Q%28w%2CX%5Em%29%20%3D%20%5Csum%20_%7Bi-1%7D%5E%7Bm%7D%20L%28M_i%28w%29%29%5Crightarrow%20min)

Для минимизации *Q* будем использовать метод стохастического градиента. 

Задача состоит в том чтобы подобрать вектор параметров, и подобрать его таким образом чтобы минимизировать
*Q*. Для этого мы смотрим в каком направледнии быстрее всего увеличивается ошибка и будем идти в обратном направлении(для минимизации ошибки). Для этого мы берем случайным образом объект из выборки и вычисляем какая на нём ошибка, и в зависимости от этого изменять вектор параметров *w*,
далее пересчитывается *Q*, и в тот момент когда *Q* перестаёт изменяться и завершается процесс подбора параметров весов *w*.  
 
#### Алгоритм:

Вход: обучающая выборка, темп обучения, параметр сглаживания.

Выход: Веса *w1,...,wn*

1) Инициализировать веса *w1,...,wn*

2) Вычислить начальное значение функционала эмпирического риска 

3) повторять 

4) выбрать объект из обучающей выборки

5) вычислить ошибку алгоритма на функции потерь 

6) сделать шаг градиентного спуска

7) получить новое значение функционала эмпирического риска

8) пока значение функционала эмпирического риска не стабилизируются или веса не перестанут изменяться.

## ADALine



