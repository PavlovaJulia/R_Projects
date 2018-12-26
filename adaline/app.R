library(shiny)
library(MASS)

ui <- fluidPage(
  titlePanel("ADAline"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(8, sliderInput("point", "количество точек(четное)", 2, 100, 50, 2)),
        column(6, textInput("mu11", "мат. ожидание1(1)", 9)), column(6, textInput("mu12", "мат. ожидание1(2)", 7)),
        column(6, textInput("sigma11", "дисперсия1(1)", 0.5)), column(6, textInput("sigma12", "дисперсия1(2)", 0.5)),
        column(6, textInput("sigma21", "дисперсия2(1)", 0.5)), column(6, textInput("sigma22", "дисперсия2(2)", 0.5))
      )
    ),
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

L <- function(M){
  (1-M)^2
}

trainingSampleNormalization <- function(xm) {   
  n <- dim(xm)[2] - 1     
  for(i in 1:n){    
    xm[, i] <- (xm[, i] - mean(xm[, i])) / sd(xm[, i])  
  }   
  return (xm)
} 

gradient <- function(xm, ntta, lyamda){
  w <- c(0.5, 0.5, 0.5)
  sumQ <- 0 # Q
  cnt <- 0 # номер итерации
  for(i in 1:nrow(xm)){
    M <- c(w %*% xm[i,-4])* xm[i,4]
    sumQ <- sumQ + L(M)
  }
  while(TRUE){
    
    cnt <- cnt+1 
    xmi <- sample(c(1:nrow(xm)), 1)
    M <- c(w %*% xm[xmi,-4]) * xm[xmi,4]
    Qi <- L(M)
    ntta <- 1/cnt # пересчет шага
    
    w <- w - ntta*c((w %*% xm[xmi,-4]  - xm[xmi,4])) * xm[xmi,-4]
    Q <- sumQ
    sumQ <- (1-lyamda)*sumQ + lyamda*Qi 
    if(cnt > 20000) {
      print("вышли по счетчику")
      break
    }
    if (abs(Q-sumQ)/max(Q,sumQ)<0.0001) {
      print("вышли по q")
      break
    }
  }
  print(w)
  return(w)
}

server <- function(input, output) {
  output$distPlot <- renderPlot({

    m <- input$point # количество точек
    mu <- matrix(as.numeric(c(input$mu11, input$mu21,input$mu12,input$mu22)), 2, 2)
    
    covmat1 <- matrix(as.numeric(c(input$sigma11, 0, 0, input$sigma12)), 2, 2)
    covmat2 <- matrix(as.numeric(c(input$sigma21, 0, 0, input$sigma22)), 2, 2)                 
    
    xm <- matrix(0, m, 3)
    # заполняем нормальным распределением 
    xm[1:(m/2),1:2] <- mvrnorm(m/2, mu[1,], covmat1)
    xm[(m/2+1):m,1:2] <- mvrnorm(m/2, mu[2,], covmat2)
    xm[1:(m/2),3] <- c(1)
    xm[(m/2+1):m,3] <- c(-1)
    
    lyamda <- 1/6
    ntta <- 1
    
    x <- seq(-2, 15, length.out = 100)
    y <- seq(-2, 15, length.out = 100)
    
    line <- matrix(0, length(x), length(y))
    xm <- trainingSampleNormalization(xm)
    print(xm)
    xm <- cbind(xm[,1:2], -1, xm[,3]) 
    w <- gradient(xm, ntta, lyamda)
    for(i in 1:length(x)){
      for(j in 1:length(y)){
        line[i,j] <- w[1]*x[i]+ w[2]*y[j]+w[3]
      }
    }
    
    #colnames(xm) <- c("первый признак","второй признак", "класс")
    colors <- c("violet", "", "green")
    plot(xm[,1:2], pch = 21, col = colors[xm[,4]+2], bg = colors[xm[,4]+2], asp = 1)
    contour(x, y, line, levels = 0, add = T)
    
    
  })
}

shinyApp(ui, server)