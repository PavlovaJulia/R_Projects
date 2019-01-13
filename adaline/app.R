library(shiny)
library(MASS)

ui <- fluidPage(
  titlePanel("Линейные классификаторы"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(8, sliderInput("point", "количество точек(четное)", 2, 100, 50, 2)),
        column(6, textInput("mu11", "мат. ожидание1(1)", 9)), column(6, textInput("mu12", "мат. ожидание1(2)", 7)),
        column(6, textInput("mu21", "мат. ожидание2(1)", 12)), column(6, textInput("mu22", "мат. ожидание2(2)", 8)),
        column(6, textInput("sigma11", "дисперсия1(1)", 0.5)), column(6, textInput("sigma12", "дисперсия1(2)", 0.5)),
        column(6, textInput("sigma21", "дисперсия2(1)", 0.5)), column(6, textInput("sigma22", "дисперсия2(2)", 0.5))
      )
    ),
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

LAdaline <- function(M){
  (1-M)^2
}

LXebba <- function(M){
  max(-M,0)
}

Llog <- function(M){
  log2(1 + exp(-M))
}

trainingSampleNormalization <- function(xm) {   
  n <- dim(xm)[2] - 1     
  for(i in 1:n){    
    xm[, i] <- (xm[, i] - mean(xm[, i])) / sd(xm[, i])  
  }   
  return (xm)
} 

gradientLog <- function(xm, ntta, lyamda){
  
  w <- matrix(runif(3,-1/(2*(ncol(xm)-1)),1/(2*(ncol(xm)-1))),1,3)
  sumQ <- 0 # Q
  cnt <- 0 # номер итерации
  for(i in 1:nrow(xm)){
    M <- c(w[1,] %*% xm[i,-4])* xm[i,4]
    sumQ <- sumQ + Llog(M)
  }
  while(TRUE){
    
    cnt <- cnt+1 
    xmi <- sample(c(1:nrow(xm)), 1)
    M <- c(w[cnt,] %*% xm[xmi,-4]) * xm[xmi,4]
    Qi <- Llog(M)
    # print(xm[xmi,-4])
    # break
    ntta <- 1/sqrt(sum(xm[xmi,-4]*xm[xmi,-4])) # пересчет шага
    
    w <- rbind(w,w[cnt,] + ntta*(1 / (1+exp(-(-M))))* xm[xmi,4]* xm[xmi,-4])
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
  return(w)
}

gradientXebba <- function(xm, ntta, lyamda){
  
  w <- matrix(0.5,1,3)
  sumQ <- 0 # Q
  cnt <- 0 # номер итерации
  for(i in 1:nrow(xm)){
    M <- c(w[1,] %*% xm[i,-4])* xm[i,4]
    sumQ <- sumQ + LXebba(M)
  }
  while(TRUE){
    
    cnt <- cnt+1
    M1 <- c()
    for(i in 1:nrow(xm)){
      M1 <- c(M1, c(w[cnt,] %*% xm[i,-4]) * xm[i,4])
    }
    Mminys <- which(M1<0)
    if(length(Mminys)==0) break
    xmi <- sample(Mminys, 1)
    M <- c(w[cnt,] %*% xm[xmi,-4]) * xm[xmi,4]
    Qi <- LXebba(M)
    ntta <- 1/cnt # пересчет шага
    w <- rbind(w,w[cnt,] + ntta*xm[xmi,4] * xm[xmi,-4])
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
  return(w)
}

gradientAdaline <- function(xm, ntta, lyamda){
  
  w <- matrix(0.5,1,3)
  sumQ <- 0 # Q
  cnt <- 0 # номер итерации
  for(i in 1:nrow(xm)){
    M <- c(w[1,] %*% xm[i,-4])* xm[i,4]
    sumQ <- sumQ + LAdaline(M)
  }
  while(TRUE){
    
    cnt <- cnt+1 
    xmi <- sample(c(1:nrow(xm)), 1)
    M <- c(w[cnt,] %*% xm[xmi,-4]) * xm[xmi,4]
    Qi <- LAdaline(M)
    ntta <- 1/cnt # пересчет шага
    
    w <- rbind(w,w[cnt,] - ntta*c((w[cnt,] %*% xm[xmi,-4]  - xm[xmi,4])) * xm[xmi,-4])
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
    xm <- cbind(xm[,1:2], -1, xm[,3]) 
    
    wAdaline <- gradientAdaline(xm, ntta, lyamda)
    wXebba <- gradientXebba(xm, ntta, lyamda)
    wLog <- gradientLog(xm, ntta, lyamda)
    
    colnames(xm) <- c("первый признак","второй признак","", "")
    #print(xm)
    colors <- c("violet", "", "green")
    plot(xm[,1:2], type = "n", asp = 1)
    # for(i in 1:(nrow(wAdaline)-1)){
    #   abline(a = wAdaline[i,3] / wAdaline[i,2], b = -wAdaline[i,1] / wAdaline[i,2], lwd = 1, col = "black")
    # }
    abline(a = wAdaline[nrow(wAdaline),3] / wAdaline[nrow(wAdaline),2], b = -wAdaline[nrow(wAdaline),1] / wAdaline[nrow(wAdaline),2], lwd = 3, col = "blue")

    # for(i in 1:(nrow(wLog)-1)){
    #   abline(a = wLog[i,3] / wLog[i,2], b = -wLog[i,1] / wLog[i,2], lwd = 1, col = "black")
    # }
    abline(a = wLog[nrow(wLog),3] / wLog[nrow(wLog),2], b = -wLog[nrow(wLog),1] / wLog[nrow(wLog),2], lwd = 3, col = "orange")
    
    #for(i in 1:(nrow(wXebba)-1)){
     # abline(a = wXebba[i,3] / wXebba[i,2], b = -wXebba[i,1] / wXebba[i,2], lwd = 1, col = "red")
    #}
    abline(a = wXebba[nrow(wXebba),3] / wXebba[nrow(wXebba),2], b = -wXebba[nrow(wXebba),1] / wXebba[nrow(wXebba),2], lwd = 3, col = "red")
    points(xm[,1:2], pch = 21, col = colors[xm[,4]+2], bg = colors[xm[,4]+2])
    
    legend("bottomleft", c("адалаин","логистическая регрессия","правило Хэбба"), pch = c("l","l","l"), col = c("blue", "orange", "red"))
  })
}

shinyApp(ui, server)