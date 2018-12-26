library(shiny)


ui <- fluidPage(
   
      titlePanel("Байесовский наивный классификатор"),
   
   sidebarLayout(
      sidebarPanel(
         fluidRow(
           column(4, textInput("lyamda1", "лямбда1", 1)), column(4,textInput("lyamda2","лямбда2", 1) ),
           column(8, sliderInput("point", "количество точек", 2, 100, 50, 2)),
           column(6, textInput("mu11", "мат. ожидание1(1)", 9)), column(6, textInput("mu12", "мат. ожидание1(2)", 7)),
           column(6, textInput("mu21", "мат. ожидание2(1)", 10)), column(6, textInput("mu22", "мат. ожидание2(2)", 8)),
           column(6, textInput("sigma11", "дисперсия1(1)", 0.5)), column(6, textInput("sigma12", "дисперсия1(2)", 0.5)),
           column(6, textInput("sigma21", "дисперсия2(1)", 0.5)), column(6, textInput("sigma22", "дисперсия2(2)", 0.5))
         )
      ),
      
      mainPanel(
         plotOutput("distPlot")
      )
   )
) 

plotnosti <- function(point, mu, sigma){
  p <- (1/(sigma*sqrt(2*pi)))* exp(-(point-mu)^2/(2*sigma^2))
  return (p)
}

baesclas <- function(lyamda, P, point, xm, mu, sigma){
  baes <- rep(0,length(unique(xm[,3])))
  names(baes) <- unique(xm[,3])
  for(i in 1:length(unique(xm[,3]))){
    baes[i] <- log(lyamda[i]*P)
    sum <- 0
      for(j in 1: (ncol(xm)-1)) { # идем по каждому признаку

        p <- plotnosti(point[j], mu[i,j], sigma[i,j]) # плотность для каждого класса и признака
        
        sum <- sum + log(p) 
      }
    baes[i] <- baes[i] + sum
  }  
  
  names(which.max(baes))
} 

munew <- function(xm){
  summu <- 0
  for(i in 1:length(xm)){
    summu <- summu + xm[i] 
  }
  mu <- summu/length(xm)
  return (mu)
}

sigmanew <- function(xm, mu){
  sumsigma <- 0
  for(i in 1:length(xm)){
    sumsigma <- sumsigma + (xm[i]-mu)^2
  }
  sigma <- sumsigma/(length(xm)-1)
  return(sigma)
}

server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     P <- 0.5
     
     lyamda <- as.numeric(c(input$lyamda1, input$lyamda2))
     m <- input$point # количество точек
     mu <- matrix(as.numeric(c(input$mu11, input$mu21,input$mu12,input$mu22)), 2, 2)
     sigma <- matrix(as.numeric(c(input$sigma11, input$sigma21,input$sigma12,input$sigma22)), 2, 2)
     
     xm <- data.frame(matrix(0, m, 3))
     # заполняем нормальным распределением(одномерное) 
     xm[1:(m/2),1] <- rnorm(m/2, mu[1,1], sigma[1,1])
     xm[(m/2+1):m,1] <- rnorm(m/2, mu[2,1], sigma[2,1])
     
     for(i in 1:(m/2)){
       xm[i,2] <- xm[i,1] #*3/50
     }
       for(j in (m/2+1):m){
        xm[j,2] <- xm[j,1]#^2/70  
     }
     xm[1:(m/2),2] <- rnorm(m/2, mu[1,2], sigma[1,2])
     xm[(m/2+1):m,2] <- rnorm(m/2, mu[2,2], sigma[2,2])
     xm[1:(m/2),3] <- c("первый")
     xm[(m/2+1):m,3] <- c("второй")
     
     mu <- matrix(c(munew(xm[1:m/2,1]), munew(xm[(m/2+1):m,1]), munew(xm[1:(m/2),2]), munew(xm[(m/2+1):m,2])),2,2)
     sigma <- matrix(c(sigmanew(xm[1:m/2,1],mu[1,1]), sigmanew(xm[(m/2+1):m,1],mu[2,1]), sigmanew(xm[1:(m/2),2], mu[1,2]), sigmanew(xm[(m/2+1):m,2],mu[2,2])),2,2)
     
     grafic <- c()
     for(i in seq(-1, 13, 0.1))
       for(j in seq(-1, 10 , 0.1))
         grafic <- rbind(grafic, c(i, j, baesclas(lyamda, P, c(i,j), xm, mu, sigma)))
     colnames(xm) <- c("крестик","нолик", "класс")
     colors <- c("первый" = "violet", "второй" = "green")
     plot(xm[,1:2], pch = 21, col = colors[xm[,3]], bg = colors[xm[,3]], asp = 1) # рисуем выборку
     points(grafic[,1:2], pch = 21, col = colors[grafic[,3]]) # рисуем точки
   })
}

shinyApp(ui = ui, server = server)

