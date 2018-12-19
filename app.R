library(shiny)
library(MASS)

ui <- fluidPage(
  titlePanel("plug-in алгоритм"),
  
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

N <- function(x, m, matrix){ 
  #плотность
  (1/(sqrt((2*pi)^2*det(matrix))))* exp((-1/2)*((x-m)%*%solve(matrix)%*%t(x-m)))
} 

munew <- function(xm){
  colMeans(xm)
}

sigmanew <- function(xm, mu){
  sumsigma <- 0
  for(i in 1:nrow(xm)){
    tmp <- matrix(c(xm[i,1], xm[i,2]),1,2)
    sumsigma <- sumsigma + t(tmp-mu)%*%(tmp-mu)
  }
  sigma <- sumsigma/(nrow(xm)-1)
  return(sigma)
}


server <- function(input, output) {
  output$distPlot <- renderPlot({
    
  P <- 0.5
  
  lyamda <- as.numeric(c(input$lyamda1, input$lyamda2))
  m <- input$point # количество точек
  mu <- matrix(as.numeric(c(input$mu11, input$mu21,input$mu12,input$mu22)), 2, 2)
  sigma <- matrix(as.numeric(c(input$sigma11, input$sigma21,input$sigma12,input$sigma22)), 2, 2)
  
  covmat1 <- matrix(as.numeric(c(input$sigma11, 0, 0, input$sigma12)), 2, 2)
  covmat2 <- matrix(as.numeric(c(input$sigma21, 0, 0, input$sigma22)), 2, 2)                 
  
  xm <- data.frame(matrix(0, m, 3))
  # заполняем нормальным распределением 
  xm[1:(m/2),1:2] <- mvrnorm(m/2, mu[1,], covmat1)
  xm[(m/2+1):m,1:2] <- mvrnorm(m/2, mu[2,], covmat2)
  xm[1:(m/2),3] <- c("первый")
  xm[(m/2+1):m,3] <- c("второй")

  mu1 <- matrix(munew(xm[1:(m/2),1:2]),1,2)
  mu2 <- matrix(munew(xm[(m/2+1):m,1:2]),1,2)
  sigma1 <- matrix(sigmanew(xm[1:(m/2),c(1,2)], mu1),2,2)
  sigma2 <- matrix(sigmanew(xm[(m/2+1):m,c(1,2)],mu2),2,2)
   print(sigma1)
   print(sigma2)
   
  # коэфициенты 
  Ainv1 <- solve(sigma1)
  Ainv2 <- solve(sigma2)
  
  sqrx1 <- Ainv1[1,1]-Ainv2[1,1]
  sqrx2 <- Ainv1[2,2]-Ainv2[2,2]
  x1x2 <- 2*Ainv1[1,2]- 2*Ainv2[1,2]
  c <- log(det(sigma1)) - log(det(sigma2)) + Ainv1[1,1]*mu1[1,1]^2 - Ainv2[1,1]*mu2[1,1]^2 + Ainv1[2,2]*mu1[1,2]^2 - Ainv2[2,2]*mu2[1,2]^2 + 2*Ainv1[1,2]*mu1[1,2]*mu1[1,1]- 2*Ainv2[1,2]*mu2[1,1]*mu2[1,2] 
  x1 <- -2*Ainv1[1,1]*mu1[1,1] + 2*Ainv2[1,1]*mu2[1,1] - 2*Ainv1[1,2]*mu1[1,2] + 2*Ainv2[1,2]*mu2[1,2]
  x2 <- -2*Ainv1[2,2]*mu1[1,2] + 2*Ainv2[2,2]*mu2[1,2] - 2*Ainv1[1,2]*mu1[1,1] + 2*Ainv2[1,2]*mu2[1,1]
  
  x <- seq(-2, 15, length.out = 100)
  y <- seq(-2, 15, length.out = 100)
  
  line <- matrix(0, length(x), length(y))
   for(i in 1:length(x)){
     for(j in 1:length(y)){
       line[i,j] <- sqrx1*x[i]^2+ sqrx2*y[j]^2+ x1x2*x[i]*y[j]+ c + x1*x[i]+ x2*y[j] 
     }
   }
  
   colnames(xm) <- c("первый признак","второй признак", "класс")
   colors <- c("первый" = "violet", "второй" = "green")
   plot(xm[,1:2], pch = 21, col = colors[xm[,3]], bg = colors[xm[,3]], asp = 1)
   contour(x, y, line, levels = 0, add = T)
  
  
  })
}

shinyApp(ui, server)