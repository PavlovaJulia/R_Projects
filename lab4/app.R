library(shiny)


ui <- fluidPage(
   
      titlePanel("Байесовский наивный классификатор"),
   
   sidebarLayout(
      sidebarPanel(
         fluidRow(
           column(4, textInput("lyamda1", "лямбда1", 1)), column(4,textInput("lyamda2","лямбда2", 1) ),
           column(8, sliderInput("point", "количество точек(четное)", 2, 100, 50, 2)),
           column(6, textInput("mu11", "мат. ожидание1(1)", 9)), column(6, textInput("mu12", "мат. ожидание1(2)", 7)),
           column(6, textInput("mu21", "мат. ожидание2(1)", 10)), column(6, textInput("mu22", "мат. ожидание2(2)", 8)),
           column(6, textInput("sigma11", "дисперсия1(1)", 0.5)), column(6, textInput("sigma12", "дисперсия1(2)", 0.5)),
           column(6, textInput("sigma21", "дисперсия2(1)", 0.5)), column(6, textInput("sigma22", "дисперсия2(1)", 0.5))
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

baesclas <- function(lyamda, P, point, norm, mu, sigma){
  baes <- rep(0,ncol(norm)-1)
  names(baes) <- unique(norm[,3])
  for(i in 1:length(unique(norm[,3]))){
    baes[i] <- log(lyamda[i]*P)
    sum <- 0
      for(j in 1: (ncol(norm)-1)) {

        
        p <- plotnosti(point[j], mu[i,j], sigma[i,j])
        
        sum <- sum + log(p)
      }
    baes[i] <- baes[i] + sum
  }  
  
  names(which.max(baes))
} 

server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     P <- 0.5
     lyamda <- as.numeric(c(input$lyamda1, input$lyamda2))
     m <- input$point
     mu <- matrix(as.numeric(c(input$mu11, input$mu21,input$mu12,input$mu22)), 2, 2)
    
     sigma <- matrix(as.numeric(c(input$sigma11, input$sigma21,input$sigma12,input$sigma22)), 2, 2)
     
     norm <- matrix(0, m, 3)
     norm[1:(m/2),1] <- rnorm(m/2, mu[1,1], sigma[1,1])
     norm[(m/2+1):m,1] <- rnorm(m/2, mu[2,1], sigma[2,1])
     norm[1:(m/2),2] <- rnorm(m/2, mu[1,2], sigma[1,2])
     norm[(m/2+1):m,2] <- rnorm(m/2, mu[2,2], sigma[2,2])
     norm[1:(m/2),3] <- c("Анечка")
     norm[(m/2+1):m,3] <- c("Юлечка")
     
     grafic <- c()
     for(i in seq(1, 15, 0.1))
       for(j in seq(1, 15, 0.1))
         grafic <- rbind(grafic, c(i, j, baesclas(lyamda, P, c(i,j), norm, mu, sigma)))
     colors <- c("Анечка" = "violet", "Юлечка" = "green")
     plot(norm[,1:2], pch = 21, col = colors[norm[,3]], bg = colors[norm[,3]], asp = 1)
     points(grafic[,1:2], pch = 21, col = colors[norm[,3]])
   })
}

shinyApp(ui = ui, server = server)

