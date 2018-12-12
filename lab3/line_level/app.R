library(shiny)

ui <- fluidPage(
  
  titlePanel("Линии уровня"),
  
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12, sliderInput("a", "а", -100, 100, 1, 1)),
        column(12,sliderInput("b12", "b1,b2", -100, 100, 0, 1)),
        column(12,sliderInput("c", "c", -100, 100, 1, 1)),
        column(12,sliderInput("p", "шаг плотности распределения", 0, 1, 0.01, 0.01))
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
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    matrix <- matrix(c(input$a, input$b12, input$b12, input$c), 2, 2)
    m <- matrix(0, 1, 2) # мат ожидание
    if(det(matrix)>0){
      print(matrix)
      # границы графика
      x1 <- matrix[1,1]+1
      x2 <- -matrix[1,1]-1
      y1 <- matrix[2,2]+1
      y2 <- -matrix[2,2]-1
      
      # координаты точек с подбираемым шагом
      x <- seq(x2, x1, length.out = 100)
      y <- seq(y2, y1, length.out = 100)
      
      p <- matrix(0, length(x), length(y)) # матрица плотностей
      
      for(i in 1: length(x)){
        for(j in 1: length(y)){
          point <- c(x[i], y[j])
          p[i,j] <- N(point, m, matrix) # для каждой точки находим плотность
        }
      }
      
      grafic <- F 
      for(i in seq(0.001, 0.1, input$p)){
        contour(x, y, p, levels = i, add = grafic, asp = 1)
        grafic  = T # добавление новых элипсов к старым
      } 
    }
  
    
  })
}

shinyApp(ui = ui, server = server)

