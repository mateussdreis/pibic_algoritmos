library(shiny)
library(stabledist)
ui<-fluidPage(
  titlePanel("Gráfico interativo de uma estável"),
  sidebarLayout(
    sidebarPanel(h2("Parâmetros"),
                 sliderInput("sliderId1","alpha", min = 0, max = 2, value = 1, step = 0.2),
                 sliderInput("sliderId2","beta", min = -1, max = 1, value = 0, step = 0.2),
                 sliderInput("sliderId3","sigma", min = 0, max = 10, value = 5),
                 sliderInput("sliderId4","mu", min = -2, max = 2, value = 0, step = 0.2)
    ),
    mainPanel(
      plotOutput("graficoHist")
    )
    
  )
  
)

server<-function(input,output){
  output$graficoHist<- renderPlot({curve(dstable(x, alpha=input$sliderId1, beta =  input$sliderId2, gamma = input$sliderId3, delta = input$sliderId4),-15,15,col=2, ylab = "valor",xlab = "coordenada")})#permite atualização do gráfico sempre que receber novas entradas
}
shinyApp(ui,server)




