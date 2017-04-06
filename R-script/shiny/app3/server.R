# server.R

library(shiny)

shinyServer(function(input, output) {
  
  sliderValues <- reactive({
    
    data.frame(
      Parameter = "Integer",
      Value = as.character(input$integer))
  }) 
  
  # HTML table 양식으로 값을 넘겨준다.
  output$values <- renderTable({ 
    sliderValues() 
  })
})
