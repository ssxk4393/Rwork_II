# server.R


# 함수에서 사용할 라이브러리, 데이터 셋 메모리 로딩과정.
library(shiny)
library(stringr)
data(iris) # 데이터 셋 로드 
data("airquality")
# data = read.csv(file= '경로명/파일명', header=T)

shinyServer(function(input, output) {

  # 데이터 셋 사용  
  output$summary <- renderPrint({
    dataset <- iris[-5]
    summary(dataset)
  })
  
})

