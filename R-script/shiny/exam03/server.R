
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
weatherAUS = read.csv('c:/NCS/Rwork_II/data/weatherAUS.csv', header = T)
weatherAUS = weatherAUS[ ,c(-1,-2, -22, -23)]

set.seed(415)
idx = sample(1:nrow(weatherAUS), 0.7*nrow(weatherAUS))
training_w = weatherAUS[idx, ]
testing_w  = weatherAUS[-idx, ]

training_w = na.omit(training_w)
testing_w = na.omit(testing_w)


shinyServer(function(input, output) {

    data <- reactive(input$radio)
    data <- as(data,'charator')
    output$summary <- renderPrint(class(data))
    # if(data=='nb'){
    #     output$summary <- renderPrint({
    # 
    #         library(e1071)
    #         model_nb <- naiveBayes(RainTomorrow~., data=training_w)
    #         pred_nb <- predict(model_nb, testing_w)
    #         t <- table(pred_nb, testing_w$RainTomorrow)
    #         sum(diag(t))/nrow(testing_w)
    #     })
    # 
    # }else if(data=='dt'){
    # 
    #     output$summary <- renderPrint({
    # 
    #         library(rpart)
    #         model_dt <- rpart(RainTomorrow~., data=training_w)
    #         pred_dt <- predict(model_dt, testing_w, type="class")
    #         t <- table(pred_dt, testing_w$RainTomorrow)
    #         sum(diag(t))/nrow(testing_w)
    #     })
    # 
    # }else if(data=='rm'){
    #     output$summary <- renderPrint({
    #         library(randomForest)
    #         model_rm <- randomForest(RainTomorrow~., data=training_w, mtree=500, mtry=4, na.action=na.omit)
    #         pred_rm <- predict(model_rm, testing_w)
    #         t <-table(pred_rm, testing_w$RainTomorrow)
    #         sum(diag(t))/nrow(testing_w)
    #     })
    # 
    # }else{
    #     output$summary <- renderPrint({
    #         library(randomForest)
    #         model_r <- svm(RainTomorrow~., data=training_w, kernel='radial')
    #         pred_r <- predict(model_r, testing_w)
    #         t_r <- table(pred_r, testing_w$RainTomorrow)
    #     })
    # }
    

})
