
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
        radioButtons("radio", label = h3("분류 알고리즘 선택"),
                     choices = list("NB 알고리즘" = 'nb', "DT 알고리즘" = 'dt',
                                    "RM 알고리즘" = 'rm', "SVM 알고리즘" = 'svm'),selected = 'nb')
    ),

    # Show a plot of the generated distribution
    mainPanel(
        h4("Summary"),
        
        textOutput("summary")
    )
  )
))
