library(shiny)
library(tidyverse)

PlotsUI<-function(id){
  ns<-NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4,
                  actionButton(inputId = ns("plotBtn"), label = "Press to plot data"),
                  selectInput(inputId = ns("plotTypeInput"), choices = c("Scatterplot", "Boxplot", "Densityplot"), label = "Choose your plot", selected = "Scatterplot"),
                  selectInput(inputId = ns("X_variable"),
                              choices = as.vector(colnames(iris)),
                              label = "Choose X variable",
                              selected = NULL),
                  selectInput(inputId = ns("Y_variable"),
                              choices = as.vector(colnames(iris)),
                              label = "Choose Y variable",
                              selected = NULL),
                  selectInput(inputId = ns("color"),
                              choices = as.vector(colnames(iris)),
                              label = "Color by",
                              selected = NULL)
      ),
      mainPanel(
        plotOutput(ns("Plot"))
      )
    )
  )
}

Plots<-function(input, output, session){
  
  plotType <- reactive({
    z <- switch(input$plotTypeInput,
                Scatterplot = geom_point(aes_string(x = input$X_variable, y = input$Y_variable, color = input$color)),
                Boxplot = geom_boxplot(aes_string(x = input$X_variable, y = input$Y_variable)),
                Densityplot = geom_density(aes_string(x = input$X_variable, color = input$color))
                )
  })
  
  observe({
    if(input$plotTypeInput=="Scatterplot"){
      updateSelectInput(session, inputId = "color", label = "Color by", choices = colnames(iris))
      updateSelectInput(session, inputId = "Y_variable", label = "Choose Y variable", choices = colnames(iris)) 
    } else if (input$plotTypeInput=="Boxplot"){
      updateSelectInput(session, inputId = "color", label = "Color by", choices = c(""))
      updateSelectInput(session, inputId = "Y_variable", label = "Choose Y variable", choices = colnames(iris))
    } else if (input$plotTypeInput=="Densityplot"){
      updateSelectInput(session, inputId = "color", label = "Color by", choices = colnames(iris))
      updateSelectInput(session, inputId = "Y_variable", label = "Choose Y variable", choices = c(""))
    }
  })
  
  r<-eventReactive(input$plotBtn,{
    ggplot(iris) + plotType()
  })
  
  output$Plot <- renderPlot({
    r()
  })
}

ui<-fluidPage(
  PlotsUI("1")
)

server <- function(input, output){
  callModule(Plots, "1")
}

shinyApp(ui, server)