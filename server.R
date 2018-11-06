library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)
source("gg_plot.R")
source("statistics.R")


shinyServer(function(input, output, session) {
  
  my_data <- readRDS("my_data.Rda")
  levels <- c("root", "tld", "auth", "cname", "final")
  my_data$level <- factor(my_data$level, levels=levels)
  numeric_cols <- sapply(my_data, is.numeric)
  numeric_cols_names <- colnames(my_data[, numeric_cols])
  possible_dimensions <- setdiff(colnames(my_data), numeric_cols_names)

  output$table <- renderDataTable({                     
    return(my_data) 
  })
  
  output$statistics <- renderDataTable({
    dimensions <- setdiff(c(input$firstDimension,
                            input$secondDimension,
                            input$thirdDimension),
                          c("NS"))
    filters <- list(list("dimension"=input$firstDimension,
                         "filter"=input$firstDimFilter))
    if(input$secondDimension != "NS") {
      filter_2 <- list("dimension"=input$secondDimension,
                       "filter"=input$secondDimFilter)
      filters <- c(filters, list(filter_2))
    }
    if(input$thirdDimension != "NS") {
      filter_3 <- list("dimension"=input$thirdDimension,
                       "filter"=input$thirdDimFilter)
      filters <- c(filters, list(filter_3))
    }
      return(statistics(my_data, dimensions, input$selectedMeasurements, filters))
  })
  
  output$firstDimension <- renderUI({
    radioButtons("firstDimension", "First dimension:",
                 choiceNames = possible_dimensions,
                 choiceValues = possible_dimensions,
                 inline = TRUE)
  })
  
  output$firstDimFilter <- renderUI({
    dimension <- input$firstDimension
    filtered_values <- unique(my_data[,dimension])
    selectInput("firstDimFilter", "Filter:",
                filtered_values,
                selected = filtered_values,
                multiple = TRUE)
  })
  
  output$secondDimension <- renderUI({
    second_dim_possibles <- setdiff(possible_dimensions, input$firstDimension)
    radioButtons("secondDimension", "Second dimension:",
                 choiceNames = c("NS",second_dim_possibles),
                 choiceValues = c("NS",second_dim_possibles),
                 inline = TRUE)
  })
  
  output$secondDimFilter <- renderUI({
    if(input$secondDimension != "NS") {
      dimension <- input$secondDimension
      filtered_values <- unique(my_data[,dimension])
      selectInput("secondDimFilter", "Filter:",
                  filtered_values,
                  selected = filtered_values,
                  multiple = TRUE)
    }
  })
  
  output$thirdDimension <- renderUI({
    third_dim_possibles <- setdiff(possible_dimensions, c(input$firstDimension,
                                                          input$secondDimension))
    radioButtons("thirdDimension", "Third dimension:",
                 choiceNames = c("NS",third_dim_possibles),
                 choiceValues = c("NS",third_dim_possibles),
                 inline = TRUE)
  })
  
  output$thirdDimFilter <- renderUI({
    if(input$thirdDimension != "NS") {
      dimension <- input$thirdDimension
      filtered_values <- unique(my_data[,dimension])
      selectInput("thirdDimFilter", "Filter:",
                  filtered_values,
                  selected = filtered_values,
                  multiple = TRUE)
    }
  })
  
  output$measurements <- renderUI({
    radioButtons("selectedMeasurements", "Measurements:",
                       choiceNames = numeric_cols_names,
                       choiceValues = numeric_cols_names,
                       inline = TRUE)
  })
  
  output$plotbox <- renderUI({
    if(input$usePlotly) box(width = NULL, plotlyOutput("gg_plot_ly"))
    else box(width = NULL, plotOutput("gg_plot"))
    
  })
  
  my_plot <- function() {
    dimensions <- setdiff(c(input$firstDimension,
                            input$secondDimension,
                            input$thirdDimension),
                          c("NS"))
    filters <- list(list("dimension"=input$firstDimension,
                         "filter"=input$firstDimFilter))
    if(input$secondDimension != "NS") {
      filter_2 <- list("dimension"=input$secondDimension,
                       "filter"=input$secondDimFilter)
      filters <- c(filters, list(filter_2))
    }
    if(input$thirdDimension != "NS") {
      filter_3 <- list("dimension"=input$thirdDimension,
                       "filter"=input$thirdDimFilter)
      filters <- c(filters, list(filter_3))
      
    }
    gg_plot_gen(my_data, dimensions, input$selectedMeasurements, filters)
  }
  
  output$gg_plot <- renderPlot({my_plot()})
  
  output$gg_plot_ly <- renderPlotly({my_plot()})
})                                                  
