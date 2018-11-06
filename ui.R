library(shiny)
library(shinydashboard)
library(plotly)

# Data Box
data_box <-  box( width = NULL, 
                  status = "primary", solidHeader = TRUE, title="Data",                
                  br(),br(),
                  dataTableOutput("table"))

# Input Box
input_box <- box(
  width = 4,
  solidHeader = TRUE,
  title = "Inputs", status = "warning",
  "Select at least one dimension (maximum of 3), and one measurement", 
  br(),
  br(),
  checkboxInput("usePlotly", "Use plotly", value = FALSE, width = NULL),
  br(),
  uiOutput("firstDimension"),
  uiOutput("firstDimFilter"),
  br(),
  uiOutput("secondDimension"),
  uiOutput("secondDimFilter"),
  br(),
  uiOutput("thirdDimension"),
  uiOutput("thirdDimFilter"),
  br(),
  uiOutput("measurements")

)

# Statistics Box

statistics_box <- box(width = 8,
                   status = "primary", solidHeader = TRUE, title="Statistics",                
                   br(),br(),
                   dataTableOutput("statistics"))



# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Plot", tabName = "plot", icon = icon("line-chart")),
    menuItem("Data", tabName = "data", icon = icon("database"))
  )
)

# Body

body <- dashboardBody(tabItems(
  tabItem(tabName = "data",
          data_box),
  tabItem(tabName = "plot",
          fluidRow(
            width = NULL,
            uiOutput("plotbox")
          ),
          fluidRow(
            input_box,
            statistics_box
          )
       )
  )
)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "DNS Analyzer"),
  sidebar,
  body
)