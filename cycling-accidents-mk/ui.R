# Ова е интерактивна аппликација за преглед на податоци за велосипедски незгоди во Македонија
# Податоците се...

library(shiny)
library(shinythemes)
library(tidyverse)
library(stringr)
library(plotly)

acc <- read_csv("mkacc.csv") %>% as_tibble()


# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("flatly"),
                  
  # Application title
  titlePanel("Велосипедски незгоди"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width=3,
       selectInput(inputId = "select_category", 
                   label = "Преглед на незгоди. Избери категорија: ",
                   choices = colnames(acc)[1:(dim(acc)[2]-1)],
                   selected = colnames(acc)[5])
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       # plotlyOutput("bar_plot", height = 800),
       plotOutput("bar_plot", height = 800),
       tags$hr(),
       dataTableOutput("table_data")
    )
  )
))
