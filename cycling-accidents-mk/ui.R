# Ова е интерактивна аппликација за преглед на податоци за велосипедски незгоди во Македонија
# Податоците се...

library(shiny)
library(shinythemes)
library(tidyverse)
library(stringr)

acc <- read_csv("acc.csv") %>% as_tibble()


# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("flatly"),
                  
  # Application title
  titlePanel("Велосипедски незгоди"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width=3,
       selectInput(inputId = "select_category", 
                   label = "Преглед на незгоди. Избери категорија: ",
                   choices = colnames(acc),
                   selected = colnames(acc)[1])
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotlyOutput("bar_plot", height = 800),
       tags$hr(),
       dataTableOutput("table_data")
    )
  )
))
