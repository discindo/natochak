library(shiny)
library(tidyverse)
library(plotly)

acc <- read_csv("acc.csv") %>% as_tibble()

theme_cycles <- 
  theme_minimal() +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=16))

shinyServer(function(input, output) {
  
  what_category <- reactive({
    x <- input$select_category
    return(x)
  })
  
  make_bar_plot <- reactive ({
    focal_category <- what_category()
    
    pp <- ggplot(acc, aes_(x=as.name(focal_category), fill=~Outcome)) +
      geom_bar() +
      coord_flip() +
      scale_fill_brewer(palette = "Spectral") +
      theme_cycles
    
    pp2 <- ggplotly(pp)
    return(pp2)
  })
  
  output$bar_plot <- renderPlotly({
    make_bar_plot()
    
  })
  
  output$table_data <- renderDataTable({
    acc
  })
  
})
