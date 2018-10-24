library(shiny)
library(tidyverse)
library(plotly)

# acc <- read_csv("mkacc.csv", col_types = "TTicicttcccciiccccc") %>% as_tibble()
acc <- read_csv("mkacc.csv") %>% as_tibble()

theme_cycles <- 
  theme_minimal() +
  theme(legend.position = "top",
        axis.text = element_text(size=12),
        axis.title = element_text(size=16)
        )

shinyServer(function(input, output) {
  
  what_category <- reactive({
    x <- input$select_category
    return(x)
  })
  
  make_bar_plot <- function(focal_category) {
    
    pp <- ggplot(acc, aes_(x=as.name(focal_category), fill=~`Последица`)) +
      geom_bar() +
      scale_fill_brewer(palette = "Spectral") +
      theme_cycles +
      theme(axis.title.x = element_blank()) +
      labs(y="Број")
    
    how_many_levels <- acc %>% select_(~focal_category) %>% distinct %>% nrow
  
    if ( how_many_levels > 5) {
      pp <- pp + coord_flip()
    } 
    
    # pp2 <- ggplotly(pp)
    return(pp)
  }
  
  make_histogram <- function(focal_category) {
    
    pp <- ggplot(acc, aes_(x=as.name(focal_category), group=~`Последица`, fill=~`Последица`)) +
      geom_histogram() +
      scale_fill_brewer(palette = "Spectral") +
      theme_cycles +
      labs(y="Број")
    
    return(pp)
  }
  
  what_type_of_plot <- reactive({
    focal_category <- what_category()
    
    class_focal <- class(focal_category) %>% unique
    
    if (class_focal %in% c("character", "factor")) {
      xx <- make_bar_plot(focal_category)
    }
    
    if (class_focal %in% c("numeric", "integer", "hms", "difftime", "POSIXct", "POSIXt")) {
      xx <- make_histogram(focal_category)
    }
        
    return(xx)
  })
    
  
  
  # output$bar_plot <- renderPlotly({
  #   make_bar_plot()
  #   
  # })

  output$bar_plot <- renderPlot({
    what_type_of_plot()
    
  })
  
  output$table_data <- renderDataTable({
    acc
  })
  
})
