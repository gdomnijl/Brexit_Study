
# TODO: Read in data

library(tidyverse)
library(shiny)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  factor_index <- reactive({grep(paste0("^",input$group_choice, "$"),colnames(dat))})
   data <- reactive({
      p<-dat %>% 
        select(c(factor_index(),31,32))
    })
   col <- reactive({
     data() %>%
       select(factor_index())
   })
  
  # output$distPlot <- renderPlot({
  #   data() %>%
  #     filter(!voter_type %in% c("0.Don't know", "1.Don't know") & !is.na(voter_type))%>%
  #     ggplot(aes(x = voter_type, y = immig_index5)) + geom_boxplot() + 
  #     facet_grid(paste0(input$group_choice,"~ .")) + 
  #     scale_x_discrete(labels=c("Hardcore Stay", "Ambivalent Stay", "Ambivalent Leave", "Hardcore Leave"))
  # })
  output$plot <- renderPlotly({
    data() %>%
      filter(!voter_type %in% c("0.Don't know", "1.Don't know") & !is.na(voter_type))%>%
      plot_ly(x = ~voter_type, y = ~immig_index5, type = 'box') %>%
      add_trace(x = ~voter_type, y = ~immig_index5) %>%
      layout(title = 'The Effect of Vitamin C on Tooth Growth in Guinea Pigs by Supplement Type',
             xaxis = list(title = 'Dose in milligrams/day'),
             yaxis = list (title = 'Tooth length'))
  })

  # output$event <- renderPrint({
  #   d <- event_data("plotly_hover")
  #   if (is.null(d)) "Hover on a point!" else d
  # })
  
})
