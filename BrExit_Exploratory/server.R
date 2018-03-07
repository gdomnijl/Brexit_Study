
# TODO: Read in data

library(tidyverse)
library(shiny)
#library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  factor_index <- reactive({grep(paste0("^",input$group_choice, "$"),colnames(plot_sub_group))})
   data <- reactive({
     if(input$dataset == "All Responses"){
       
      p<-plot_sub_group %>% 
        group_by(factor = plot_sub_group[,factor_index()],endtime,vote) %>%
        summarise(count = sum(wt, na.rm = TRUE))
     
      } else if(input$dataset == "Continuous Responses (all 13 waves)"){
    
      p<-dat_13 %>%
         group_by(factor = ins_dat[,factor_index()],endtime,vote) %>%
         summarise(count = sum(wt, na.rm = TRUE))
     }
     print(head(p))
     p
    })
  
  output$distPlot <- renderPlot({
    
    data() %>%
      filter(vote == "Leave the EU") %>%
      ggplot(aes(x = endtime,
                        y= count,
                        group = factor,
                        color = factor)) +
      geom_line() + geom_point() + coord_trans(y="log2")
  })
  # output$plot <- renderPlotly({
  #   ins_dat %>%
  #     group_by(endtime, vote)%>%
  #     summarise(count = sum(wt, na.rm = TRUE)) %>%
  #     plot_ly(x = ~endtime, y = ~count, type = 'scatter', mode = 'lines', linetype = ~vote) %>%
  #     layout(title = 'The Effect of Vitamin C on Tooth Growth in Guinea Pigs by Supplement Type',
  #            xaxis = list(title = 'Dose in milligrams/day'),
  #            yaxis = list (title = 'Tooth length'))
  # })
  # 
  # output$event <- renderPrint({
  #   d <- event_data("plotly_hover")
  #   if (is.null(d)) "Hover on a point!" else d
  # })
  
})
