
# TODO: Read in data
dat <- read.csv("../data/voter_type_data.csv")
dat$voter_type <- factor(dat$voter_type, levels = c("0.Stay/remain in the EU", 
                                                    "1.Stay/remain in the EU", 
                                                    "1.Leave the EU",
                                                    "0.Leave the EU",
                                                    "0.Don't know",
                                                    "1.Don't know"))
dat$headHouseholdPast<-plyr::revalue(as.factor(dat$headHouseholdPast), 
                                     c("1"="My father", "2"="My mother", 
                                       "3" = "Someone else",
                                       "4" = "No one in my house worked",
                                       "9999" = "Don't know"))
library(tidyverse)
library(shiny)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output){

  factor_index <- reactive({grep(paste0("^",input$group_choice, "$"),colnames(dat))})
   data <- reactive({
   #  if(input$group_choice != "none"){
      dat %>% 
        select(c(factor_index(),31,32)) %>%
        filter(!voter_type %in% c("0.Don't know", "1.Don't know") & !is.na(voter_type))
    # }else{
    #   dat %>% 
    #     select(c(31,32)) %>%
    #     filter(!voter_type %in% c("0.Don't know", "1.Don't know") & !is.na(voter_type))
    # }
   })
  
   ct<-dat %>%
     select(voter_type) %>%
     filter(!voter_type %in% c("0.Don't know", "1.Don't know") & !is.na(voter_type))%>%
     group_by(voter_type) %>%
     summarise(count = n())

  # output$plot <- renderPlot({
  #   data() %>%
  #     filter(!voter_type %in% c("0.Don't know", "1.Don't know") & !is.na(voter_type))%>%
  #     ggplot(aes(x = voter_type, y = immig_index5)) + geom_boxplot() + 
  #     facet_grid(paste0(input$group_choice,"~ .")) + 
  #     scale_x_discrete(labels=c("Hardcore Stay", "Ambivalent Stay", "Ambivalent Leave", "Hardcore Leave"))
  # })#paste0("~",input$group_choice)
  output$plot <- renderPlotly({
        p<-ggplot(data(), aes_string(x = "voter_type", y = "immig_index5", color = input$group_choice)) + 
      geom_boxplot() + 
          scale_x_discrete(labels=c("Always-Stay", "Switched-Stay", "Switched-Leave", "Always-Leave"))+
          annotate("text", x = 1:4, y = -0.1, label = c(as.character(ct$count)))
        ggplotly(p) %>% 
          layout(height = input$plotHeight, autosize=TRUE) %>%
          # plot_ly(data(), y = ~immig_index5, x = ~voter_type, color = paste0("~",input$group_choice), type = "box") %>%
          layout(boxmode = "group")
        
      })
    #}else{
      # p <- ggplot(data(), aes_string(x = "voter_type", y = "immig_index5")) + 
      #   geom_boxplot()
    
    # if at least one facet column/row is specified, add it
    # facets <- paste(input$facet_row, '~', input$facet_col)
    # if (facets != '. ~ .') p <- p + facet_grid(facets)

  
})
