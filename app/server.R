library(tidyverse)
library(shiny)
library(plotly)
# TODO: Read in data
#timeline <-read.csv("../data/timeline_plot_vote.csv")

# ref_time <- as.Date(as.POSIXct("2016-06-23"))
# ref_time2 <- as.Date(as.POSIXct("2016-06-24"))
# 
# timeline$endtime <-as.Date(timeline$endtime)

#dat <- read.csv("../data/bi_voter_type_data.csv")

# dat$profile_gross_personal <- factor(dat$profile_gross_personal, levels = c("under £5,000 per year",
#                                                                             "£5,000 to £9,999 per year",
#                                                                             "£10,000 to £14,999 per year",
#                                                                             "£15,000 to £19,999 per year",
#                                                                             "£25,000 to £29,999 per year",
#                                                                             "£30,000 to £34,999 per year",
#                                                                             "£40,000 to £44,999 per year",
#                                                                             "£45,000 to £49,999 per year",
#                                                                             "£50,000 to £59,999 per year",
#                                                                             "£60,000 to £69,999 per year",
#                                                                             "£70,000 to £99,999 per year",
#                                                                             "£100,000 and over", "Don't know"))
# dat$profile_gross_household <- factor(dat$profile_gross_household, levels = c("under £5,000 per year",
#                                                                             "£5,000 to £9,999 per year",
#                                                                             "£10,000 to £14,999 per year",
#                                                                             "£15,000 to £19,999 per year",
#                                                                             "£25,000 to £29,999 per year",
#                                                                             "£30,000 to £34,999 per year",
#                                                                             "£40,000 to £44,999 per year",
#                                                                             "£45,000 to £49,999 per year",
#                                                                             "£50,000 to £59,999 per year",
#                                                                             "£60,000 to £69,999 per year",
#                                                                             "£70,000 to £99,999 per year",
# 
#                                                                             "£100,000 and £149,999 per year",
#                                                                             "£150,000 and over",
#                                                                             "Don't know",
#                                                                             "Prefer not to answer"))


# Define server logic required to draw a histogram
shinyServer(function(input, output){
  # for plotting immig_index vs voter_type
  # bi_dat <- read.csv("data/Final_relvl_bi_voter_type.csv") %>%
  #   select(-profile_ethnicity,-profile_newspaper, -profile_religion,-profile_religion_denom)
  # bi_dat$bi_voter_type <- factor(bi_dat$bi_voter_type, levels = c("0.Stay/remain in the EU",
  #                                                           "1.Stay/remain in the EU",
  #                                                           "1.Leave the EU",
  #                                                           "0.Leave the EU",
  #                                                           "0.Don't know",
  #                                                           "1.Don't know"))
  factor_index <- reactive({grep(paste0("^",input$group_choice, "$"),names(bi_dat))})
  y_index <- reactive({grep(paste0("^", input$y, "$"), names(bi_dat))}) 
  data <- reactive({
   #  if(input$group_choice != "none"){
      d<-bi_dat %>% 
        select(c(factor_index(),17,y_index())) %>%
        filter(!bi_voter_type %in% c("0.Don't know", "1.Don't know") & !is.na(bi_voter_type))

    # }else{
    #   dat %>% 
    #     select(c(31,32)) %>%
    #     filter(!voter_type %in% c("0.Don't know", "1.Don't know") & !is.na(voter_type))
    # }
   })
  
   ct<-bi_dat %>%
     select(bi_voter_type) %>%
     filter(!bi_voter_type %in% c("0.Don't know", "1.Don't know") & !is.na(bi_voter_type))%>%
     group_by(bi_voter_type) %>%
     summarise(count = n())

   
  # for plotting vote_ratio vs timeline
   
   # raw_p <-ins_dat %>% 
   #   select(endtime, vote, wt) %>%
   #   group_by(endtime,vote) %>%
   #   mutate(count = sum(wt,na.rm = TRUE)) %>%
   #   ungroup() %>%
   #   group_by(endtime) %>%
   #   mutate(total_count = sum(wt, na.rm = TRUE)) %>%
   #   mutate(ratio = count / total_count)
   
   # factor_index2 <- reactive({grep(paste0("^",input$group_choice, "$"),colnames(timeline))})
   # timeline_data <- reactive({ 
   #   t<-timeline %>% 
   #   select(endtime, vote, wt, factor_index2()) %>%
   #   group_by(endtime,vote, factor_index2()) %>%
   #   mutate(count = sum(wt,na.rm = TRUE)) %>%
   #   ungroup() %>%
   #   group_by(endtime, factor_index2()) %>%
   #   mutate(total_count = sum(wt, na.rm = TRUE)) %>%
   #   mutate(ratio = count / total_count)
   # })
   
   

  # output$plot <- renderPlot({
  #   data() %>%
  #     filter(!voter_type %in% c("0.Don't know", "1.Don't know") & !is.na(voter_type))%>%
  #     ggplot(aes(x = voter_type, y = immig_index5)) + geom_boxplot() + 
  #     facet_grid(paste0(input$group_choice,"~ .")) + 
  #     scale_x_discrete(labels=c("Hardcore Stay", "Ambivalent Stay", "Ambivalent Leave", "Hardcore Leave"))
  # })#paste0("~",input$group_choice)
  output$plot <- renderPlotly({
    if(input$group_choice == "none"){
        p<-ggplot(data(), aes_string(x = "bi_voter_type", y = input$y, fill = "bi_voter_type")) + 
          geom_boxplot() + 
          scale_x_discrete(labels=c("Always-Stay", "Switched-Stay", "Switched-Leave", "Always-Leave"))+
          annotate("text", x = 1:4, y = -0.1, label = c(as.character(ct$count)))+
          scale_fill_manual(values=c("#2c7fb8","#7fcdbb","#fdae6b","#e6550d"),
                            name="Voter Type",
                            labels=c("Always Stay", "Switched-Stay", 
                                     "Switched-Leave", "Always Leave"))
        
        ggplotly(p) %>%
          layout(height = input$plotHeight, autosize=TRUE) %>%
          # plot_ly(data(), y = ~immig_index5, x = ~voter_type, color = paste0("~",input$group_choice), type = "box") %>%
          layout(boxmode = "group")
    }else{
      p<-ggplot(data(), aes_string(x = "bi_voter_type", y = input$y, color = input$group_choice)) + 
        geom_boxplot() + 
        scale_x_discrete(labels=c("Always-Stay", "Switched-Stay", "Switched-Leave", "Always-Leave"))+
        #annotate("text", x = 1:4, y = 0, label = c(as.character(ct$count)))
      
      ggplotly(p) %>%
        layout(height = input$plotHeight, autosize=TRUE) %>%
        # plot_ly(data(), y = ~immig_index5, x = ~voter_type, color = paste0("~",input$group_choice), type = "box") %>%
        layout(boxmode = "group")
    }
      })
    #}else{
      # p <- ggplot(data(), aes_string(x = "voter_type", y = "immig_index5")) + 
      #   geom_boxplot()
    
    # if at least one facet column/row is specified, add it
    # facets <- paste(input$facet_row, '~', input$facet_col)
    # if (facets != '. ~ .') p <- p + facet_grid(facets)

  
  #START
  # output$timeline_plot <- renderPlotly({
  # 
  #   p2 <- ggplot(timeline_data(), aes_string(x = "endtime", y= "ratio",
  #                                      #group = input$group_choice,
  #       group = paste0("interaction(vote,", input$group_choice, ")"),
  #       colour = input$group_choice)) + geom_line() + geom_point() +
  #           annotate("segment", x = ref_time, xend = ref_time, y = 0, yend = 0.8,
  #                    colour = "black") +
  #           annotate("text", x = ref_time, y = 0.9, label = "Referendum")
  # 
  #   ggplotly(p2) %>%
  #     layout(autosize=TRUE) %>%
  #     # plot_ly(data(), y = ~immig_index5, x = ~voter_type, color = paste0("~",input$group_choice), type = "box") %>%
  #     layout(boxmode = "group")
  #   })

    # ggplot(aes(x = endtime, y= ratio, group = interaction(vote, rl_countryOfBirth),
    #            colour =  rl_countryOfBirth)) + geom_line() + geom_point() +
    # annotate("segment", x = ref_time, xend = ref_time, y = 0, yend = 0.8,
    #          colour = "black") +
    # annotate("text", x = ref_time, y = 0.9, label = "Referendum")
})
