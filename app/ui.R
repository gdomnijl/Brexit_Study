library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Brexit Referendum Survey"),
  
  fluidRow(
    
    column(4,
           wellPanel(
             selectInput("group_choice", "Breakdown by groups", 
                         c("none", names(bi_dat)[c(2:11,13,18:28)]), 
                         selected = "none", multiple = FALSE,
                         selectize = TRUE, width = NULL, size = NULL),
             selectInput("y", "Anti-Immigration Measure", 
                         c("none", names(bi_dat)[c(14:16)]), 
                         selected = "immig_index5", multiple = FALSE,
                         selectize = TRUE, width = NULL, size = NULL)
           )       
    )),
   
  plotlyOutput("plot")
  ))
    
  
  #demo_group <- c("General Response","Country of Origin","Age","Country"),
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     sidebarPanel(
#       
#       selectInput("group_choice", "Breakdown by groups", c(names(dat)[c(2:19,22:28)]), selected = "country", multiple = FALSE,
# 
#                   selectize = TRUE, width = NULL, size = NULL)
#       
#     ),
#     
#     # Show a plot of the generated distribution
#     mainPanel(
#        #plotOutput("plot")
#        plotlyOutput("plot")
# 
#        #plotlyOutput("timeline_plot")
# 
#     )
#   )
# ))
