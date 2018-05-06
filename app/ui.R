library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Brexit Referendum Survey"),
  
  #demo_group <- c("General Response","Country of Origin","Age","Country"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput("group_choice", "Breakdown by groups", c(names(dat)[c(2:19,22:28)]), selected = "country", multiple = FALSE,

                  selectize = TRUE, width = NULL, size = NULL)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       #plotOutput("plot")
       plotlyOutput("plot")

       #plotlyOutput("timeline_plot")

    )
  )
))
