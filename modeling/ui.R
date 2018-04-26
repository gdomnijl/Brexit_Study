library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Brexit Referendum Survey"),
  
  #demo_group <- c("General Response","Country of Origin","Age","Country"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput("group_choice", "Breakdown by different demographic group", names(dat)[4:28], selected = "country", multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
