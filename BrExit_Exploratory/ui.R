library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Brexit Referendum Survey"),
  
  #demo_group <- c("General Response","Country of Origin","Age","Country"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput("group_choice", "Breakdown by different demographic group", names(plot_sub_group)[5:15], selected = "countryOfBirth", multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
      selectInput("dataset", "Sample Selection", c("All Responses", "Continuous Responses (all 13 waves)"), 
                  selected = "All Responses" )#"Continuous Responses (all 13 waves)")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
