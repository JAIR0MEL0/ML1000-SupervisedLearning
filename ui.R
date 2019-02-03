## ui.R

library(shiny)

shinyUI(fluidPage(

  # Application title
  headerPanel("Kickstarter Classification"),

  # Sidebar with a slider input for number of bins
  sidebarPanel(),
    #selectInput("variable", "Variable:",
    #            list("Cylinders" = "cyl", 
    #                 "Transmission" = "am", 
    #                 "Gears" = "gear")),
    
    #checkboxInput("outliers", "Show outliers", FALSE)
  
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("mpgPlot")
  )
))
