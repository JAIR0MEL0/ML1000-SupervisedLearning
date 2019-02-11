#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("caret")
library("ggplot2")
library("randomForest")
KS_data = read.csv('./ks_final_data.csv', header = TRUE)
KS_model = readRDS('./ks_model')

percentage <- prop.table(table(KS_data$state)) * 100

# Define UI for application
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type="text/css", href="styles.css"),
    tags$script(src="prediction-handler.js")
  ),
  
  fluidRow(
    column(12,
           h1("Kickstarter Key Metrics", class="centered-header"),
           h4("How successful are their ideas ?", class="centered-header")
    ),
    column(12,
           h4("Reality Check", class="centered-header"),
           plotOutput("successPie")
    )
  ),
  
  
  fluidRow(
    column(12,
           h4("Nobody can be certain about success, but here we place some key metrics that can help you with the big picture.", class="centered-header header-explanation")
    ),
    column(6,
           h3("Backers"),
           plotOutput("barBackers")
    ),
    
    column(6,
           h3("Goal in USD"),
           plotOutput("usdGoal")
    )
  ),
  
  fluidRow(
    column(6,
           h3("Amount of projects by category"),
           plotOutput("barCategory")
    ),
    
    column(6,
           h3("Amount of projects by country"),
           plotOutput("barCountry")
    )
  ),
  
  fluidRow(
    column(12, 
           h3("Live Prediction", class="centered-header"),
           h4("Choose your project options:", class="centered-header")
    )
  ),
  
  # Sidebar with a slider input for number of bins
  fluidRow(
    column(6,
           numericInput(inputId = "goal",
                        label = "Goal in USD",
                        value = 1000),
           numericInput(inputId = "backers",
                        label = "Number of Backers",
                        value = 10),
           numericInput(inputId = "days_on_deck",
                        label = "Days of Campaign",
                        value = 10),
           selectInput(inputId = "country",
                       label = "Choose your country:",
                       choices = c(
                         "Austria", 
                         "Australia",
                         "Belgium", 
                         "Canada", 
                         "Switzerland", 
                         "Germany", 
                         "Denmark", 
                         "Spain", 
                         "France", 
                         "United Kingdom", 
                         "Hong Kong",
                         "Ireland",
                         "Italy",
                         "Japan",
                         "Luxemburg",
                         "Mexico",
                         "Netherlands",
                         "Normay",
                         "New Zeland",
                         "Sweden",
                         "Singapore",
                         "United States"
                       )
           ),
           selectInput(inputId = "category",
                       label = "Choose your Cateogory:",
                       choices = c(
                         "Art",
                         "Comics",
                         "Crafts",
                         "Dance",
                         "Design",
                         "Fashion",
                         "Film & Video",
                         "Food",        
                         "Games",
                         "Journalism",
                         "Music",
                         "Photography",
                         "Publishing",
                         "Technology",
                         "Theater" 
                       )
           )            
    ),
    column(6,
           actionButton("predictButton", "Predict", class="predict-button")
    )
  )   
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$successPie <- renderPlot({
    #pie(percentage, col=c(2, 3), ylim=c(0, 100))
    successSummary <- summary(KS_data$state)
    pieData <- data.frame(
      titles = names(successSummary),
      values = prop.table(successSummary) * 100
    )
    ggplot(pieData, aes(x="", y=values, fill=titles)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0)
  })
  
  # Plot backers
  output$barBackers <- renderPlot({
    ggplot(data=KS_data[c("backers")],aes(x=backers))+geom_histogram(breaks=seq(0,2000,100),col="black",fill='red')
  })
  
  # Plot USD goal
  output$usdGoal <- renderPlot({
    ggplot(data=KS_data[c("usd_goal_real")],aes(x=usd_goal_real))+geom_histogram(breaks=seq(0,200000,1000),col="black",fill='green')
  })
  
  output$barCountry <- renderPlot({
    ggplot(data.frame(KS_data), aes(x=country)) + geom_bar()
  })
  
  output$barCategory <- renderPlot({
    ggplot(data.frame(KS_data), aes(x=main_category)) + geom_bar(col='black',fill='blue') + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  })
  
  selectedBackers <- renderText({ input$backers })
  selectedGoal <- renderText({ input$goal })
  selectedDaysOnDeck <- renderText({ input$days_on_deck })
  selectedCountry <- reactive({
    switch(input$country,
           "Austria" = "AT",
           "Australia" = "AU",
           "Belgium" = "BE",
           "Canada" = "CA",
           "Switzerland" = "CH",
           "Germany" = "DE",
           "Denmark" = "DK",
           "Spain" = "ES",
           "France" = "FR",
           "United Kingdom" = "GB",
           "Hong Kong" = "HK",
           "Ireland" = "IE",
           "Italy" = "IT",
           "Japan" = "JP",
           "Luxemburg" = "LX",
           "Mexico" = "MX",
           "Netherlands" = "NL",
           "Normay" = "NO",
           "New Zeland" = "NZ",
           "Sweden" = "SE",
           "Singapore" = "SP", 
           "United States" = "US"
    )
  })
  
  selectedCategory <- reactive({
    switch(input$category,
           "Art" = "Art",
           "Comics" = "Comics",
           "Crafts" = "Crafts",
           "Dance" = "Dance",
           "Design" = "Design",
           "Fashion" = "Fashion",
           "Film & Video" = "Film & Video",
           "Food" = "Food",
           "Games" = "Games",
           "Journalism" = "Journalism",
           "Music" = "Music",
           "Photography" = "Photography",
           "Publishing" = "Publishing",
           "Technology" = "Technology",
           "Theater" = "Theater"
    )
  })
  
  observeEvent(input$predictButton, {
    userChoice <- data.frame(
      main_category = as.factor(selectedCategory()),
      backers = as.integer(selectedBackers()),
      country = as.factor(selectedCountry()),
      usd_goal_real = as.numeric(selectedGoal()),
      days_on_deck = as.integer(selectedDaysOnDeck())
    )
    
    prediction <- predict(KS_model, userChoice)
    session$sendCustomMessage(type = 'predictionResult', message = prediction)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
