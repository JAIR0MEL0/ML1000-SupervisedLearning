## server.R

library(shiny)
library(ggplot2)

# We tweak the "am" field to have nicer factor labels. Since this doesn't
# rely on any user inputs we can do this once at startup and then use the
# value throughout the lifetime of the application

data=read.csv("./csv-files/ks-projects-201801.csv", header = TRUE, dec = ".")

#check # of rows
#Creation of the Binary result column
data[["successful"]] <- ifelse(data$state=="successful",'YES','NO')
data$successful <- factor(data$successful, labels = c('NO', 'YES'))
percentage <- prop.table(table(data$successful)) * 100

shinyServer(function(input, output) {
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  output$mpgPlot <- renderPlot({
    #barplot(percentage, ylim=c(0, 100), main="Success Rate", 
    #        xlab="Outcomes", col="red")
    pie(percentage, col=c(2, 3), ylim=c(0, 100), main="Success vs Failed")
  })  
})
