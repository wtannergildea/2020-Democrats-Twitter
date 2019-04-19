#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(twitteR)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(shinythemes)
library(tidytext)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("2020 Democratic Presidential Candidates"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlotly({
     ggplot(tweets, aes(x = created, fill = screenName)) +
       geom_histogram(
         position = "identity", bins = 50, show.legend = FALSE) +
       facet_wrap(~ screenName, nrow = 2) +
       
       labs(title = "2020 Democratic Challengers' Tweet Activity",
            subtitle = "Frequency of Tweets in 2019",
            caption = "Source: Twitter") +
       xlab("Date") +
       ylab("Frequency") +
       theme_fivethirtyeight()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

