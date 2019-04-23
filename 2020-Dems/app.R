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

cleaned_tweets <- read_rds("cleaned_tweets")
afinn_tweets <- read_rds("afinn_tweets")
nrc_tweets <- read_rds("nrc_tweets")
bing_tweets <- read_rds("bing_tweets")

# Define UI for application that draws a histogram
ui <- navbarPage("2020 Dems on Twitter", theme = shinytheme("darkly"),
            
# HOME PAGE 

  tabPanel("Home",
  
  fluidPage(
   
   # Application title
   titlePanel("2020 Democratic Presidential Candidates' Twitter Activity"),
   
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
         plotlyOutput("tweet_freq")
      )
   )
)),

# TAB TWO

tabPanel("Summary Statistics",
         
         fluidPage(
           
           # Application title
           titlePanel("2020 Democratic Presidential Candidates' Twitter Activity"),
           
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
               #put in the plotlyOutput function here
               
             )
           )
         )),

tabPanel("Sentiment Analysis",
         
         fluidPage(
           
           # Application title
           titlePanel("Sentiment Analysis"),
           
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
               #put in the plotlyOutput function here
               
               plotlyOutput("tweet_sentiments_afinn"),
               
               br(),
               
               plotlyOutput("tweet_sentiments_nrc"),
               
               br(),
               
               plotlyOutput("tweet_sentiments_bing")
               
               # text description of the visualization above
               
             )
           )
         )),

tabPanel("Individual Key Words",
         
         fluidPage(
           
           # Application title
           titlePanel("Sentiment Analysis"),
           
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
               
               
               
             )
           )
         ))



)





# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # Tweet frequency histogram
   output$tweet_freq <- renderPlotly({
     ggplot(cleaned_tweets, aes(x = created, fill = screenName)) +
       geom_histogram(
         position = "identity", show.legend = FALSE) +
       facet_wrap(~ screenName, nrow = 2) +
       
       labs(title = "2020 Democratic Challengers' Tweet Activity",
            subtitle = "Frequency of Tweets in 2019",
            caption = "Source: Twitter") +
       xlab("Date") +
       ylab("Frequency") +
       theme_fivethirtyeight()
   })
   
   
   # TEXT SENTIMENT ANALYSIS PAGE
   
   # AFINN OUTPUT
   output$tweet_sentiments_afinn <- renderPlotly({
     
     ggplot(afinn_tweets, aes(x = screenName, y = average_positivity, fill = screenName)) +
              geom_bar(stat = "identity") +
       labs(title = "Average Positivity of Tweets",
            subtitle = "Per Afinn",
            y = "Average Positivity Rating") +
       theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank())
  })
   # NRC OUTPUT
   output$tweet_sentiments_nrc <- renderPlotly({
     
     ggplot(nrc_tweets, aes(x = screenName, y = n, fill = sentiment)) +
       geom_bar(stat = "identity") +
       labs(title = "Number of Positive and Negative Tweets",
            subtitle = "Per NRC",
            y = "Count") +
       theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank())
   })
   
   # BING OUTPUT
   output$tweet_sentiments_bing <- renderPlotly({
     
     ggplot(bing_tweets, aes(x = screenName, y = n, fill = sentiment)) +
       geom_bar(stat = "identity") +
       labs(title = "Number of Positive and Negative Tweets",
            subtitle = "Per BING",
            y = "Count") +
       theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank())
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

