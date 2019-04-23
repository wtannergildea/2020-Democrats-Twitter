
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
library(shinyjs)
library(DT)

cleaned_tweets <- read_rds("cleaned_tweets")
afinn_tweets <- read_rds("afinn_tweets")
nrc_tweets <- read_rds("nrc_tweets")
bing_tweets <- read_rds("bing_tweets")

summary_table <- read_rds("clean_summary_table") %>% 
  rename("Handle" = "screenName", 
         "# of Tweets in 2019" = "tweet_count", 
         "Average Tweet Length in Characters" = "mean_tweet_length", 
         "Average Favorites Per Tweet" = "fav_average", 
         "Average Retweets Per Tweet" = "rt_average") 
  

# Define UI for application that draws a histogram
ui <- navbarPage("2020 Dems on Twitter", theme = shinytheme("readable"),
                 
####################################                 
# HOME PAGE 
####################################

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

####################################
# SUMMARY STATS
####################################

tabPanel("Summary Statistics",
         
         fluidPage(
           
           # Application title
           titlePanel("2020 Democratic Presidential Candidates' Twitter Activity"),
           
             
             # Show a plot of the generated distribution
             mainPanel(
               #put in the plotlyOutput function here
               DTOutput("summary_table")
               
             )
           )
         ),

####################################
# SENTIMENT ANALYSIS
####################################

tabPanel("Sentiment Analysis",
         
         fluidPage(
           
           # Application title
           titlePanel("Sentiment Analysis"),
           
           # Sidebar with a slider input for number of bins 
           sidebarLayout(
          
          # Allows the user to toggle between types of lexicons.
             
               sidebarPanel(
                 selectInput("tweet_sentiments", "Choose Sentiment:", 
                             c("Average Positivity: Afinn",
                               "Positivity and Negativity: Bing",
                               "Positivity and Negativity: NRC"),
                             "Average Positivity: Afinn") # sets default
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               #put in the plotlyOutput function here
               
               plotlyOutput("tweet_sentiments")
               
               # text description of the visualization above
               
             )
           )
         )),

####################################
# KEY WORDS
####################################

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

  ####################################
  # SUMMARY STATS
  ####################################
  
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
       
       theme_fivethirtyeight() +
  
    # For connection to the bin sidebar
       
       geom_histogram(bins = input$bins) 
     
   })
   
  # Summary table
     
     output$summary_table <- renderDT(
       
       summary_table,
       class = 'display', 
       rownames = FALSE,
       caption = 'Summary Statistics',
       options = list(dom = 'ft')
    
   )
   
   ####################################
   # TEXT SENTIMENT ANALYSIS PAGE
   ####################################
   
   

   output$tweet_sentiments <- renderPlotly({

     # AFINN OUTPUT
     
     if (input$tweet_sentiments == "Average Positivity: Afinn") {
       
     ggplot(afinn_tweets, aes(x = screenName, y = average_positivity, fill = screenName)) +
              geom_bar(stat = "identity") +
       labs(title = "Average Positivity of Tweets",
            subtitle = "Per Afinn",
            y = "Average Positivity Rating") +
       theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank())
     }
     
     else if (input$tweet_sentiments == "Positivity and Negativity: NRC") {

   # NRC OUTPUT
    
     ggplot(nrc_tweets, aes(x = screenName, y = n, fill = sentiment)) +
       geom_bar(stat = "identity") +
       labs(title = "Number of Positive and Negative Tweets",
            subtitle = "Per NRC",
            y = "Count") +
       theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank())
     }
     
     else if (input$tweet_sentiments == "Positivity and Negativity: Bing") {
       
   # BING OUTPUT

     ggplot(bing_tweets, aes(x = screenName, y = n, fill = sentiment)) +
       geom_bar(stat = "identity") +
       labs(title = "Number of Positive and Negative Tweets",
            subtitle = "Per BING",
            y = "Count") +
       theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank())
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

