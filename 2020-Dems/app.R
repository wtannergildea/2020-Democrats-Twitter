
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

  tabPanel("Intro",
  
  fluidPage(
   
   # Application title
   titlePanel("The Story of the 2020 Democratic Primary, as Told Through Twitter"),
   
   p(paste("A record number of Democrats are seeking their party's nomination for the Presidency in 
           2020. Presumptive front-runners Joe Biden and Bernie Sanders aren't scaring away the competition: the 
           field includes  former and sitting U.S. senators, representatives, governors, 
           a mayor, cabinet secretaries, an author, and a tech executive.")),
   
   br(),
   
   p(paste("The success of any presidential campaign hinges on the candidate's ability to connect with voters 
            on the ground, especially in the early primary and caucus states of Iowa, New Hampshire, South Carolina, and Nevada.")),
   
   br(),
   
   p(paste("As proven by the current President, however, campaigns are also waged online - especially on Twitter.
           According, the 2020 Democratic primary candidates have all taken to the platform to spread their message.")),
   
   br(),
   
   p(paste("But how has each candidate used Twitter? How frequently do they tweet? How are their tweets received?
            What language do they use, what's the tone of their writing, and what issues are they talking about most?")),
   
   br(),
   
   p(paste("Using Twitter's API, I tried to answer these questions. For my analysis, I picked twelve of the most popular
           candidates, both in terms of polling and fundraising. Hope you enjoy the results!")),

   
      # Show a plot of the generated distribution
      mainPanel(
        # insert here eventually
      )
   )
),

####################################
# SUMMARY STATS
####################################

tabPanel("Summary Statistics",
         
         fluidPage(
           
         # Application title
           titlePanel("Summary of Twitter Activity"),
           
           sidebarLayout(
             sidebarPanel(
               sliderInput("bins",
                           "# of Bins:",
                           min = 25,
                           max = 50,
                           value = 30), 
               width = 2),
               
                 
              # Show a plot of the generated distribution
              mainPanel(
                
                "Here you can see the candidates' Twitter activity since the beginning of 2019.",
                
                br(),
                
                plotlyOutput("tweet_freq"),
                
                br(),
                br(),
                br(),
                
                p(paste("The table below presents a summary of each candidates' Twitter activity, 
                including their total number of tweets, the average length of their tweets, 
                and the average number of favorites and retweets each tweet receives.")),
                
                DTOutput("summary_table")

         )))),

####################################
# SENTIMENT ANALYSIS
####################################

tabPanel("Sentiment Analysis",
         
         fluidPage(
           
           # Application title
           titlePanel("Sentiment Analysis"),
           
           # Sidebar with a slider input for number of bins 
           # sidebarLayout(
          
          # Allows the user to toggle between types of lexicons.
             
             #   sidebarPanel(
             #     selectInput("tweet_sentiments", "Choose Sentiment:",
             #                 c("Average Positivity: Afinn",
             #                   "Positivity and Negativity: Bing",
             #                   "Positivity and Negativity: NRC"),
             #                 "Average Positivity: Afinn") # sets default
             # ),
             
             # Show a plot of the generated distribution
             # mainPanel(
               #put in the plotlyOutput function here
               
               plotlyOutput("afinn"),
               
               br(),
               
               plotlyOutput("nrc"),
               
               br(),
               
               plotlyOutput("bing")
               
               # text description of the visualization above
               
             )
           ),

####################################
# KEY WORDS
####################################

tabPanel("Individual Key Words",
         
         fluidPage(
           
           # Application title
           titlePanel("Key Words"),
           
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
         )),

############
# END CREDITS
#############

tabPanel("Footnotes",
         
         fluidPage(
           
           # Application title
           titlePanel("Acknowledgements"),
           
           p(paste("Although I worked on this project alone, I could not have done it alone.")),
           
           br(),
           
           p(paste("I'd like to thank Michael Galarnyk, who wrote an excellent Medium article
                   explaing how to access Twitter's API using R.
                   https://medium.com/@GalarnykMichael/accessing-data-from-twitter-api-using-r-part1-b387a1c7d3e ")),
           
           br(),
           
           p(paste("I'd like to thank my friend Max Weiss, whose analysis of Trump's tweets was instrumental in developing
                  my own project and codebase.")),
           
           br(),
           
           p(paste("And finally, I give my full gratitude to David Kane (Preceptor), the head of Gov:1005 (Data) at Harvard and the
                  rope that protected me from the sirens.")),
           
           # Show a plot of the generated distribution
           mainPanel(
             # insert here eventually
           )
         )
)

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
       options = list(dom = 't')
    
   )
   
   ####################################
   # TEXT SENTIMENT ANALYSIS PAGE
   ####################################
   
   

   output$afinn <- renderPlotly(
       
    # AFINN OUTPUT
     
     ggplot(afinn_tweets, aes(x = screenName, y = average_positivity, fill = screenName)) +
              geom_bar(stat = "identity") +
       labs(title = "Average Positivity of Tweets",
            subtitle = "Per Afinn",
            y = "Average Positivity Rating") +
       theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank())
     )
     
     
    # NRC OUTPUT
     
    output$nrc <- renderPlotly({
      
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
   
   output$bing <- renderPlotly({

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

