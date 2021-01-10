# Even though loaded in the script, still necessary to load libraries here too.

library(shiny)
library(twitteR)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(shinythemes)
library(tidytext)
library(plotly)
library(DT)
data("stop_words")

####################################                 
# LOAD DATA
####################################

cleaned_tweets <- read_rds("cleaned_tweets")
afinn_tweets <- read_rds("afinn_tweets")
nrc_tweets <- read_rds("nrc_tweets")
bing_tweets <- read_rds("bing_tweets")
word_freq <- read_rds("word_frequency")

# For word freq by candidate section, if necessary in the future (unused now)
# kamala_words <- read_rds("kamala_words")

# I make some small edits to the cleaned summary table created in the script - need
# to rename the columns.

summary_table <- read_rds("clean_summary_table") %>% 
  rename("Handle" = "screenName", 
         "# of Tweets in 2019" = "tweet_count", 
         "Average Tweet Length in Characters" = "mean_tweet_length", 
         "Average Favorites Per Tweet" = "fav_average", 
         "Average Retweets Per Tweet" = "rt_average") 

####################################                 
# USER INTERFACE
####################################

# Define UI for the application, including a navigation bar at the top.

ui <- navbarPage("2020 Dems on Twitter", theme = shinytheme("flatly"),
                 
####################################                 
# HOME PAGE 
####################################

  tabPanel("Intro",
  
  fluidPage(
   
   # Application title
    
   titlePanel("The Story of the 2020 Democratic Primary, as Told Through Twitter"),
   
   # The p(paste("text")) function is how I insert text into the app, repeated many times.
   
   p(paste("A record number of Democrats are seeking their party's nomination for the Presidency
          in 2020. Presumptive front-runners Joe Biden and Bernie Sanders aren't scaring away the
          competition: the field includes  former and sitting U.S. senators, representatives, 
          governors, a mayor, cabinet secretaries, an author, and a tech executive.")),
   
   # The br() function adds white space to the app.
   
   br(),
   
   p(paste("The success of any presidential campaign hinges on the candidate's ability to connect
          with voters on the ground, especially in the early primary and caucus states of Iowa, 
           New Hampshire, South Carolina, and Nevada.")),
   
   br(),
   
   p(paste("As proven by the current President, however, campaigns are also waged online - 
          especially on Twitter. According, the 2020 Democratic primary candidates have all 
           taken to the platform to spread their message.")),
   
   br(),
   
   p(paste("But how has each candidate used Twitter? How frequently do they tweet? How are their 
          tweets received? What language do they use, what's the tone of their writing, and 
           what issues are they talking about most?")),
   
   br(),
   
   p(paste("Using Twitter's API, I tried to answer these questions. For my analysis, I picked 
            twelve of the most popular candidates, both in terms of polling and fundraising. 
           Hope you enjoy the results!"))
   )
),

####################################
# SUMMARY STATS
####################################

tabPanel("Summary Statistics",
         
         fluidPage(
           
         # Application title
           
           titlePanel("Summary of Twitter Activity"),
           
           # This sidebar allows for the user to control the bin width of the visualization.
           
           sidebarLayout(
             sidebarPanel(
               sliderInput("bins",
                           "# of Bins:",
                           min = 10,
                           max = 50,
                           value = 30), 
               width = 2),
             
                 
              # The main panel will feature text and the two visualizations for this tab.
             
              mainPanel(
                
                "Not all candidates have taken to Twitter with the same frequency as others.
                Here you can see the candidates' Twitter activity since the beginning of 2019, as visualized
                by the volume of their tweets over time.",
                
                br(),
                br(),
                
                # This function pulls the created visualization from the section below, and outputs
                # it in the app.
                
                plotOutput("tweet_freq"),
                
                br(),
                br(),
                br(),
                
                p(paste("The table below presents a summary of each candidates' Twitter activity, 
                including their total number of tweets, the average length of their tweets, 
                and the average number of favorites and retweets each tweet receives.")),
                
                br(),
                
                p(paste("Feel free to order by a particular column by clicking on the corresponding header.")),
                
                br(),
                
                # The DT package is very helpful for making interactive tables.
                
                DTOutput("summary_table"),
                
                br(),
                br()

         )))),

####################################
# SENTIMENT ANALYSIS
####################################

tabPanel("Sentiment Analysis",
         
         fluidPage(
           
           # Application title
           
           titlePanel("Sentiment Analysis"),
               
               p(paste("Using a methodology called Sentiment Analysis, we are able to get a better
               sense of the tone of each candidate. This is done by checking each candidate's tweets
               for specific baskets of words, or lexicons. Depending on the lexicon, we can measure
               the sentiments of a candidate's language, such their positivity, negativity, or even politeness.")),
          
               br(),
              
               p(paste("In sentiment analysis using R, there are three main lexicons: AFINN, NRC, and BING.
                       The AFINN lexicon assigns a positivity rating to the text by aggregating the scores of
                       individual words, which range from -5 (most negative) to +5 (most positive). Some candidates
                       can have very high positivity ratings but still an overall negative AFINN value, given the magnitude 
                       of their negativity rating. What outliers do you notice among the candidates?")),
          
               br(),
               
               # I created visualizations for all three lexicons in the script, so I only need to
               # pull them from below here and stick them in between my text.
           
               plotlyOutput("afinn"),
               
               br(),
               br(),
               br(),
               br(),
          
              p(paste("The NRC lexicon is interesting because it assigns a specific emotion to each word, in addition
                      to an overall binary value of either positive or negative. These emotions include
                      anger, anticipation, disgust, fear, joy, sadness, surprise, and trust. You can use the
                      legend on the right to select for specific emotions and compare the candidates.")),
          
               br(),
               
               plotlyOutput("nrc"),
               
               br(),
          
              p(paste("The final standard lexicon, BING, is similar to the NRC method but does not include
              emotions. It focuses simply on applying a binary value of either positive or negative to each
                      tweet, based on the aggregate of each word's rating. You can see the relationship between 
                      this metric and the overall positivity rating assigned using the AFINN lexicon above.")),
          
               br(),
               
               plotlyOutput("bing")
               
               
             )
           ),

####################################
# KEY WORDS
####################################

tabPanel("Individual Key Words",
         
         fluidPage(
           
           # Application title
           
           titlePanel("Key Words"),
           
           # Sidebar with multiple inputs:
           
           sidebarLayout(
             sidebarPanel(
               
               # This text input box allows the user to search for any keyword within each
               # candidate's tweets. 
               
               textInput("word", "Please enter your keyword, e.g. healthcare.", "healthcare"),
               
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               
               # This drop-down box allows the user to select a particular candidate to view,
               # within the visualization of their top 25 used words.
               
               selectInput("candidate", 
                           "Please select a candidate to view.",
                           c("KamalaHarris",
                           "amyklobuchar",
                           "BernieSanders",
                           "BetoORourke",
                           "CoryBooker",
                           "ewarren",
                           "Hickenlooper",
                           "JayInslee",
                           "JoeBiden",
                           "JulianCastro",
                           "PeteButtigieg",
                           "SenGillibrand"))
             ),
             
             # Now for the main text and visualizations, which change according to the selected
             # inputs above.
             
             mainPanel(
               
               p(paste("Finally, it is also interesting to analyze candidates' language at the individual word
                       level. Which candidates use which what words, and how frequently?")),
               
               br(),
               
               p(paste("Using the input box to the left, you can search each candidate's account for individual words.
                       Try some yourself - what about 'Trump', 'Mueller', 'healthcare', or 'climate'?")),
               br(),
               
               DTOutput("word_count_table"),
               
               br(),
               br(),
               
               p(paste("In addition to looking at individual key words, you can also select a candidate's Twitter 
                      handle to visualize which individual words they use most frequently. Use the drop-down box on the left
                      to pick a candidate.")),
               
               br(),
               
               plotOutput("candidate_words"),
               
               br()
               
               # I could also make a visualization of word use between two candidates the user selects,
               # like Mara did here: https://github.com/batpigandme/tidymueller/
               # This is something to be considered in a future project.
             )
           )
         )),

############
# END CREDITS
#############

tabPanel("Footnotes",
         
         fluidPage(
           
           # I also want to add an acknowledgements page at the end.
           
           titlePanel("Acknowledgements"),
           
           p(paste("Although I worked on this project alone, I could not have done it alone.")),
           
           br(),
           
           p(paste("I'd like to thank Michael Galarnyk, who wrote an excellent Medium article
                   explaining how to access Twitter's API using R.")),
          
          # Still trying to figure out how to make this a hyperlink using HTML tags.
          
           p(paste("Check it out here: https://medium.com/@GalarnykMichael/accessing-data-from-twitter-api-using-r-part1-b387a1c7d3e")),
           
           br(),
           
           p(paste("I'd also like to thank my friend Max Weiss, whose analysis of Trump's tweets was instrumental in developing
                  my own project and codebase.")),
           
           br()
         )))

###################################
# SERVER
###################################


# Define server logic required to create all the visualizations above. This is what's 
# "under the hood" of the app itself.

server <- function(input, output) {

  ###################################
  # SUMMARY STATS
  ###################################
  
  # Tweet frequency histogram
   output$tweet_freq <- renderPlot({
     
     cleaned_tweets %>% 
     ggplot(aes(x = created, fill = screenName)) +
       
       geom_histogram(
         position = "identity", show.legend = FALSE) +
       
       facet_wrap(~ screenName, nrow = 2) +
       
       labs(title = "2020 Democratic Challengers' Twitter Activity",
            subtitle = "Frequency of Tweets in 2019",
            caption = "Source: Twitter") +
       
       xlab("Date") +
       ylab("Frequency") +
       
       guides(fill = FALSE) +
       
    # Selected theme
       
       theme_fivethirtyeight() +
  
    # For connection to the bin sidebar
       
       geom_histogram(bins = input$bins) 
     
   })
   
  # Summary table
     
     output$summary_table <- renderDT(
       
       summary_table,
       
       # This are additional attributes particular to the DT package.
       
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
       labs(title = "Average Positivity of Candidates' Tweets",
            subtitle = "Per Afinn",
            y = "Average AFINN Positivity Rating", 
            fill = "Select Candidate") +
       theme(axis.title.x=element_blank(),
             # axis.text.x=element_blank(),
             axis.text.x = element_text(angle = 60, hjust = 1)) # very useful/cool for angling 
             # axis.ticks.x=element_blank())
     )
     
     
    # NRC OUTPUT
     
    output$nrc <- renderPlotly({
      
     ggplot(nrc_tweets, aes(x = screenName, y = n, fill = sentiment)) +
       geom_bar(position = "dodge", stat = "identity") +
       labs(title = "Emotions Conveyed by Candidates' Tweets",
            subtitle = "Per NRC",
            y = "Count",
            fill = "Sentiment") +
       theme(axis.title.x=element_blank(),
             # axis.text.x=element_blank(),
             axis.text.x = element_text(angle = 60, hjust = 1))
             # axis.ticks.x=element_blank())
     })
     
   # BING OUTPUT
   
   output$bing <- renderPlotly({

     ggplot(bing_tweets, aes(x = screenName, y = n, fill = sentiment)) +
       geom_bar(position = "dodge", stat = "identity") +
       labs(title = "Number of Positive and Negative Tweets",
            subtitle = "Per BING",
            y = "Count",
            fill = "Sentiment") +
       theme(axis.title.x=element_blank(),
             # axis.text.x=element_blank(),
             axis.text.x = element_text(angle = 60, hjust = 1))
             # axis.ticks.x=element_blank())
    })

   ####################################
   # KEY WORDS
   ####################################
   
   output$word_count_table <- renderDT(
     
  # This is for creating the DT word frequency table. It also connects to the user's input
  # as they select for which word they want to search.
     
     word_freq %>% dplyr::filter(Word == input$word),
     class = 'display',
     rownames = FALSE,
     options = list(dom = 't')
     
   )
   
  # This creates the visualization of the top 25 words per candidate. The original code
  # is in the script, but I found it easier to do it all here.
   
   output$candidate_words <- renderPlot({
    
     candidate_words <- 
       cleaned_tweets %>% 
       filter(screenName == input$candidate) %>% # Allows for selection of particular candidate.
       unnest_tokens(word, text) %>% 
       anti_join(stop_words) %>% 
       count(word) %>% 
       filter(word != "https" & word != "t.co") %>% # A few filler words kept appearing
       arrange(desc(n)) %>% 
       slice(1:25) # Takes top 25 only
     
     # Using the data above to create the visualization. The reorder and coord_flip functions are great
     # in making the bars horizontal, not vertical.
     
       ggplot(candidate_words, aes(x = reorder(word, n), y = n)) +
       geom_col() +
       coord_flip() +
       labs(title = "Count of Most Frequently Used Words") +
       xlab("Word") +
       ylab(NULL) +
       
       theme_fivethirtyeight()
  
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

