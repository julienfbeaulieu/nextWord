library(shiny)

##load tables
shinyUI(pageWithSidebar(
  headerPanel('Next word prediction'),
  sidebarPanel(
    
    h2("Input"),
    textInput("text",
              "Enter text..."),
    
    helpText("Note that only the last 3 words",
             "will be taken into account")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("NextWord", 
              
              fluidRow(
                column(8,
                       h3("Model predictions:"),
                       plotOutput("wordcloud")),
                column(4,
                       h3("Best 3 guesses:"),
                       tableOutput("top3")))),
        
      tabPanel("About",
        
               br(),
               p("Author: Julien Beaulieu"),
               p("Last update: 15 april 2016"),
               
               br(),
               p("NextWord is a shiny app that tries to predict the next word you will be typing."),
               
               br(),
               p("The predictions are made based on an interpolated ", 
                 a("n-gram model", href = "http://en.wikipedia.org/wiki/N-gram"),
                 ". Namely, the model looks at the previous 3 words and analyse which words typically follow them.
                  The model was trained using the ", 
                 a("HC Corpora", href = "http://www.corpora.heliohost.org/"),
                 ", which includes excerpts from blogs, newspapers and tweets in English."),
               
               br(),
               p("After typing a set of words, NextWord's wordcloud will show you the most probable following word.
                 The size of the words show the confidence of the model: the bigger the words, the more NextWord thinks he's right.
                 The color of the words show the ranking: in blue are the first guesses, whereas in red are the last guesses.
                 Additionnaly, the three top guesses and their likelihood are tabulated under the wordcloud."),
               
               br(),
               p("The tag ## is used to denote any numerical values. For instance, both 9am and 10am would be incoded to ##am."),
               
               br(),
               p("For more information, visit the ",
                 a("github repository", href = "xxx"),
                 " of the project."),
               
               br(),
               p("This shiny app was built for the ", 
                 a("Data Science Capstone", href = "https://www.coursera.org/learn/data-science-project"),
                 " as part of the Data Science Specialization offered by John Hopkins University"))
    )
  )
))