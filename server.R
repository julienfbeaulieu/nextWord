library(shiny)
library(wordcloud)
library(stringr)

#load ngram tables
ngram.1 <- read.csv("ngram.1.trunc.csv")
ngram.2 <- read.csv("ngram.2.trunc.csv")
ngram.3 <- read.csv("ngram.3.trunc.csv")
ngram.4 <- read.csv("ngram.4.trunc.csv")

# Server function
shinyServer(function(input,output) {
  
  output$top3 <- renderTable({
    clean <- cleanData(input$text)
    predictions <- InterpolatedPred(clean)
    
    predictions[1:3,]
  })

  output$wordcloud <- renderPlot({
    clean <- cleanData(input$text)
    predictions <- InterpolatedPred(clean)
    frequency <- floor(1000*predictions$likelihood) ##convert likelihood to counts of words
    wordcloud(predictions$candidates,frequency, scale=c(6,1), min.freq = 2, colors = brewer.pal(8,"RdBu"))
    
  })
})



# cleanData function
cleanData <- function(text) {
  
  ##clean input
  text <- gsub("[^A-Za-z0-9 ]", "", text) ##remove any non-alphabetical characters
  text <- tolower(text) ##bring to lowercase
  text <- gsub("[0-9]+","##", text) ##replace any numbers by a ## tag
  text <- gsub("[ ]{2,}"," ", text) ##remove numerous spaces when they occurs
  text <- gsub("^ ","", text) ##remove space at beggining of strings when they occur
  
  return(text)
}

unigramEval <- function(ngramSet) {
  
  unigram <- word(ngramSet,-1)
  
  history.freq <- sum(ngram.1$Frequency)
  unigram.freq <- ngram.1[match(unigram,ngram.1$Ngram), "Frequency"]
  
  MLE <- unigram.freq/history.freq
  MLE[!is.finite(MLE)] <- 0
  
  return(MLE)
}

bigramEval <- function(ngramSet) {
  
  history <- word(ngramSet,-2,-2)
  bigram <- word(ngramSet,-2,-1)
  
  history.freq <- ngram.1[match(history,ngram.1$Ngram),"Frequency"]
  bigram.freq <- ngram.2[match(bigram,ngram.2$Ngram), "Frequency"]
  
  MLE <- bigram.freq/history.freq
  MLE[!is.finite(MLE)] <- 0
  
  return(MLE)
}

trigramEval <- function(ngramSet) {
  
  history <- word(ngramSet,-3,-2)
  trigram <- word(ngramSet,-3,-1)
  
  history.freq <- ngram.2[match(history,ngram.2$Ngram),"Frequency"]
  trigram.freq <- ngram.3[match(trigram,ngram.3$Ngram), "Frequency"]
  
  MLE <- trigram.freq/history.freq
  MLE[!is.finite(MLE)] <- 0
  
  return(MLE)
}

quadrigramEval <- function(ngramSet) {
  
  history <- word(ngramSet,-4,-2)
  quadrigram <- word(ngramSet,-4,-1)
  
  history.freq <- ngram.3[match(history,ngram.3$Ngram),"Frequency"]
  quadrigram.freq <- ngram.4[match(quadrigram,ngram.4$Ngram), "Frequency"]
  
  MLE <- quadrigram.freq/history.freq
  MLE[!is.finite(MLE)] <- 0
  
  return(MLE)
}

quadrigramEval <- function(ngramSet) {
  
  history <- word(ngramSet,-4,-2)
  quadrigram <- word(ngramSet,-4,-1)
  
  history.freq <- ngram.3[match(history,ngram.3$Ngram),"Frequency"]
  quadrigram.freq <- ngram.4[match(quadrigram,ngram.4$Ngram), "Frequency"]
  
  MLE <- quadrigram.freq/history.freq
  MLE[!is.finite(MLE)] <- 0
  
  return(MLE)
}

InterpolatedPred <- function(history, lambda = rep(1/4,4)) {
  
  ##Construct candidate list, based on bigrams
  candidateTest <- word(history,-1)
  candidateTest.regex <- paste0("^",candidateTest," ")
  candidates <- word(ngram.2[grepl(candidateTest.regex,ngram.2$Ngram),"Ngram"],-1)
  
  ##Add few common candidates if list is too short
  if (length(candidates) < 3) {
    candidates <- c(candidates,"the","to","and")
  }
  
  ##construct solutions
  solutions <- paste(history,candidates)
  
  ##Ask individual ngram models
  mle <- data.frame(unigramEval(solutions), 
                    bigramEval(solutions), 
                    trigramEval(solutions),
                    quadrigramEval(solutions))
  
  ##Combine answer
  likelihood <- as.matrix(mle) %*% (lambda)
  predictions <- data.frame(candidates,likelihood)
  predictions <- predictions[order(predictions$likelihood, decreasing = TRUE),]
  predictions$likelihood <- signif(predictions$likelihood,5)
  
  return(predictions)
}
