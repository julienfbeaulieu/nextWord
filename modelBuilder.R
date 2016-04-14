setwd("C:/Users/Julien/Documents/GitHub/nextWord/Model construct")

##input data
ngram.1 <- read.csv("ngram.1.trunc.csv")
ngram.2 <- read.csv("ngram.2.trunc.csv")
ngram.3 <- read.csv("ngram.3.trunc.csv")
ngram.4 <- read.csv("ngram.4.trunc.csv")

testSet.6 <- read.csv("testSet.6.csv")


##Evaluation models

unigramEval <- function(ngramSet) {
  
  library(stringr)
  unigram <- word(ngramSet,-1)
  
  history.freq <- sum(ngram.1$Frequency)
  unigram.freq <- ngram.1[match(unigram,ngram.1$Ngram), "Frequency"]
  
  MLE <- unigram.freq/history.freq
  MLE[!is.finite(MLE)] <- 0
  
  return(MLE)
}

bigramEval <- function(ngramSet) {
  
  library(stringr)
  history <- word(ngramSet,-2,-2)
  bigram <- word(ngramSet,-2,-1)
  
  history.freq <- ngram.1[match(history,ngram.1$Ngram),"Frequency"]
  bigram.freq <- ngram.2[match(bigram,ngram.2$Ngram), "Frequency"]
  
  MLE <- bigram.freq/history.freq
  MLE[!is.finite(MLE)] <- 0
  
  return(MLE)
}

trigramEval <- function(ngramSet) {
  
  library(stringr)
  history <- word(ngramSet,-3,-2)
  trigram <- word(ngramSet,-3,-1)
  
  history.freq <- ngram.2[match(history,ngram.2$Ngram),"Frequency"]
  trigram.freq <- ngram.3[match(trigram,ngram.3$Ngram), "Frequency"]
  
  MLE <- trigram.freq/history.freq
  MLE[!is.finite(MLE)] <- 0
  
  return(MLE)
}

quadrigramEval <- function(ngramSet) {
  
  library(stringr)
  history <- word(ngramSet,-4,-2)
  quadrigram <- word(ngramSet,-4,-1)
  
  history.freq <- ngram.3[match(history,ngram.3$Ngram),"Frequency"]
  quadrigram.freq <- ngram.4[match(quadrigram,ngram.4$Ngram), "Frequency"]
  
  MLE <- quadrigram.freq/history.freq
  MLE[!is.finite(MLE)] <- 0
  
  return(MLE)
}

quadrigramEval <- function(ngramSet) {
  
  library(stringr)
  history <- word(ngramSet,-4,-2)
  quadrigram <- word(ngramSet,-4,-1)
  
  history.freq <- ngram.3[match(history,ngram.3$Ngram),"Frequency"]
  quadrigram.freq <- ngram.4[match(quadrigram,ngram.4$Ngram), "Frequency"]
  
  MLE <- quadrigram.freq/history.freq
  MLE[!is.finite(MLE)] <- 0
  
  return(MLE)
}

pentagramEval <- function(ngramSet) {
  
  library(stringr)
  history <- word(ngramSet,-5,-2)
  pentagram <- word(ngramSet,-5,-1)
  
  history.freq <- ngram.4[match(history,ngram.4$Ngram),"Frequency"]
  pentagram.freq <- ngram.5[match(pentagram,ngram.5$Ngram), "Frequency"]
  
  MLE <- pentagram.freq/history.freq
  MLE[!is.finite(MLE)] <- 0
  
  return(MLE)
}

hexagramEval <- function(ngramSet) {
  
  library(stringr)
  history <- word(ngramSet,-6,-2)
  hexagram <- word(ngramSet,-6,-1)
  
  history.freq <- ngram.5[match(history,ngram.5$Ngram),"Frequency"]
  hexagram.freq <- ngram.6[match(hexagram,ngram.6$Ngram), "Frequency"]
  
  MLE <- hexagram.freq/history.freq
  MLE[!is.finite(MLE)] <- 0
  
  return(MLE)
}

##Accuracy function
checkAccuracy <- function(predictions,lastWord) {
  
  ###top1 accuracy
  top1Accuracy <- sum(predictions[,1] == lastWord)/ (dim(predictions)[1])
  
  ###top3 accuracy
  top3Accuracy <- (sum(predictions[,1] == lastWord)+
                     sum(predictions[,2] == lastWord)+
                     sum(predictions[,3] == lastWord))/ (dim(predictions)[1])
  Accuracy <- cbind(top1Accuracy,top3Accuracy)
  return(Accuracy)
  
}

##Prediction model

###Test set
set.seed(456)
testSet.sample <- sample(testSet.6$Ngram,1000)
library(stringr)
history.6 <- word(testSet.sample,1,5)
lastWord.6 <- word(testSet.sample,-1)

###Prediction function
InterpolatedPred3 <- function(history, lambda = rep(1/4,4)) {
  
  ##Construct candidate list, based on bigrams
  library(stringr)
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
  mle.IM <- as.matrix(mle) %*% (lambda)
  
  reorder <- sort(mle.IM,decreasing = TRUE,index.return = TRUE)
  
  top3 <- as.character(candidates[reorder$ix[1:3]])
  top3.MLE<- as.numeric(reorder$x[1:3])
  
  return(top3)
}

##make predictions and record time
ptm <- proc.time()
predictions3 <- t(sapply(history.6,InterpolatedPred3))
time3 <- (proc.time() - ptm)

##check accuracy
accuracy3 <- checkAccuracy(predictions3,lastWord.6) 

##check space allocation
ngram.size <- c(as.numeric(object.size(ngram.1)),
                as.numeric(object.size(ngram.2)),
                as.numeric(object.size(ngram.3)),
                as.numeric(object.size(ngram.4)))

Eval.size <- c(as.numeric(object.size(unigramEval)),
               as.numeric(object.size(bigramEval)),
               as.numeric(object.size(trigramEval)),
               as.numeric(object.size(quadrigramEval)),
               as.numeric(object.size(InterpolatedPred3)))

##write records
write.csv(predictions3,"pred3.csv")
write.csv(accuracy3, "accuracy3.csv")
write.table(as.matrix(time3),"time3.txt")
write.table(ngram.size,"ngram.size.txt")
write.table(Eval.size,"Eval.size.txt")




##Test wordclouds
###Prediction function which returns full candidate list
InterpolatedPred4 <- function(history, lambda = rep(1/4,4)) {
  
  ##Construct candidate list, based on bigrams
  library(stringr)
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
  
  return(predictions)
}

###Wordcloud function
makeWordCloud <- function(candidates, likelihood){
  
  frequency <- floor(1000*likelihood) ##convert likelihood to counts of words
  library(wordcloud)
  wordcloud(candidates,frequency, min.freq = 2)
}
