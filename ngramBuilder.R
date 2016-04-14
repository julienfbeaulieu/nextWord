setwd("C:/Users/Julien/Google Drive/15 - coursera/Capstone")

##input data
blogs <- readLines("en_US.blogs.txt",encoding="UTF-8")
twitter <- readLines("en_US.twitter.txt",encoding="UTF-8")
news <- readLines("en_US.news.txt",encoding="UTF-8")

##extract samples for model training, development and testing
set.seed(123)
sample <- c(sample(blogs,length(blogs)*0.01),
            sample(twitter,length(twitter)*0.01),
            sample(news,length(news)*0.01))
partition <- runif(length(sample))

sample.training <- sample[partition <= 0.80]
sample.devset <- sample[partition >0.80 & partition <= 0.90]
sample.testing <- sample[partition > 0.90]

##clean data
cleanData <- function(text) {
  
  sentences <- unlist(strsplit(text, "(?<!Mr|Mrs|Dr|Ms|St)[?!.]", perl = TRUE)) ##split data in sentences
  sentences <- gsub("[^A-Za-z0-9 ]", "", sentences) ##remove any non-alphabetical characters
  sentences <- tolower(sentences) ##bring to lowercase
  sentences <- gsub("[0-9]+","##", sentences) ##replace any numbers by a ## tag
  sentences <- gsub("[ ]{2,}"," ", sentences) ##remove numerous spaces when they occurs
  sentences <- gsub("^ ","", sentences) ##remove space at beggining of strings when they occur
  
  profanity <- read.csv("profanityList2.csv", header = FALSE) ##load list of profanities
  profanityRegex <- paste("\\b",profanity,"\\b", sep="") ##adapt in Regex form
  
  ## for all sentences, for all profanities, check if the sentence has a profanity
  hasProfanity <- rep(FALSE,length(sentences)) 
  for (s in 1:length(sentences)) {
    for (p in 1:length(profanityRegex)) {
      if (grepl(profanityRegex[p],sentences[s])) {
        hasProfanity[s] <- TRUE
        break
      }
    }
  }
  
  sentences <- sentences[!hasProfanity] ## remove sentences with profanity
  sentences <- sentences[nchar(sentences)>0] ## remove empty lines
  
  return(sentences)
}

corpus.training <- as.data.frame(cleanData(sample.training),stringsAsFactors = FALSE)
corpus.devset <- as.data.frame(cleanData(sample.devset),stringsAsFactors = FALSE)
corpus.testing <- as.data.frame(cleanData(sample.testing),stringsAsFactors = FALSE)

##Remove unused data
rm(blogs,news,partition,sample,sample.devset,sample.testing,sample.training,twitter)
gc(verbose = FALSE)

## Train models
### build ngram frequency tables
getNGramFrequency <- function(corpus,min,max) {
  library(RWeka)
  ngram <- NGramTokenizer(corpus, Weka_control(min = min, max = max))
  ngram.freq <- as.data.frame(table(ngram))
  names(ngram.freq) <- c("Ngram", "Frequency")
  return (ngram.freq)
}

ngram.1 <- getNGramFrequency(corpus.training,1,1)
ngram.2 <- getNGramFrequency(corpus.training,2,2)
ngram.3 <- getNGramFrequency(corpus.training,3,3)
ngram.4 <- getNGramFrequency(corpus.training,4,4)
ngram.5 <- getNGramFrequency(corpus.training,5,5)
ngram.6 <- getNGramFrequency(corpus.training,6,6)

##truncated tables
ngram.1.trunc <- ngram.1[ngram.1$Frequency > 2,]
ngram.2.trunc <- ngram.2[ngram.2$Frequency > 2,]
ngram.3.trunc <- ngram.3[ngram.3$Frequency > 2,]
ngram.4.trunc <- ngram.4[ngram.4$Frequency > 2,]
ngram.5.trunc <- ngram.5[ngram.5$Frequency > 1,]
ngram.6.trunc <- ngram.6[ngram.6$Frequency > 1,]

###export those for later use
write.csv(ngram.1,"ngram.1.csv")
write.csv(ngram.2,"ngram.2.csv")
write.csv(ngram.3,"ngram.3.csv")
write.csv(ngram.4,"ngram.4.csv")
write.csv(ngram.5,"ngram.5.csv")
write.csv(ngram.6,"ngram.6.csv")

write.csv(ngram.1.trunc,"ngram.1.trunc.csv")
write.csv(ngram.2.trunc,"ngram.2.trunc.csv")
write.csv(ngram.3.trunc,"ngram.3.trunc.csv")
write.csv(ngram.4.trunc,"ngram.4.trunc.csv")
write.csv(ngram.5.trunc,"ngram.5.trunc.csv")
write.csv(ngram.6.trunc,"ngram.6.trunc.csv")


write.table(corpus.training,"corpus.training.txt")
write.table(corpus.devset,"corpus.devset.txt")
write.table(corpus.testing,"corpus.testing.txt")

##develop model
devSet.1 <- getNGramFrequency(corpus.devset,1,1)
devSet.2 <- getNGramFrequency(corpus.devset,2,2)
devSet.3 <- getNGramFrequency(corpus.devset,3,3)
devSet.4 <- getNGramFrequency(corpus.devset,4,4)
devSet.5 <- getNGramFrequency(corpus.devset,5,5)
devSet.6 <- getNGramFrequency(corpus.devset,6,6)

write.csv(devSet.1,"devSet.1.csv")
write.csv(devSet.2,"devSet.2.csv")
write.csv(devSet.3,"devSet.3.csv")
write.csv(devSet.4,"devSet.4.csv")
write.csv(devSet.5,"devSet.5.csv")
write.csv(devSet.6,"devSet.6.csv")

##test model
testSet.1 <- getNGramFrequency(corpus.testing,1,1)
testSet.2 <- getNGramFrequency(corpus.testing,2,2)
testSet.3 <- getNGramFrequency(corpus.testing,3,3)
testSet.4 <- getNGramFrequency(corpus.testing,4,4)
testSet.5 <- getNGramFrequency(corpus.testing,5,5)
testSet.6 <- getNGramFrequency(corpus.testing,6,6)

write.csv(testSet.1,"testSet.1.csv")
write.csv(testSet.2,"testSet.2.csv")
write.csv(testSet.3,"testSet.3.csv")
write.csv(testSet.4,"testSet.4.csv")
write.csv(testSet.5,"testSet.5.csv")
write.csv(testSet.6,"testSet.6.csv")

