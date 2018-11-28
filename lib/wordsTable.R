#library(hunspell)
#Sys.setenv(DICPATH = "/my/custom/hunspell/dir")
#hunspell:::dicpath()

library(stringr)
library(tm)
library(dplyr)
library(tidytext)
library(broom)


wordsTable <- function(fileNames, outName){
  
  # check how many words tesseract has
  words <- ""
  for(x in fileNames) words <- paste(words, readChar(x, file.info(x)$size))
  words <- strsplit(words,"\n")[[1]]
  bag <- strsplit(words," ")
  n <- length(unlist(bag))
  
  # create dataset of words
  tokens <- c()
  i <- 1
  for(x in fileNames){
    tokens[i] <- readChar(x, file.info(x)$size)
    i <- i + 1
  }
  
  tokens <- strsplit(tokens,"\n")

  for(i in 1 : length(tokens)){
    tokens[[i]] <- strsplit(tokens[[i]]," ")
  }
  
  bag <- matrix(NA, ncol = 5, nrow = n, 
                dimnames = list(rep("", n), c("token", "word", "line", "doc", "error")))
  i <- 1
  for(d in 1:length(tokens)){ # for each doc
    for(l in 1:length(tokens[[d]])){ # for each line
      for(w in 1:length(tokens[[d]][[l]])){ # for each word
        bag[i,] <- c(tokens[[d]][[l]][w], w, l, d, 0)
        i <- i + 1
      }
    }
  }
  
  bag <- bag[complete.cases(bag),]
  
  # toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  
  corpus <- VCorpus(VectorSource(bag[,1]))%>%
    tm_map(content_transformer(tolower))%>%
    # tm_map(toSpace, "\\W")%>%
    tm_map(removePunctuation)%>%
    tm_map(removeWords, character(0))%>%
    tm_map(stripWhitespace)
  
  library(textreg)
  b <- convert.tm.to.character(corpus)
  bag[,1] <- b
  
  assign(outName, bag)
  save(list=outName, file = paste0("../output/", outName, ".RData"))
  return(eval(as.name(paste(outName))))
}

wordsTable(ocrTrain, "ocrTrainTable")


