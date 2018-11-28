#library(hunspell)
#Sys.setenv(DICPATH = "/my/custom/hunspell/dir")
#hunspell:::dicpath()

library(stringr)
library(tm)
library(dplyr)
library(tidytext)
library(broom)


OCRText <- function(ocrTrain){
  
  # check how many words tesseract has
  tessWords <- ""
  for(x in ocrTrain) tessWords <- paste(tessWords, readChar(x, file.info(x)$size))
  tessWords <- strsplit(tessWords,"\n")[[1]]
  bag <- strsplit(tessWords," ")
  n <- length(unlist(bag))
  
  
  tesseract <- c()
  i <- 1
  for(x in ocrTrain){
    tesseract[i] <- readChar(x, file.info(x)$size)
    i <- i + 1
  }
  
  tesseract <- strsplit(tesseract,"\n")
  # tesseract <- tesseract[tesseract!=""]
  
  for(i in 1 : length(tesseract)){
    tesseract[[i]] <- strsplit(tesseract[[i]]," ")
  }
  
  bag <- matrix(NA, ncol = 5, nrow = n, dimnames = list(rep("", n), c("token", "word", "line", "doc", "error")))
  i <- 1
  for(d in 1:length(tesseract)){ # for each doc
    for(l in 1:length(tesseract[[d]])){ # for each line
      for(w in 1:length(tesseract[[d]][[l]])){ # for each word
        bag[i,] <- c(tesseract[[d]][[l]][w], w, l, d, 0)
        i <- i + 1
      }
    }
  }
  
  bag <- bag[complete.cases(bag),]
  
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  
  corpus <- VCorpus(VectorSource(bag[,1]))%>%
    tm_map(content_transformer(tolower))%>%
    # tm_map(toSpace, "\\W")%>%
    tm_map(removePunctuation)%>%
    tm_map(removeWords, character(0))%>%
    tm_map(stripWhitespace)
  
  ocrText <- tidytext::tidy(corpus) %>%
    select(text) %>%
    unnest_tokens(dictionary, text)
  
  library(textreg)
  b <- convert.tm.to.character(corpus)
  bag[,1] <- b
  OCRText <- bag
  save(OCRText, file = "../output/OCRText.RData")
  
}


