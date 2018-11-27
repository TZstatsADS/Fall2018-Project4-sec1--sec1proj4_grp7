library(stringr)
library(tm)
library(dplyr)
library(tidytext)
library(broom)

dictionary <- function(truthTrain){
  
  groundTruth <- ""
  for(x in truthTrain) groundTruth <- paste(groundTruth, readChar(x, file.info(x)$size))
  
  groundTruth <- strsplit(groundTruth,"\n")[[1]]
  groundTruth <- groundTruth[groundTruth!=""]
  bag <- str_split(groundTruth," ")
  bag <- unlist(bag)
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  
  corpus <- VCorpus(VectorSource(bag))%>%
    tm_map(content_transformer(tolower))%>%
    tm_map(toSpace, "\\W")%>%                             
    tm_map(removePunctuation)%>%
    tm_map(removeNumbers)%>%
    tm_map(removeWords, character(0))%>%
    tm_map(stripWhitespace)
  
  dict <- tidytext::tidy(corpus) %>%
    select(text) %>%
    unnest_tokens(dictionary, text)
  
  dict <- as.matrix(dict)
  dict <- dict[nchar(dict) > 1] # no single character words
  dict <- unique(dict)
  
  save(dict, file = "../output/dict.RData")
}

