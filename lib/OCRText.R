#library(hunspell)
#Sys.setenv(DICPATH = "/my/custom/hunspell/dir")
#hunspell:::dicpath()

library(stringr)
library(tm)
library(dplyr)
library(tidytext)
library(broom)


OCRText <- function(ocrTrain){
  
  tesseract <- ""
  for(x in ocrTrain) tesseract <- paste(tesseract, readChar(x, file.info(x)$size))
  
  tesseract <- strsplit(tesseract,"\n")[[1]]
  tesseract <- tesseract[tesseract!=""]
  bag <- str_split(tesseract," ")
  bag <- unlist(bag)
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  
  corpus <- VCorpus(VectorSource(bag))%>%
    tm_map(content_transformer(tolower))%>%
    tm_map(toSpace, "\\W")%>%
    tm_map(removePunctuation)%>%
    tm_map(removeWords, character(0))%>%
    tm_map(stripWhitespace)
  
  dict2 <- tidytext::tidy(corpus) %>%
    select(text) %>%
    unnest_tokens(dictionary, text)
  
  dict2 <- as.matrix(dict2)
  OCRText <- cbind(dict2, rep(0, nrow(dict2)))
  colnames(OCRText) <- c("word", "error")
  
  save(OCRText, file = "../output/OCRText.RData")
}


