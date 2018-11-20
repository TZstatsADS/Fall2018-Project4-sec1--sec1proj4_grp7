#library(hunspell)
#Sys.setenv(DICPATH = "/my/custom/hunspell/dir")
#hunspell:::dicpath()

library(stringr)
library(tm)
library(dplyr)
library(tidytext)
library(broom)

files <- list.files(path="../data/tesseract/", pattern="*.txt", full.names=TRUE, recursive=FALSE)
tesseract <- ""

for(x in files) tesseract <- paste(tesseract, readChar(x, file.info(x)$size))

tesseract <- strsplit(tesseract,"\n")[[1]]
tesseract <- tesseract[tesseract!=""]
bag <- str_split(tesseract," ")
bag <- unlist(bag)

corpus <- VCorpus(VectorSource(bag))%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(removeWords, character(0))%>%
  tm_map(stripWhitespace)

dict2 <- tidytext::tidy(corpus) %>%
  select(text) %>%
  unnest_tokens(dictionary, text)

save(dict2, file = "../output/dict2.RData")

load(file = "C:/Users/Xiaojie Wei/Documents/GitHub/Fall2018-Project4-sec1--sec1proj4_grp7/output/dict2.RData")
#dict$ncharact <- nchar(dict$dictionary)

