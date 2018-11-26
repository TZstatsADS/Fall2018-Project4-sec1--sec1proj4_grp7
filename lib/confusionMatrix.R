library(stringr)
library(tm)
library(dplyr)
library(tidytext)
library(broom)

files <- list.files(path="../data/ground_truth/", pattern="*.txt", full.names=TRUE, recursive=FALSE)
groundTruth <- ""
for(x in files) groundTruth <- paste(groundTruth, readChar(x, file.info(x)$size))
groundTruth <- strsplit(groundTruth,"\n")[[1]]


files <- list.files(path="../data/tesseract/", pattern="*.txt", full.names=TRUE, recursive=FALSE)
tesseract <- ""
for(x in files) tesseract <- paste(tesseract, readChar(x, file.info(x)$size))
tesseract <- strsplit(tesseract,"\n")[[1]]

barplot(nchar(groundTruth) - nchar(tesseract))
length(groundTruth) - length(tesseract) # 14 missing lines in tesseract

cbind(groundTruth[580:600], tesseract[580:600])
length(groundTruth)
length(tesseract)


truth <- groundTruth
ocr <- tesseract
offsetSpots <- c()
abnormal <- nchar(truth) > 3 * nchar(ocr) # arbitrary




offsetting <- function(truth, ocr){
  abnormal <- nchar(truth) > 3 * nchar(ocr)
  for(i in 2:length(abnormal)){
    if(abnormal[i] & ocr[i] != ""){
      print(i)
      ocr <- c(ocr[1:(i - 1)], "", ocr[(i):length(ocr)])
      offsetting(truth, ocr)
    }
  }
  return(ocr)
}


groundTruth[(29758 - 2): (29758 + 2)]
tesseract[(29758 - 2): (29758 + 2)]

tessNew <- offsetting(groundTruth, tesseract)
comp <- nchar(groundTruth) - nchar(tessNew)
barplot(comp)


for(i in 2:length(abnormal)){
  if(abnormal[i]){
    ocr <- c(ocr[1:(i - 1)], "", ocr[(i):length(ocr)])
    abnormal <- nchar(truth) > 3 * nchar(ocr)
    offsetSpots <- c(offsetSpots, i)
  }
}

comp <- nchar(truth[-offsetSpots]) - nchar(ocr[-offsetSpots])
barplot(comp)

length(offsetSpots)  # 14 lines were insserted to tesseract

cbind(tail(truth, 50), tail(ocr, 50)) 


cbind(tail(groundTruth), tail(tesseract))
