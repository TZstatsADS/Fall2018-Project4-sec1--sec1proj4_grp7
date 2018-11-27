# library(stringr)
# library(tm)
# library(dplyr)
# library(tidytext)
# library(broom)

files <- list.files(path="../data/ground_truth/", pattern="*.txt", full.names=TRUE, recursive=FALSE)
groundTruth <- array(NA, c(1, length(files)))
for(x in 1:length(files)) groundTruth[,x] <- readChar(files[x], file.info(files[x])$size)
groundTruth <- strsplit(groundTruth,"\n")


files <- list.files(path="../data/tesseract/", pattern="*.txt", full.names=TRUE, recursive=FALSE)
tesseract <- array(NA, c(1, length(files)))
for(x in 1:length(files)) tesseract[,x] <- readChar(files[x], file.info(files[x])$size)
tesseract <- strsplit(tesseract,"\n")

pdf("plots.pdf", width=100, height=100)
par(mfrow = c(10,10))
for(i in 1: length(files)) barplot(nchar(tesseract[[i]]) / nchar(groundTruth[[i]]))
dev.off()

str(groundTruth[[1]][10])

nWords <- function(input) sapply(strsplit(input, " "), length)

strsplit(groundTruth, " ")


a <- array(0, c(36, 36))
possibilities <- c(0:9, letters)


for(i in 1:length(files)){
  truthBag <- strsplit(groundTruth[[i]]," ")
  tessBag <- strsplit(tesseract[[i]]," ")
  for(j in 1 : min(length(truthBag), length(tessBag))){
    if(length(truthBag[[j]]) == length(tessBag[[j]])){
      truthWords <- truthBag[[j]]
      tessWords <- tessBag[[j]]
      for(k in 1: length(truthWords)){
        if((nchar(truthWords[k]) == nchar(tessWords[k]))){
          truthLetters <- unlist(strsplit(truthWords[k], ""))
          tessLetters <- unlist(strsplit(tessWords[k], ""))
          for(l in 1:length(truthLetters)){
            if(truthLetters[l] != tessLetters[l]){
              a[match(tolower(truthLetters[l]), possibilities), 
                match(tolower(tessLetters[l]), possibilities)] = 
                a[match(tolower(truthLetters[l]), possibilities), 
                  match(tolower(tessLetters[l]), possibilities)] + 1
            }
          }

        }
      }
    }
    
  }
}

confMat <- a

save(confMat, file = "../output/confusionMatrix.RData")

# 
# # tesseract <- ""
# # for(x in files) tesseract <- paste(tesseract, readChar(x, file.info(x)$size))
# # tesseract <- strsplit(tesseract,"\n")[[1]]
# 
# barplot(nchar(groundTruth) - nchar(tesseract))
# length(groundTruth) - length(tesseract) # 14 missing lines in tesseract
# 
# # cbind(groundTruth[580:600], tesseract[580:600])
# # length(groundTruth)
# # length(tesseract)
# # groundTruth[(29758 - 2): (29758 + 2)]
# # tesseract[(29758 - 2): (29758 + 2)]
# 
# 
# truth <- groundTruth
# ocr <- tesseract
# 
# # recursive method
# 
# offsetting <- function(truth, ocr){
#   abnormal <- nchar(truth) > 3 * nchar(ocr)
#   for(i in 2:length(abnormal)){
#     if(abnormal[i] & ocr[i] != ""){
#       print(i)
#       ocr <- c(ocr[1:(i - 1)], "", ocr[(i):length(ocr)])
#       offsetting(truth, ocr)
#     }
#   }
#   return(ocr)
# }
# tessNew <- offsetting(groundTruth, tesseract)
# comp <- nchar(groundTruth) - nchar(tessNew)
# barplot(comp)
# 
# 
# # normal method
# 
# offsetSpots <- c()
# abnormal <- nchar(truth) > 3 * nchar(ocr) # arbitrary
# 
# for(i in 2:length(abnormal)){
#   if(abnormal[i]){
#     ocr <- c(ocr[1:(i - 1)], "", ocr[(i):length(ocr)])
#     abnormal <- nchar(truth) > 3 * nchar(ocr)
#     offsetSpots <- c(offsetSpots, i)
#   }
# }
# 
# comp <- nchar(truth[-offsetSpots]) - nchar(ocr[-offsetSpots])
# comp <- nchar(truth) - nchar(ocr)
# barplot(comp)
# 
# length(offsetSpots)  # 14 lines were insserted to tesseract
# 
# cbind(tail(truth, 50), tail(ocr, 50)) 
# 
# 
# cbind(tail(groundTruth), tail(tesseract))
