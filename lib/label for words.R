
ocrErrors <- d2

groundTruth1 <- array(NA, c(1, length(truthFiles)))
for(x in 1:length(truthFiles)) groundTruth1[,x] <- readChar(truthFiles[x], file.info(truthFiles[x])$size)
groundTruth1 <- strsplit(groundTruth1,"\n")
groundTruth1

data("crude")
tdm <- TermDocumentMatrix(crude)[1:10,1:20]
Docs(tdm)







groundTruth <- array(NA, c(1, length(truthTrain)))

for (x in 1:length(truthTrain)){
  groundTruth[,x] <- readChar(truthTrain[x], file.info(truthTrain[x])$size)
  Line <- strsplit(groundTruth[,x],"\n")
  Line_seperate <- unlist(Line)
  
  for(i in 1:length(readLines(truthTrain[x]))){
    wordbag <- strsplit(Line_seperate[[i]]," ")
    wordbag <- unlist(wordbag)
    
    for (j in 1:length(wordbag[i])){
      word <- wordbag[[j]]
      word <- cbind(word,i,x) #i is #line, x is #document
    }
  }
  return(word)
} 


groundTruth[,1]
m <- strsplit(groundTruth[,1],"\n")
mm <- unlist(m)

p <- strsplit(mm[[1]]," ")

pp <- unlist(p)
head(pp)
pp[1]
unlist(m)[[1]]
pp[[1]]


#################################################################
groundTruth <- array(NA, c(1, length(truthTrain)))
lines <- array(NA,c(1:length(readLines(truthTrain[x]))))

for (x in 1:length(truthTrain)){
  groundTruth[,x] <- readChar(truthTrain[x], file.info(truthTrain[x])$size)
  
  for (i in 1:length(readLines[x])){
    lines <- array(NA,c(1:length(readLines(truthTrain[x]))))
    lines[,i] <-  unlist(strsplit(groundTruth[,x],"\n")) 
    for (j in 1:)
  }
}

 
######################################################################
yy <- list()
for (i in 1:3){
  yy[[i]] <- matrix(NA,3,1)
}




