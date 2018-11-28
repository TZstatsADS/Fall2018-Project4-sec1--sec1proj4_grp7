correct <- function(OCRTestErrors, docTopic, wordTopic){
  
  source("../lib/differ.R")
  load(file="../output/confusionMatrix.RData")
  
  nTopics=30
  OCRCorrectTable <- OCRTestErrors
  
  for(i in 1:nrow(OCRCorrectTable)){
    if((OCRCorrectTable[i,5] == 1) & !(" " %in% unlist(strsplit(OCRCorrectTable[i,1], "")))){
      candidates <- differCandidates(OCRCorrectTable[i,1])
      winner <- candidates[1]
      prevBest <- 0
      if(length(candidates) != 0){
        for(k in 1:length(candidates)){
          pWord <- 0
          mistakeProb <- 1
          for(j in 1:nTopics){
            # probDoc <- docTopic[docTopic[,"topic"] == j & 
            #                       docTopic[,"document"] == OCRCorrectTable[i,4],"gamma"]
            probDoc <- docTopic[OCRCorrectTable[i,4], j]
            # if(! candidates[k] %in% wordTopic[,"term"]){
            if(! candidates[k] %in% rownames(wordTopic)){
              probWord = 0
            }
            else{
            # probWord <- wordTopic[wordTopic[,"topic"] == j &
            #                         wordTopic[,"term"] == candidates[k], 3]
            probWord <- wordTopic[candidates[k], j]
            }
  
            pWord <- pWord + as.numeric(probDoc) * as.numeric(probWord)
          }
          
          originalLetters <- unlist(strsplit(OCRCorrectTable[i,1],""))
          candidateLetters <- unlist(strsplit(candidates[k],""))
          
          for(h in 1:length(originalLetters)){
            mistakeProb <- mistakeProb *
              confMat[candidateLetters[h], originalLetters[h]]/
              sum(confMat[candidateLetters[h],])
          }
          
          score <- pWord * mistakeProb
          if(score > prevBest) winner <- candidates[k]; prevBest <- score
          
        }
        OCRCorrectTable[i,1] <- winner
        print(paste0("word ", i, "/", nrow(OCRCorrectTable)))
      }
    }
    
  }
  save(OCRCorrectTable,file = "../output/OCRCorrectTable.RData")
  return(OCRCorrectTable)
}





