
source("../lib/LDA.R")
source("../lib/differ.R")
source("../lib/confusionMatrix.R")
load(file="../output/confusionMatrix.RData")
load(file = "../output/LDA.RData")
load(file="../output/ocrerror.RData")

# load(file="../output/wordTopic.RData")
# load(file="../output/docTopic.RData")


nTopics=20

ocrcorrect <- MM
ocrcorrect <- ocrerror


for(i in 1:nrow(ocrcorrect)){
  if((ocrcorrect[i,5] == 1) & !(" " %in% unlist(strsplit(ocrcorrect[i,1], "")))){
    candidates <- differCandidates(ocrcorrect[i,1])
    winner <- candidates[1]
    prevBest <- 0
    for(k in 1:length(candidates)){
      pWord <- 0
      mistakeProb <- 1
      for(j in 1:nTopics){
        probDoc <- docTopic[docTopic$topic == j & 
                              docTopic$document == ocrcorrect[i,4],3]
        if(sum(wordTopic$term == candidates[k]) == 0){
          probWord = 0
        }
        else{
        probWord <- wordTopic[wordTopic$topic == j &
                                wordTopic$term == candidates[k], 3]
        }
        originalLetters <- unlist(strsplit(ocrcorrect[i,1],""))
        candidateLetters <- unlist(strsplit(candidates[k],""))
        

        pWord <- pWord + probDoc * probWord
      }
      
      for(h in 1:length(originalLetters)){
        mistakeProb <- mistakeProb *
          confMat[candidateLetters[h], originalLetters[h]]/
          sum(confMat[candidateLetters[h],])
      }
      
      score <- pWord * mistakeProb
      if(score <- prevBest) winner <- candidate[k]; prevBest <- score
      
    }
    ocrcorrect[i,1] <- winner
    print(i)
  }
  
}

compare <- head(cbind(ocrcorrect[,1], MM[,1]))
compare

save(ocrcorrect,file = "../output/ocrcorrect.RData")





