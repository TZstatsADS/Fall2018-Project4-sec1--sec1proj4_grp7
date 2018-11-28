
OCRText
l
ocrErrors

for(i in 1:length(ocrErrors)){
  if(ocrErrors[i,2]){
    cand <- differCandidates(ocrErrors[i,1])
    for(i in cand){
      for(j in 1:30)
        
    }
  } 
}


# word line documents error



source("../lib/LDA.R")
source("../lib/differ.R")
load(file = "../output/LDA.RData")
load(file="../output/OCRText.RData")
save(file="../output/wordTopic.RData")
save(file="../output/docTopic.RData")

differCandidates("ompany")

for (i in 1:length(ocrErrors)){
  
  #p_wc probability of word_c in each document
   p_wc <- sum(p_wt*p_t)
  


  #p_wt probability of word_c in topic_k per document
  
   
  #p_t probability of topic_k in each document
  doc <- OCRText[1,][4]
  topic <- docTopic[docTopic$document == doc,]
  topic <- as.data.frame(tpoic)
  
  for (j in 1:20){
    word_tpoic <- wordTopic[wordTopic$topic == topic[1,][2],]
    word_topic <- word_topic[wordTopic$term == OCRText[i,][1]]
    beta <- word_topic[,3]
    return(beta)
  }
 
  p_w <- sum(robabili)

}


