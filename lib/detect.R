load("../output/dict.RData")
load("../output/OCRText.RData")
load("../output/digrams.RData")


#d2 <- OCRText[nchar(OCRText[,1]) < 21,]

detect <- function(d2, digram){
  #d2 <- OCRtext[nchar(OCRtext[,1]) < 25,]
  d2 <- d2[nchar(d2[,1]) > 1,] # no single character words
  numberletters <- c(0:9, letters)

  stop = F
  for(i in 1:nrow(d2)){
    n <- nchar(d2[i,1])
    counter <- 1
    if(prod(unlist(strsplit(d2[i,1], "")) %in% numberletters)){ # if contains only letters
      for(k in 1 : (n-1)){
        for(l in 2:n){
          if(k < l){
            # cat(i, " ", k, " ", l)
            if(digram[n, counter, match(substr(d2[i,1], k, k), numberletters), 
                match(substr(d2[i,1], l, l), numberletters)] == 0){
              d2[i,5] <- 1
              stop = TRUE
              break
            }
            counter <- counter + 1
            if (stop){break}
          }   
          if (stop){break}
        }
        if (stop){break}
      }
    }
  }
  
  table(as.numeric(d2[,5]))
  ocrerror <- d2
  return(ocrerror)
  #save(ocrerror,file = "../output/ocrerror.RData")
}
























