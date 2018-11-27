load("../output/dict.RData")
load("../output/OCRText.RData")
load("../output/digrams.RData")


#d2 <- OCRText[nchar(OCRText[,1]) < 21,]

detect <- function(d2){
  
  d2 <- d2[nchar(d2[,1]) > 1,] # no single character words

  stop = F
  for(i in 1:nrow(d2)){
    n <- nchar(d2[i,1])
    counter <- 1
    if(prod(unlist(strsplit(d2[i,1], "")) %in% letters)){ # if contains only letters
      for(k in 1 : (n-1)){
        for(l in 2:n){
          if(k < l){
            # cat(i, " ", k, " ", l)
            if(a[n, counter, match(substr(d2[i,1], k, k), letters), 
                match(substr(d2[i,1], l, l), letters)] == 0){
              d2[i,2] <- 1
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
  
  table(as.numeric(d2[,2]))
  
}

ocrErrors <- d2
