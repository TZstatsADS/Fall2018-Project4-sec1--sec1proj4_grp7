
Digram <- function(d){
  # d = dictionary
  
  # d <- dict[nchar(dict) < 21]
  
  N <- max(nchar(d))
  digram <- array(0, c(N, (N-1)*N/2, 36, 36))
  numberletters <- c(0:9, letters)
  
  for(i in d){
    n <- nchar(i)
    counter <- 1
    for(k in 1 : (n-1)){
      for(l in 2:n){
        if(k < l){
          digram[n, counter, match(substr(i, k, k), numberletters), 
            match(substr(i, l, l), numberletters)] <- 1 
          counter <- counter + 1
        }   
      }
    }
  }
  
  save(digram, file = "../output/digrams.RData")
  return(digram)
}
