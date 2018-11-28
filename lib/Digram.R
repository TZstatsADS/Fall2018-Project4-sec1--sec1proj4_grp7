load("../output/dict.RData")


digram <- function(d){
  # d = dictionary
  
  # d <- dict[nchar(dict) < 21]
  
  N <- max(nchar(d))
  digram <- array(0, c(N, (N-1)*N/2, 26, 26))
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

# 
# #use word of length 3 as an example
# wordlist <- c('abc', 'def','ghif')
# 
# subdic <- function(wordlist){
#   
#   listname <- paste('d', c(1,1,2), c(2,3,3), sep = '')
#   
#   #defining the positional dictornary
#   positionaldic <- list(matrix(NA, nrow = 26, ncol = 26), 
#                       matrix(NA, nrow = 26, ncol = 26),
#                       matrix(NA, nrow = 26, ncol = 26)) 
# 
#   names(positionaldic) <- listname
#   nword <- length(wordlist)
# 
#   for (i in 1: nword){
#   
#     word <- wordlist[i]
#   
#     for (a in 1:(nword-1)){
#     
#       for (b in (a+1):nword){
#         row <- which(letters == strsplit(word, split = '')[[1]][a]) 
#         col <- which(letters == strsplit(word, split = '')[[1]][b])
#         positionaldic[[paste("d", a, b ,sep = '')]][row, col] <- 1  
#     }
#   }
#   }
#   return(positionaldic)
# }
# 
# 
