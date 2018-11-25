load("../output/dict.RData")


####For creating a subdictionary for word of fixed length
d <- dict[nchar(dict) < 16]

N <- 15
a <- array(0, c(N, (N-1)*N/2, 26, 26))

for(i in d){
  n <- nchar(i)
  counter <- 1
  for(k in 1 : (n-1)){
    for(l in 2:n){
      if(k < l){
        a[n, counter, match(substr(i, k, k), letters), 
          match(substr(i, l, l), letters)] <- 1 
        counter <- counter + 1
      }   
    }
  }
}


save(a, file = "../output/digrams.RData")
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
