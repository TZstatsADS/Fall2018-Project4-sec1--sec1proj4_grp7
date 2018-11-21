load("../output/dict.RData")
load("../output/dict2.RData")
load("../output/digrams.RData")


d2 <- dict2[nchar(dict2[,1]) < 16,]
d2 <- d2[nchar(d2[,1]) > 1,] # no single character words


for(i in 1:nrow(d2)){
  n <- nchar(d2[i,1])
  counter <- 1
  for(k in 1 : (n-1)){
    for(l in 2:n){
      if(k < l){
        cat(i, " ", k, " ", l)
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

table(as.numeric(d2[,2]))

sum(d2[,2])

