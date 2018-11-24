load(file = "../output/dict.RData")

# w <- "cat"

# @cand this is fnowpd
# @end

differCandidates <- function(w){
  
  cand <- c()
  
  for(i in 1:nchar(w)){
    l <- substr(w, i,i)
    v <- letters[!letters %in% l]
    beg <- substr(w, 1,i - 1)
    end <- substr(w, i + 1,nchar(w))
    candidates <- paste(beg, v, end, sep = "")
    cand <- c(cand, candidates[candidates %in% dict])
  }
  
  for(i in 1:nchar(w)){
    for(j in 1:nchar(w)){
      if(j > i){
        l_i <- substr(w, i,i)
        l_j <- substr(w, j,j)
        v_i <- letters[!letters %in% l_i]
        v_j <- letters[!letters %in% l_j]
        beg <- substr(w, 1,i - 1)
        mid <- substr(w, i + 1, j - 1)
        end <- substr(w, j + 1,nchar(w))
        candidates <- paste(beg, v_i, mid, v_j, end, sep = "")
        cand <- c(cand, candidates[candidates %in% dict])
      }
    }
  }
  return(cand)
}
