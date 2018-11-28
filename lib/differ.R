load(file = "../output/dict.RData")

# This file includes a function that generates a list of words that differ by zero, one, or two characters from a given word

# Finding words that only differ by one letter

# @w this is the input word
# @l this is the letter at position i 
# @v this is a vector with all letters except l 
# @beg this is a string of all the characters in a word before i
# @end this is a string of all the characters in a word after i 
# @candidates this is the list of all possible strings that differ by one character, i, from w 
# @cand this is is the list of all possible strings that differ by one character,i, from w that match with a word in a dictionary 

# Finding words that differ by two letters

# @w this is the input word
# @l_i this is the letter at position i 
# @l_j this is the letter at position j
# @v_i this is a vector with all letters except l_i
# @v_j this is a vector with all letters except l_j
# @beg this is a string of all the characters in a word before i
# @mid this is a string of all the chracters in a word between i and j
# @end this is a string of all the characters in a word after j
# @candidates this is the list of all possible strings that differ by two characters, i, and j from w
# @cand this is is the list of all possible strings that differ by characters i,j, from w that match with a word in a dictionary 


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
  return (cand) 
}
