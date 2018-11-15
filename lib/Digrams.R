load("../output/dict.RData")

dict <- as.matrix(dict)

dict <- dict[nchar(dict) > 1] # no single character words
dict <- unique(dict)

for(i in 1:length(dict)){
  comb <- nchar(dict[i]) * (nchar(dict[i])-1) / 2
}

