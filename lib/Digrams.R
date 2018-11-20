load("../output/dict.RData")
load("../output/dict2.RData")

dict <- as.matrix(dict)
dict <- dict[nchar(dict) > 1] # no single character words
dict <- unique(dict)


dict2 <- as.matrix(dict2)
dict2 <- cbind(dict2, rep(0, nrow(dict2)))
colnames(dict2) <- c("word", "error")



