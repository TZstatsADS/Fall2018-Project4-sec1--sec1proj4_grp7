
#####Output text files for each doc######

outputtext <- function(ocrcorrect){
  ndoc <- 20
  docnumber <-  as.numeric(ocrcorrect[,4])
  linenumber <- as.numeric(ocrcorrect[,3])
  
  for (doc in 1:ndoc){
    nline <- length(unique(as.numeric(ocrcorrect[docnumber == doc,3])))
    data <- c()
    
    for (line in 1:nline){
      datarow <- paste(paste(ocrcorrect[docnumber == doc & linenumber == line,1], collapse = ""))
      data <- c(data, datarow)
    }
  write.table(data, file = paste("../output/correct/ocrcorrect", doc,".txt"),
              sep = "", row.names = FALSE, col.names = FALSE)
}
}
