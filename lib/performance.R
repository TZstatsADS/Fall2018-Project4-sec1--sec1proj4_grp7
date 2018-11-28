
performance <- function(truth, reconstruction, outName){
  nDocs <- length(unique(truth[,4]))
  
  precision <- c()
  
  for(d in 1:nDocs){
    nLines <- length(unique(truth[truth[,4] == as.character(d), 3 ]))
    for(l in 1:nLines){
      nWordsTruth <- length(unique(truth[(truth[,4] == as.character(d)) & 
                                           (truth[,3] == as.character(l)),
                                         2 ]))
      nWordsOCR <- length(unique(reconstruction[(reconstruction[,4] == as.character(d)) & 
                                                (reconstruction[,3] == as.character(l)),
                                              2 ]))
      if(nWordsTruth == nWordsOCR){
        for(w in 1:nWordsTruth){
          truthWord <- truth[(truth[,4] == as.character(d)) & 
                               (truth[,3] == as.character(l)) &
                               (truth[,2] == as.character(w)),
                             1 ]
          ocrWord <- reconstruction[(reconstruction[,4] == as.character(d)) & 
                                    (reconstruction[,3] == as.character(l)) &
                                    (reconstruction[,2] == as.character(w)),
                                  1 ]
          nCharTruth <- nchar(truthWord)
          nCharOCR <- nchar(ocrWord)
          if(nCharTruth == nCharOCR){
            corrects <- sum(unlist(strsplit(truthWord, split = "")) ==
                              unlist(strsplit(ocrWord, split = "")))
            precision <- c(precision, corrects / nCharTruth)
            print(l)
          }
        }
      }
    }
  }
  # save(precision, file = paste0("../output/", outName, ".RData"))
  
  assign(outName, precision)
  save(list=outName, file = paste0("../output/", outName, ".RData"))
  return(eval(as.name(paste(outName))))
}