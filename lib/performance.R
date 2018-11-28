
# #original
# ground_truth_vec <- str_split(paste(current_ground_truth_txt, collapse = " ")," ")[[1]] #1078
# old_intersect_vec <- vecsets::vintersect(tolower(ground_truth_vec), tolower(tesseract_vec)) #607
# new_intersect_vec <- vecsets::vintersect(tolower(ground_truth_vec), tolower(tesseract_delete_error_vec)) #600
# ground_truth_vec_chr <- unlist(strsplit(gsub(pattern = " ","", ground_truth_vec),""))
# old_intersect_vec_chr <- vecsets::vintersect(tolower(ground_truth_vec_chr), tolower(tesseract_vec)) #607
# new_intersect_vec_chr <- vecsets::vintersect(tolower(ground_truth_vec_chr), tolower(tesseract_delete_error_vec)) #600
# 
# OCR_performance_table <- data.frame("Tesseract" = rep(NA,4),
#                                     "Tesseract_with_postprocessing" = rep(NA,4))
# row.names(OCR_performance_table) <- c("word_wise_recall","word_wise_precision",
#                                       "character_wise_recall","character_wise_precision")
# OCR_performance_table["word_wise_recall","Tesseract"] <- length(old_intersect_vec)/length(ground_truth_vec)
# OCR_performance_table["word_wise_precision","Tesseract"] <- length(old_intersect_vec)/length(tesseract_vec)
# OCR_performance_table["word_wise_recall","Tesseract_with_postprocessing"] <- length(new_intersect_vec)/length(ground_truth_vec)
# OCR_performance_table["word_wise_precision","Tesseract_with_postprocessing"] <- length(new_intersect_vec)/length(tesseract_delete_error_vec)
# OCR_performance_table["character_wise_recall","Tesseract"] <- length(old_intersect_vec_chr)/length(ground_truth_vec_chr)
# OCR_performance_table["character_wise_precision","Tesseract"] <- length(old_intersect_vec_chr)/length(tesseract_vec)
# OCR_performance_table["character_wise_recall","Tesseract_with_postprocessing"] <- length(new_intersect_vec_chr)/length(ground_truth_vec_chr)
# OCR_performance_table["character_wise_precision","Tesseract_with_postprocessing"] <- length(new_intersect_vec_chr)/length(tesseract_delete_error_vec)
# kable(OCR_performance_table, caption="Summary of OCR performance")


#New

for (i in 1:100){
  ##Word evaluation
  ground_truth_vec[i] <- str_split(paste(current_ground_truth_txt[i], collapse = " ")," ")[[1]] #1078
  old_intersect_vec[i] <- vecsets::vintersect(tolower(ground_truth_vec[i]), tolower(tesseract_vec[i])) #607
  new_intersect_vec[i] <- vecsets::vintersect(tolower(ground_truth_vec[i]), tolower(tesseract_delete_error_vec[i])) #600
}

OCR_performance_table <- data.frame("Tesseract" = rep(NA,4),
                                    "Tesseract_with_postprocessing" = rep(NA,4))
row.names(OCR_performance_table) <- c("word_wise_recall","word_wise_precision",                                                                                   "character_wise_recall","character_wise_precision")
sum1 <- 0
sum2 <- 0
sum3 <- 0
sum4 <- 0
for (j in 1:100){
  sum1 <- sum1+(length(old_intersect_vec[j])/length(ground_truth_vec[j]))
  sum2 <- sum2+(length(old_intersect_vec[j])/length(tesseract_vec[j]))
  sum3 <- sum3+(length(new_intersect_vec[j])/length(ground_truth_vec[j]))
  sum4 <- sum4+(length(new_intersect_vec[j])/length(tesseract_delete_error_vec[j]))
}

OCR_performance_table["word_wise_recall","Tesseract"] <- mean(sum1)
OCR_performance_table["word_wise_precision","Tesseract"] <- mean(sum2)
OCR_performance_table["word_wise_recall","Tesseract_with_postprocessing"] <- mean(sum3)
OCR_performance_table["word_wise_precision","Tesseract_with_postprocessing"] <- mean(sum4)

##Character evaluation
for (k in 1:100){
  ground_truth_vec_chr[k] <- unlist(strsplit(gsub(pattern = " ","", ground_truth_vec[k]),""))
  tesseract_vec_chr[k] <- unlist(strsplit(gsub(pattern = " ","", tesseract_vec[k]),""))
  tesseract_delete_error_vec_chr[k] <- unlist(strsplit(gsub(pattern = " ","", tesseract_delete_error_vec[k]),""))
  old_intersect_vec_chr[k] <- vecsets::vintersect(tolower(ground_truth_vec_chr[k]),tolower(tesseract_vec_chr[k])) #607
  new_intersect_vec_chr[k] <- vecsets::vintersect(tolower(ground_truth_vec_chr[k]),tolower(tesseract_delete_error_vec_chr[k])) #600
}

sum1_chr <- 0
sum2_chr <- 0
sum3_chr <- 0
sum3_chr <- 0
for (l in 1:100){
  sum1_chr <- sum1_chr + (length(old_intersect_vec_chr[l])/length(ground_truth_vec_chr[l]))
  sum2_chr <- sum2_chr + (length(old_intersect_vec_chr[l])/length(tesseract_vec_chr[l]))
  sum3_chr <- sum3_chr + (length(new_intersect_vec_chr[l])/length(ground_truth_vec_chr[l]))
  sum4_chr <- sum4_chr + (length(new_intersect_vec_chr[l])/length(tesseract_delete_error_vec_chr[l]))
}

OCR_performance_table["character_wise_recall","Tesseract"] <- mean(sum1_chr)
OCR_performance_table["character_wise_precision","Tesseract"] <- mean(sum2_chr)
OCR_performance_table["character_wise_recall","Tesseract_with_postprocessing"] <- mean(sum3_chr)
OCR_performance_table["character_wise_precision","Tesseract_with_postprocessing"] <- mean(sum4_chr)

kable(OCR_performance_table, caption="Summary of OCR performance")


