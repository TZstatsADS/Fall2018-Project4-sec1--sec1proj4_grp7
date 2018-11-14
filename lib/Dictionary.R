for( iin files)

word_ground_txt <-  paste(current_ground_truth_txt, collapse = " ")
bag_words <- str_split(word_ground_txt," ")

corpus <- VCorpus(VectorSource(hm_data$cleaned_hm))%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(removeWords, character(0))%>%
  tm_map(stripWhitespace)

