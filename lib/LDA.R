
topicModelling <- function(files){
  
  textInFiles <- lapply(files, function (filename) read.table(filename, sep="\t", stringsAsFactors = F))
  textInFiles <- lapply(textInFiles, function(x) x$V1)
  
  a <- Corpus(VectorSource(textInFiles), readerControl = list(language="lat")) #specifies the exact folder where my text file(s) is for analysis with tm.
  a <- tm_map(a, tolower)
  a <- tm_map(a , stripWhitespace)
  a <- tm_map(a, removeWords, stopwords("en"))
  # warnings are not relevant: https://stackoverflow.com/questions/51081415/transformation-drops-documents-error-in-r
  
  adtm <- DocumentTermMatrix(a)
  l <- LDA(adtm, k = 30, control = list(seed = 1234))
  
  save(l, file = "../output/LDA.RData")
  return(l)
}




# plotting

# ap_top_terms <- ap_topics %>%
#   group_by(topic) %>%
#   top_n(20, beta) %>%
#   ungroup() %>%
#   arrange(topic, -beta)

# ap_top_terms %>%
#   mutate(term = reorder(term, beta)) %>%
#   ggplot(aes(term, beta, fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free") +
#   coord_flip()
# 
# beta_spread <- ap_topics %>%
#   mutate(topic = paste0("topic", topic)) %>%
#   spread(topic, beta) %>%
#   filter(topic1 > .001 | topic2 > .001) %>%
#   mutate(log_ratio = log2(topic2 / topic1))

# beta_spread




