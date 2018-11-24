library(tm)
library(topicmodels)
library(tidytext)
library(tidyverse)


a  <- Corpus(DirSource("../data/ground_truth/"), readerControl = list(language="lat")) #specifies the exact folder where my text file(s) is for analysis with tm.
a <- tm_map(a, content_transformer(tolower))
sw <- c("the", "and", "for", "that", "with", "this", "will", "are", "has", "not", "our", "have", "was", "which")
a <- tm_map(a , stripWhitespace)
a <- tm_map(a, removeWords, sw)

adtm <- DocumentTermMatrix(a)
l <- LDA(adtm, k = 4, control = list(seed = 1234))

ap_topics <- tidy(l, matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread
