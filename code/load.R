library(tidyverse)
library(tidytext)
library(magrittr)
library(lubridate)
library(scales)
library(stringr)


headlines <- read_csv('data/abcnews-date-text.csv') %>% 
  mutate(date = ymd(as.character(publish_date)),
         year = year(as.character(date)))

words <- headlines %>% 
  unnest_tokens(word, headline_text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  ungroup()

words %>% 
  top_n(10, n) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#TD-IF
wordsByYear <- headlines %>% 
  unnest_tokens(word, headline_text) %>% 
  count(year, word, sort = TRUE) %>% 
  ungroup()

totalWords <- wordsByYear %>% 
  group_by(year) %>% 
  summarize(total = sum(n))

headlineWords <- left_join(wordsByYear, totalWords)

headlineWords %>% 
  ggplot(aes(n/total, fill = year)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~year, ncol = 2, scales = "free_y")


headlineWords %<>%
  bind_tf_idf(word, year, n)

headlineWords %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, rev(unique(word)))) %>% 
  top_n(20, tf_idf) %>% 
  filter(!grepl('\\d', word)) %>% 
  ggplot(aes(word, tf_idf, fill = year)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip() +
  facet_wrap(~year, ncol = 2, scales = "free")

headlines %>% 
  filter(year == 2015) %>% 
  unnest_tokens(bigram, headline_text, token = "ngrams", n = 3) %>% 
  count(bigram, sort = TRUE) %>% 
  ungroup()