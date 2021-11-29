library(gutenbergr)
library(tidyverse)
library(tidytext)
data(stop_words)
works <- gutenberg_works()
cerv <- filter(works, grepl("Cervantes", author))
#id = 996

don_quixote <- gutenberg_download(996)



dq_fixed <- don_quixote %>%
  mutate(linenumber = row_number()) %>%
  select(-gutenberg_id) %>%
  filter(linenumber >= 578) %>%
  mutate(chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE))))

tidy_dq <- dq_fixed %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_dq %>%
  count(word, sort = TRUE) 


#Sentiment Analysis Start
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")
tidy_dq %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

dq_sentiment <- tidy_dq %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = chapter, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(dq_sentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE)


afinn <- tidy_dq %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 200) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  tidy_dq %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  tidy_dq %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 200, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")



bing_word_counts <- tidy_dq %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)


library(wordcloud)

tidy_dq %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))



#True Numbers
source("Book2TN-v3 - hw.R")
library(tnum)

tnBooksFromLines(don_quixote$text, "cervantes/don_quixote")
tnum.getDBPathList(taxonomy="subject", levels = 2)

q1 <- tnum.query(query = "cervantes# has *", max = 5000)
df1 <- tnum.objectsToDf(q1)


writeLines(don_quixote$text, "don_quixote_text.txt")
