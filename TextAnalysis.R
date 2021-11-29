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
tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)
