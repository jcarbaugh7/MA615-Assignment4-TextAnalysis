library(gutenbergr)
library(tidyverse)
library(tidytext)
library(stringi)
library("magrittr")
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
  group_by(index = chapter) %>% 
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
  count(method, index = chapter, sentiment) %>%
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
  slice_max(n, n = 20) %>% 
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


#########################################################
#True Numbers
source("Book2TN-v6A-1.R")
library(tnum)
tnum.authorize("mssp1.bu.edu")
tnum.setSpace("test2")

# writeLines(don_quixote$text, "don_quixote_text.txt")
# text <- don_quixote$text
# str(text)
# text2 <- readLines("don_quixote_text_fix.txt")
# text2  <- stri_enc_toutf8(text2)
# str(text2)
# tnBooksFromLines(text2, "cervantes/don_quixote_v4", startLine = 2604)
tnum.getDBPathList(taxonomy="subject", levels = 2)

q1 <- tnum.query(query = "cervantes/don_quixote_v4/# has *", max = 31478 )
df1 <- tnum.objectsToDf(q1)

##########################################################
#sentimentr
library(sentimentr)

dq_text <- filter(df1, property == "text")
line_ex <- dq_text$string.value[2178]
sentiment(line_ex)
sents <- sentiment(get_sentences(dq_text))

dq_text %<>% separate(col = subject, into = c("author", "book", "chapter", "paragraph","sentence"), sep = "/", fill = "right")

sents_chapter <- sentiment_by(get_sentences(dq_text), by = 'chapter')
sents_chapter <- filter(sents_chapter, word_count > 100) %>%
  mutate(sents_chapter, chapter = substr(chapter, 9,12)) %>%
  mutate(chapter = as.numeric(chapter))

ggplot(sents_chapter, aes(chapter, ave_sentiment)) +
  geom_col(show.legend = FALSE)
