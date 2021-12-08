library(gutenbergr)
library(tidyverse)
library(tidytext)
library(stringi)
library("magrittr")
data(stop_words)

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

# tidy_dq %>%
#   count(word, sort = TRUE) 


#Sentiment Analysis Start
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

nrc_sad <- get_sentiments("nrc") %>% 
  filter(sentiment == "sadness")

dq_sentiment <- tidy_dq %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = chapter, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

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


lex_combined <- bind_rows(afinn, bing_and_nrc)



bing_word_counts <- tidy_dq %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()




#########################################################
#True Numbers
source("Book2TN-v6A-1.R")
library(tnum)
tnum.authorize("mssp1.bu.edu")
tnum.setSpace("test2")


text2 <- readLines("don_quixote_text_fix.txt")
text2  <- stri_enc_toutf8(text2)

#Ingest book into truenumbers. Commented out to avoid repeating the process
# tnBooksFromLines(text2, "cervantes/don_quixote_v4", startLine = 2604)

q1 <- tnum.query(query = "cervantes/don_quixote_v4/# has *", max = 31478 )
df1 <- tnum.objectsToDf(q1)

##########################################################
#sentimentr
library(sentimentr)

dq_text <- filter(df1, property == "text")

sents <- sentiment(get_sentences(dq_text))

dq_text %<>% separate(col = subject, into = c("author", "book", "chapter", "paragraph","sentence"), sep = "/", fill = "right")

sents_para <- sentiment_by(get_sentences(dq_text), by = c('chapter', 'paragraph')) %>%
  filter(!is.na(paragraph)) %>%
  mutate(index = 1:2088)

sents_chapter <- sentiment_by(get_sentences(dq_text), by = 'chapter') %>%
  filter(word_count > 100) %>%
  mutate(chapter = substr(chapter, 9,12)) %>%
  mutate(chapter = as.numeric(chapter)) %>%
  mutate(sentiment = ave_sentiment) %>%
  select(chapter, sentiment)%>%
  mutate(lex = "sentimentr")

dq_sentiment3 <- mutate(dq_sentiment, chapter = index + 1, sentiment) %>%
  select(chapter, sentiment)%>%
  mutate(lex = "Bing et al.")

bing_and_sents <- bind_rows(dq_sentiment3, sents_chapter)

dq_sentiment2 <- mutate(dq_sentiment, chapter = index + 1, sentiment = sentiment +50) %>%
  select(chapter, sentiment)%>%
  mutate(lex = "Bing et al.")
bing_and_sents2 <- bind_rows(dq_sentiment2, sents_chapter)


profs_chapter <- profanity_by(get_sentences(dq_text), by = 'chapter') %>%
  filter(word_count > 100) %>%
  mutate(chapter = substr(chapter, 9,12)) %>%
  mutate(chapter = as.numeric(chapter))

# ggplot(profs_chapter, aes(chapter, profanity_count)) +
#   geom_col(show.legend = FALSE)



emos_chapter <- emotion_by(get_sentences(dq_text), by = 'chapter') %>%
  filter(word_count > 100) %>%
  mutate(chapter = substr(chapter, 9,12)) %>%
  mutate(chapter = as.numeric(chapter)) %>%
  filter(emotion_type %in% c("anger", "fear", "joy", "sadness"))


