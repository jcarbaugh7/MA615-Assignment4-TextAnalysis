# library(tidyverse)
# library(tidytext)
# library(stringi)
# library("magrittr")
library(wordcloud)
data(stop_words)

#TASK2----------------------------------------------
nrc_joy_plot <- function(df, nrc_joy){
  p <- df %>%
    inner_join(nrc_joy) %>%
    count(word, sort = TRUE) %>%
    filter(n > 100) %>%
    ggplot(aes(n, reorder(word, n))) +
    geom_col(show.legend = FALSE) +
    labs(x = "Count",
         y = NULL) + 
    ggtitle(label = "Count of NRC Joy words")
  return(p)
}

bing_sent_plot <- function(df){
  p <- ggplot(dq_sentiment, aes(index, sentiment)) +
    geom_col(show.legend = FALSE)
  return(p)
}

all_sent_plot <- function(df){
  p <- ggplot(df, aes(index, sentiment, fill = method)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~method, ncol = 1, scales = "free_y")
}

bing_word_count_plot <- function(df){
  p <- df %>%
    group_by(sentiment) %>%
    slice_max(n, n = 20) %>% 
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(n, word, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(x = "Contribution to sentiment",
         y = NULL)
  return(p)
}

word_cloud <- function(tidy_dq){
  tidy_dq %>%
    anti_join(stop_words) %>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 100))
}




#TASK3------------------------------------------------






