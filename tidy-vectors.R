library(tidyverse)
library(tidytext)
library(widyr)
library(magrittr)
library(jsonlite)
library(lubridate)
library(here)
library(ggrepel)

tweets <- stream_in(file("raw/tweet_text.ndjson"))
# write_rds(tweets, "raw/tweets.rds")

remove_regex <- "&amp;|&lt;|&gt;"

tweets %<>%
  filter(!str_detect(full_text, "^RT")) %>%
  mutate(
    full_text = str_remove_all(full_text, remove_regex),
    tweet_id = row_number()
  )

slide_windows <- function(tbl, doc_var, window_size) {
  # each word gets a skipgram (window_size words) starting on the first
  # e.g. skipgram 1 starts on word 1, skipgram 2 starts on word 2

  each_total <- tbl %>%
    group_by(!!doc_var) %>%
    mutate(doc_total = n(),
           each_total = pmin(doc_total, window_size, na.rm = TRUE)) %>%
    pull(each_total)

  rle_each <- rle(each_total)
  counts <- rle_each[["lengths"]]
  counts[rle_each$values != window_size] <- 1

  # each word get a skipgram window, starting on the first
  # account for documents shorter than window
  id_counts <- rep(rle_each$values, counts)
  window_id <- rep(seq_along(id_counts), id_counts)


  # within each skipgram, there are window_size many offsets
  indexer <- (seq_along(rle_each[["values"]]) - 1) %>%
    map2(rle_each[["values"]] - 1,
         ~ seq.int(.x, .x + .y)) %>%
    map2(counts, ~ rep(.x, .y)) %>%
    flatten_int() +
    window_id

  tbl[indexer, ] %>%
    bind_cols(data_frame(window_id)) %>%
    group_by(window_id) %>%
    filter(n_distinct(!!doc_var) == 1) %>%
    ungroup
}

tidy_pmi <- tweets %>%
  unnest_tokens(word, full_text, token = "tweets") %>%
  add_count(word) %>%
  filter(n >= 20) %>%
  select(-n) %>%
  slide_windows(quo(tweet_id), 5) %>%
  pairwise_pmi(word, window_id)


tidy_word_vectors <- tidy_pmi %>%
  widely_svd(item1, item2, pmi, nv = 256, maxit = 1000)

nearest_synonyms <- function(df, token) {
  df %>%
    widely(~ . %*% (.[token, ]), sort = TRUE, maximum_size = NULL)(item1, dimension, value) %>%
    select(-item2)
}

tidy_word_vectors %>%
  nearest_synonyms("obama")

analogy <- function(df, token1, token2, token3) {
  df %>%
    widely(~ . %*% (.[token1, ] - .[token2, ] + .[token3, ]), sort = TRUE, maximum_size = NULL)(item1, dimension, value) %>%
    select(-item2)

}

# operating systems
tidy_word_vectors %>%
  analogy("president", "obama", "republican")

tidy_word_vectors %>%
  filter(dimension %in% c(25:48)) %>%
  group_by(dimension) %>%
  top_n(12, abs(value)) %>%
  ungroup %>%
  mutate(item1 = reorder(item1, value)) %>%
  group_by(dimension, item1) %>%
  arrange(desc(value)) %>%
  ungroup %>%
  mutate(item1 = factor(paste(item1, dimension, sep = "__"),
                        levels = rev(paste(item1, dimension, sep = "__"))),
         dimension = factor(paste0("Dimension ", dimension),
                            levels = paste0("Dimension ", as.factor(1:24)))) %>%
  ggplot(aes(item1, value, fill = dimension)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~dimension, scales = "free_y", ncol = 4) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  scale_fill_viridis_d() +
  coord_flip() +
  labs(x = NULL, y = "Value",
       title = "First 24 principal components of the Twitter corpus",
       subtitle = "Top words contributing to the components that explain the most variation")
