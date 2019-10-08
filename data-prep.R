library(mongolite)
library(lubridate)
library(tidyverse)
library(tidytext)
library(tidylo)


if (file.exists("data/tweets.qs")) {
  tweets <- qs::qread("data/tweets.qs")
} else {
  container <- system("docker run -d --rm -p 27017:27017 mongo")
  m <- mongo("tweets", "capstone", url = "mongodb://localhost:27017")
  m$import(file(here::here("data-raw", "tweets", "political_tweets.ndjson")))

  tweets <- m$find(
    query = '{
      "lang": "en",
      "retweeted": false
      }',
    fields = '{
      "user.screen_name" : true,
      "full_text": true,
      "created_at": true,
      "retweet_count": true,
      "favorite_count": true
      }'
    )

  tweets <- tweets %>%
    mutate(
      user = flatten_chr(user)
    )

  qs::qsave(tweets, "data/tweets.qs")
  m$disconnect()
  system(paste0("docker stop ", container))
}


cong_115 <- qs::qread("data/voteview_115_congress.qs")
cong_115 <- cong_115 %>%
  filter(!is.na(twitter), twitter != "") %>%
  mutate(
    # "independents"
    party_code = case_when(
      twitter == "SenAngusKing" ~ 100L,
      twitter == "SenSanders" ~ 100L,
      TRUE ~ party_code
    ),
    party_code = case_when(
      party_code == 100L ~ "D",
      party_code == 200L ~ "R"
    )
  )

tweets <- inner_join(tweets, cong_115, by = c("user" = "twitter"))

remove_regex <- "&amp;|&lt;|&gt;"

tweets <- tweets %>%
  mutate(
    created_at = parse_date_time(created_at, "abdHMszY")
  ) %>%
  # drop retweets
  filter(!str_detect(full_text, "^RT")) %>%
  mutate(full_text = str_remove_all(full_text, remove_regex)) %>%
  unnest_tokens(word, full_text, token = "tweets") %>%
  group_by(word) %>%
  filter(n() > 10) %>%
  ungroup() %>%
  anti_join(get_stopwords())

qs::qsave(tweets, "data/tokenized-tweets.qs")

word_counts <-
  tweets %>%
  count(party_code, word, sort = TRUE) %>%
  bind_tf_idf(word, party_code, n) %>%
  bind_log_odds(word, party_code, n)

top_15 <-
word_counts %>%
  filter(log_odds > 1) %>%
  mutate(word = fct_reorder(word, log_odds))

ggplot(top_15, aes(word, log_odds, color = party_code)) +
  geom_point() +
  facet_wrap(~party_code, scales = "free_y") +
  coord_flip()
