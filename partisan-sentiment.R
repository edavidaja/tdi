library(tidyverse)
library(tidytext)
library(magrittr)
library(jsonlite)
library(lubridate)
library(here)
library(ggrepel)

# tweets <- "https://files.pushshift.io/misc/political_tweets.ndjson.xz"

# download.file(
#   url = tweets,
#   destfile = here("raw", basename(tweets))
# )

# unxz in WSL
# unxz political_tweets.ndjson.xz

# pull the frst and last
# head -n 1 political_tweets.ndjson > some_tweets.ndjson


# c("raw/first.ndjson", "raw/last.ndjson") %>%
#   map(read_json) %>%
#   walk(str)
tweets <- stream_in(file("raw/tweet_text.ndjson"))
# write_rds(tweets, "raw/tweets.rds")

remove_regex <- "&amp;|&lt;|&gt;"

x <- tweets %>%
  # mutate(
  #   time_of_tweet = parse_date_time(tweeted_at, "abdHMszY"),
  #   tweet_month = month(time_of_tweet),
  #   tweet_year = year(time_of_tweet)
  # ) %>%
  # filter(!str_detect(full_text, "^RT")) %>%
  mutate(full_text = str_remove_all(full_text, remove_regex)) %>%
  unnest_tokens(word, full_text, token = "tweets") %>%
  anti_join(stop_words)

library(mongolite)

m <- mongo("members", "voteview", url = "mongodb://localhost:27017")
m$import(file(here::here("raw", "voteview", "dump", "voteview", "voteview_members.bson")), bson = TRUE)

members_115 <- m$find(
  '{"congress":115}',
  fields = '{"twitter" : true, "chamber": true, "party_code" : true, "nominate.dim1": true, "bioname" : true, "_id": false }'
)
scores <- members_115$nominate
members_115$nominate <- NULL
members_115 %<>% bind_cols(members_115, scores) %>% as_tibble()

members <-
  members_115 %>%
  mutate(
    party_code = as.double(party_code),
    # "independents"
    party_code = case_when(
      twitter == "SenAngusKing" ~ 100,
      twitter == "SenSanders" ~ 100,
      TRUE ~ party_code
    ),
    party = case_when(
      party_code == 100 ~ "D",
      party_code == 200 ~ "R"
    )
  ) %>%
  select(party, left_right = dim1, handle = twitter, bioname, chamber)

tidied <- inner_join(x, members, by = "handle")

freq <- tidied %>%
  group_by(party) %>%
  count(word, sort = TRUE) %>%
  left_join(tidied %>% group_by(party) %>% summarise(total = n())) %>%
  mutate(freq = n/total) %>%
  select(party, word, freq) %>%
  spread(party, freq)

library(scales)
freq %>%
  drop_na() %>%
ggplot(., aes(D, R)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  labs(
    title = "Building a partisan vocabulary:",
    subtitle = "Relative frequencies of words mentioned by members of Congress on Twitter"
  )


## salient words by chamber and party

tfidf <-
  with_party %>%
  mutate(chamber_party = str_c(party,"-", chamber)) %>%
  group_by(chamber_party, word) %>%
  summarise(
    n = n(),
    avg_partisan_affect = mean(left_right, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  bind_tf_idf(word, chamber_party, n)

top_tfidf <- tfidf %>%
  group_by(chamber_party) %>%
  top_n(100, tf_idf)

top_tfidf %>%
  drop_na(word) %>%
  filter(chamber_party != "R-President") %>%
  ggplot(., aes(avg_partisan_affect, tf_idf, label = word)) +
  geom_text_repel(
    segment.alpha = 0,
    aes(colour=avg_partisan_affect)
    ) +
  facet_wrap("chamber_party", nrow = 2) +
  scale_colour_distiller(
    palette = "RdBu",
    guide = guide_colourbar(direction = "horizontal",
    title.position ="top")
    )+
  labs(
    title = "Building a partisan vocabulary:",
    subtitle = "What are the most salient words tweeted by Democrats and Republicans?",
    x = "Average DW Nominate score (by word)",
    y = "TF / IDF"
  ) +
  theme_minimal()
