library(mongolite)
library(lubridate)
library(tidyverse)
library(tidytext)
library(rvest)

dl <- c(
  tweets =  "https://files.pushshift.io/misc/political_tweets.ndjson.xz",
  voteview = "https://voteview.com/static/db/current.zip"
)

walk2(
  dl,
  names(dl),
  ~download.file(
    url = .x,
    destfile = here("data-raw", basename(.y))
    )
  )

unzip(here("data-raw", "current.zip"), overwrite = TRUE, exdir = here("data-raw", "voteview"))

tweet_file <- here::here("data", "tweets.qs")
if (file.exists(tweet_file)) {
  tweets <- qs::qread(tweet_file)
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

  qs::qsave(tweets, tweet_file)
  m$disconnect()
  system(paste0("docker stop ", container))
}

voteview_file <- here::here("data", "voteview_115_congress.qs")
if (file.exists(voteview_file)) {
  cong_115 <- qs::qread(voteview_file)
} else {
  container <- system("docker run -d --rm -p 27017:27017 mongo")
  m <- mongo("tweets", "capstone", url = "mongodb://localhost:27017")
  m$import(
    file(here::here(
      "data-raw", "voteview", "dump", "voteview", "voteview_members.bson"
      )),
    bson = TRUE
    )

  cong_115 <- m$find(
    query ='{"congress": 115}',
    fields = '{
      "twitter" : true,
      "chamber" : true,
      "party_code" : true,
      "nominate" : true,
      "bioname"  : true,
      "_id": false
      }'
    )
  qs::qsave(cong_115, voteview_file)
  m$disconnect()
  system(paste0("docker stop ", container))
}

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

tagged_tweets <- inner_join(tweets, cong_115, by = c("user" = "twitter"))

qs::qsave(tagged_tweets, here::here("data", "tagged_tweets.qs"))

demonyms_link <- "https://en.wikipedia.org/wiki/List_of_demonyms_for_U.S._states_and_territories"

footnotes <- regex(
  "
  (   # start capture group
  \\( # opening parens
  |   # or
  \\[ # opening bracket
  )   # end capture group
  .+  # anything within parens
  (   # cgroup2
  \\] # end bracket
  |   # or
  \\) # end parens
  )   # cgroup2
  ",
  comments = TRUE
)

demonyms <- read_html(demonyms_link) %>%
  html_node("table") %>%
  html_table() %>%
  rename(state = 1, demonym = 2, alts = 3) %>%
  mutate(alts = str_remove_all(alts, footnotes)) %>%
  separate_rows(alts, sep = ",|\\bor\\b") %>%
  mutate(
    alts = str_remove_all(alts, "^[A-z ]+:")
  ) %>%
  mutate_all(str_trim) %>%
  flatten_chr() %>%
  unique()

qs::qsave(demonyms[demonyms != ""], here::here("data", "demonyms.qs"))
