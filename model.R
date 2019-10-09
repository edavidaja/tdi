library(tidymodels)
library(tidytext)
library(textrecipes)
library(furrr)

tweets <- qs::qread(
  here::here("data", "tagged_tweets.qs"),
  nthreads = parallel::detectCores() / 2
    )

split_tweets <- tweets %>% initial_split(strata = party_code)

training_data <- training(split_tweets)
validation_data <- testing(split_tweets)

# as classification problem:
# predict party based on tweets
# todo(ajae): predict dim1 based on text
rec <- recipe(party_code ~ full_text, data = training_data) %>%
  step_tokenize(full_text, token = "tweets") %>%
  step_tokenfilter(full_text, min_times = 10, max_tokens = 250) %>%
  step_tfidf(full_text) %>%
  prep()
