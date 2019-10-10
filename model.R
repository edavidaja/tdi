library(tidymodels)
library(tidytext)
library(textrecipes)
library(furrr)
plan(multicore)

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

train_data <- juice(rec)
val_data <- bake(rec, new_data = validation_data)

param_grid <- grid_regular(
  range_set(trees, c(50, 250)),
  range_set(mtry, c(1, 15)), levels = 5
  )

rf_spec <- rand_forest("classification", mtry = varying(), trees = varying()) %>%
  set_engine("randomForest")

param_grid <- param_grid %>%
  mutate(specs = merge(., rf_spec))

fit_one_spec <- function(model) {
  model %>%
    fit(author ~ ., data = train_data) %>%
    predict(new_data = val_data) %>%
    mutate(truth = val_data$author) %>%
    accuracy(truth, .pred_class) %>%
    pull(.estimate)
}

final <- param_grid %>%
  mutate(accuracy = future_map_dbl(specs, fit_one_spec))

grid_results <-
final %>%
  mutate_at(vars(trees:mtry), factor) %>%
  ggplot(aes(mtry, trees, fill = accuracy)) +
  geom_tile() +
  scale_fill_viridis_c()

ggsave("grid_results.png", plot = grid_results)
