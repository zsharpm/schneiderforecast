library(rsample)
library(recipes)
library(purrr)
library(tidyquant)
library(forecast)
library(timetk)
library(sweep)
library(yardstick)

# random sampling
initial <- initial_split(training_data %>% filter(forecastid == 1L), prop = 0.75)

# rolling origin sampling
roll_rs <- rolling_origin(
  training_data %>% filter(forecastid == 1L),
  initial = 60 * 4,
  assess = 60,
  cumulative = TRUE, # when true each resample contains cumulative data
  skip = 9 # if 0, rolling origin will increment by one day
  )

# plot first sample
roll_rs$splits[[1]] %>%
  analysis() %>%
  mutate(set = "analysis") %>%
  bind_rows(roll_rs$splits[[1]] %>% assessment() %>% mutate(set = "assessment")) %>%
  ggplot(aes(x = timestamp, y = value, color = set)) +
  geom_line()

# plot the second sample - each rolling sample increments forward one day
roll_rs$splits[[2]] %>%
  analysis() %>%
  mutate(set = "analysis") %>%
  bind_rows(roll_rs$splits[[2]] %>% assessment() %>% mutate(set = "assessment")) %>%
  ggplot(aes(x = timestamp, y = value, color = set)) +
  geom_line()

# time series summary
roll_rs$splits[[1]] %>%
  analysis() %>%
  tk_index() %>%
  tk_get_timeseries_summary() %>%
  glimpse()

