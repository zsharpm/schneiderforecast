

library(tidyverse)

submission_format <- read_csv("data/Power_Laws_Forecasting_Energy_Consumption_-_Submission_Format.csv")
names(submission_format) <- str_to_lower(names(submission_format))

trn <- read_rds(here::here("data/trn.rds"))
tst <- read_rds(here::here("data/tst.rds"))

tst <- tst %>%
  mutate(hour = as.factor(hour)) %>%
  select(
   forecastid,
   month,
   wday,
   hour,
   value
  )

 trn_agrgt <- trn %>%
  #filter(forecastid == 188) %>%
  mutate(
    hour = as.factor(hour),
    value = floor(log(value + 1) * 10) / 10
  ) %>%
  group_by(
    forecastid,
    month,
    wday,
    hour
  ) %>%
  summarize(
   value = mean(value, na.rm = T)
  ) %>%
  ungroup() %>%
  distinct()

trn_forecast_agrgt <- trn %>% group_by(forecastid) %>%
  mutate(
    value = floor(log(value + 1) * 10) / 10
  ) %>%
  summarize(
    value2 = mean(value, na.rm = T)
  ) %>%
  ungroup()

agrgt <- trn %>%
  mutate(
    value = floor(log(value + 1) * 10) / 10
  ) %>%
  summarize(
    value3 = mean(value, na.rm = T)
  ) %>%
  ungroup() %>%
  pull(value3)

# ggplot(trn_agrgt %>% filter(forecastid == 6603), aes(x = hour, y = value, color = wday)) + geom_point() + facet_wrap(~ month)

preds <- tst %>%
  select(
   forecastid,
   month,
   wday,
   hour
  ) %>%
  mutate(hour = as.factor(hour)) %>%
  left_join(trn_agrgt, by = c("forecastid", "month", "wday", "hour"))

submit_agrgt <- submission_format %>%
  mutate(rn = row_number()) %>%
  timetk::tk_augment_timeseries_signature() %>%
  select(
    rn,
    obs_id,
    siteid,
    timestamp,
    forecastid,
    month,
    wday,
    hour
  ) %>%
  mutate(
    forecastid = as.factor(forecastid),
    month = as.factor(month),
    wday = as.factor(wday),
    hour = as.factor(hour)
  ) %>%
  left_join(trn_agrgt, by = c("forecastid", "month", "wday", "hour")) %>%
  left_join(trn_forecast_agrgt, by = "forecastid") %>%
  mutate(
   value = ifelse(is.na(value), value2, value),
   value = ifelse(is.na(value), agrgt, value)
  ) %>%
  mutate(
   value = exp(value) - 1
  ) %>%
  arrange(rn) %>%
  select(
    obs_id,
    SiteId = siteid,
    Timestamp = timestamp,
    ForecastId = forecastid,
    Value = value
  )

nrow(submit_agrgt) == nrow(submission_format)

sum(is.na(submit_agrgt$value))

write_csv(submit_agrgt, here::here(paste0("submissions/submission_", as.integer(Sys.time()), ".csv")))

submit %>%
  bind_cols(submit_agrgt %>% select(Value2 = Value)) %>%
  mutate(diff = Value2 - Value) %>%
  arrange(desc(diff)) %>%
  ggplot(aes(x = diff)) + geom_histogram()

submit_combined <- submit %>%
  bind_cols(submit_agrgt %>% select(Value2 = Value)) %>%
  mutate(Value = (Value + Value2) / 2) %>%
  select(-Value2)

write_csv(submit_combined, here::here(paste0("submissions/submission_", as.integer(Sys.time()), ".csv")))
