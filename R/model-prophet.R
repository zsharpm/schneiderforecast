

source("R/00-setup.R")
source("R/import.R")

# submission forecast periods: 10, 40, 90, and 360 days

# training_data_raw %>%
#   group_by(forecastid) %>%
#   summarize(mints = min(timestamp), maxts = max(timestamp)) %>%
#   ungroup() %>%
#   mutate(int = difftime(maxts, mints, units = "days")) %>%
#   count(int)

training_data_raw %>% distinct(forecastid) %>% anti_join(submission_format %>% distinct(forecastid), "forecastid")

holidays <- holidays_raw %>%
  group_by(date) %>%
  summarize(holiday = paste0(holiday, collapse = " and ")) %>%
  ungroup() %>%
  select(
    ds = date,
    holiday
  )

trn_ts <- training_data_raw %>%
  #filter(forecastid %in% c(1, 5, 42)) %>%
  mutate(date = as.Date(timestamp)) %>%
  inner_join(submission_forecast_period, "forecastid") %>%
  mutate(forecastperiodns = forecastperiodns / 3600000000000) %>%
  na.omit() %>%
  mutate(ds = timestamp, y = log(1 + value)) %>%
  select(
    f = forecastid,
    p = forecastperiodns,
    ds,
    y
  ) %>%
  nest(-f, -p)

mdl_ts <- trn_ts %>%
  mutate(model = map(data, ~ prophet(
    .x,
    growth = "linear",
    yearly.seasonality = FALSE,
    weekly.seasonality = "auto",
    daily.seasonality = "auto"
    #holidays = holidays
    )))

# save(mdl_ts, file = "data/mdl_ts.RData")

mdl_ts <- mdl_ts %>%
  mutate(future = map2(model, p, function(model, p)  {
    if (p == 24) {
      make_future_dataframe(model, periods = 60, freq = "day", include_history = FALSE)
    } else {
      if (p == 1) {
        make_future_dataframe(model, periods = 192, freq = 3600, include_history = FALSE)
      } else {
        make_future_dataframe(model, periods = 192, freq = 900, include_history = FALSE)
      }
}}))

mdl_ts <- mdl_ts %>%
  mutate(forecast = map2(model, future, ~ predict(.x, .y)))

# save(mdl_ts, file = "data/mdl_ts.RData")

# plot(mdl_ts[1, ]$model[[1]], mdl_ts[1, ]$forecast[[1]])

# mdl_ts %>% rename(forecastid = f) %>% distinct(forecastid) %>% anti_join(submission_format %>% distinct(forecastid), "forecastid")

forecasts <- mdl_ts %>%
  unnest(forecast) %>%
  bind_rows(mdl_ts2 %>% unnest(forecast)) %>%
  mutate(yhat = exp(yhat) - 1) %>%
  select(forecastid = f, timestamp = ds, yhat) %>%
  distinct()

submit <- submission_format %>%
  select(-value) %>%
  left_join(forecasts, by = c("forecastid", "timestamp")) %>%
  left_join(agrgts, by = "forecastid") %>%
  mutate(yhat = ifelse(is.na(yhat), value_median, yhat)) %>%
  select(
    obs_id,
    SiteId = siteid,
    Timestamp = timestamp,
    ForecastId = forecastid,
    Value = yhat
  )

submit %>%
  filter(ForecastId == 128) %>%
  arrange(Timestamp) %>%
  count(ForecastId, Timestamp) %>%
  filter(n > 1)

nrow(submit) / nrow(submission_format)

# missing forecastid
missing_forecastid <- distinct(submission_format, forecastid, timestamp) %>%
  anti_join(
    submit %>%
      distinct(ForecastId, Timestamp) %>%
      rename(forecastid = ForecastId, timestamp = Timestamp),
    c("forecastid", "timestamp")) %>%
  distinct(forecastid)

missing_site_id <- missing_forecastid %>%
  inner_join(forecastid_to_site, "forecastid") %>%
  distinct(siteid)

training_data_raw %>%
  #inner_join(forecastid_to_site, "forecastid") %>%
  filter(siteid %in% missing_site_id$siteid) %>%
  ggplot(aes(x = as.factor(siteid), y = value)) +
  geom_boxplot()

agrgts <- training_data_raw %>%
  filter(siteid %in% missing_site_id$siteid) %>%
  group_by(siteid) %>%
  summarize(value_median = median(value, na.rm = T)) %>%
  ungroup() %>%
  inner_join(forecastid_to_site, "siteid") %>%
  select(forecastid, value_median)

mdl_ts %>% filter(f == 281) %>% unnest(forecast) %>%
  select(ds, yhat) %>%
  bind_rows(submission_format %>% filter(forecastid == 281) %>% select(ds = timestamp, yhat = value)) %>%
  ggplot(aes(x = ds, y = yhat)) +
  geom_point()

training_data_raw %>% filter(forecastid == 700) %>%
  select(timestamp, value) %>%
  mutate(
    miss = ifelse(is.na(value), "y", "n"),
    value = ifelse(is.na(value), 0, value)) %>%
  bind_rows(submission_format %>% filter(forecastid == 700) %>% select(timestamp, value)) %>%
  ggplot(aes(x = timestamp, y = value, color = miss)) +
  geom_point()


mdl_ts2 <- mdl_ts %>%
  filter(f %in% missing_forecastid$forecastid) %>%
  mutate(future = map2(model, p, function(model, p)  {
    if (p == 24) {
      make_future_dataframe(model, periods = 60 * 4, freq = "day", include_history = FALSE)
    } else {
      if (p == 1) {
        make_future_dataframe(model, periods = 192 * 4, freq = 3600, include_history = FALSE)
      } else {
        make_future_dataframe(model, periods = 192 * 4, freq = 900, include_history = FALSE)
      }
    }}))

mdl_ts2 <- mdl_ts2 %>%
  mutate(forecast = map2(model, future, ~ predict(.x, .y)))

submit2 <- submission_format %>%
  left_join(agrgts, "forecastid") %>%

ggplot(submit, aes(x = log(Value + 1))) + geom_histogram()

ggplot(submit, aes(x = Value)) + geom_histogram()

write_csv(submit, here::here(paste0("submissions/submission_", as.integer(Sys.time()), ".csv")))

prev_submit <- read_csv("submissions/submission_1521945508.csv")

cor(prev_submit$Value, submit$Value)

sum(prev_submit$obs_id != submit$obs_id)
