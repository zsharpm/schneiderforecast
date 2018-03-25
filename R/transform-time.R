

timestuff <- training_data_raw %>%
  bind_rows(submission_format) %>%
  select(-siteid, -value) %>%
  inner_join(submission_forecast_period, "forecastid") %>%
  inner_join(forecast_period, "forecastperiodns") %>%
  mutate(
    date = as.Date(timestamp)#,
    # quarter_hourly = floor_index(timestamp, unit = "15 minutes"),
    # hourly = floor_index(timestamp, unit = "hours"),
    # daily = floor_index(timestamp, unit = "days"),
    # timestamp_key = case_when(
    #   period == "15 minutes" ~ quarter_hourly,
    #   period == "hourly" ~ hourly,
    #   period == "daily" ~ daily
    #)
  ) %>%
  select(-forecastperiodns) %>%
  tk_augment_timeseries_signature() %>%
  select(
    -set,
    -timestamp,
    -diff,
    -month.lbl,
    -wday.lbl,
    -year.iso,
    -week.iso,
    -month.xts,
    -wday.xts
  )



# features_date_lag <- training_data %>%
#   select(
#     forecastid,
#     timestamp,
#     value
#   ) %>%
#   mutate(
#    timestamp_lag_1year = timestamp - years(1),
#    timestamp_lag_1month = timestamp - months(1),
#    timestamp_lag_2month = timestamp - months(3),
#    timestamp_lag_1week = timestamp - weeks(1),
#    timestamp_lag_1day = timestamp - days(1)
#   ) %>%
#   left_join(training_data %>% select(forecastid, timestamp, value_lag_1year = value), by = c("forecastid", "timestamp_lag_1year" = "timestamp")) %>%
#   left_join(training_data %>% select(forecastid, timestamp, value_lag_1month = value), by = c("forecastid", "timestamp_lag_1month" = "timestamp")) %>%
#   left_join(training_data %>% select(forecastid, timestamp, value_lag_2month = value), by = c("forecastid", "timestamp_lag_2month" = "timestamp")) %>%
#   left_join(training_data %>% select(forecastid, timestamp, value_lag_1week = value), by = c("forecastid", "timestamp_lag_1week" = "timestamp")) %>%
#   left_join(training_data %>% select(forecastid, timestamp, value_lag_1day = value), by = c("forecastid", "timestamp_lag_1day" = "timestamp")) %>%
#   select(
#     forecastid,
#     timestamp
#   )
