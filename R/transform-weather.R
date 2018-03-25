

hourly_series <- create_series(
  floor_date(min(weather_raw$timestamp)) ~ ceiling_date(max(weather_raw$timestamp)),
  "hourly",
  include_end = TRUE
) %>% rename(timestamp = date) %>%
  tk_augment_timeseries_signature() %>%
  #crossing(trn %>% distinct(siteid)) %>%
  mutate(
    #siteid = as.factor(siteid),
    month = as.factor(month)
  )
#
# daily_series <- create_series(
#   floor_date(min(weather$timestamp)) ~ ceiling_date(max(weather$timestamp)),
#   "daily",
#   include_end = TRUE
# ) %>% rename(daily = date)

quarter_hourly_weather <- weather_raw %>%
  filter(siteid %in% sample_siteid$siteid) %>%
  as_tbl_time(index = timestamp) %>%
  arrange(timestamp) %>%
  group_by(siteid) %>%
  collapse_by("15 minutes") %>%
  ungroup() %>%
  group_by(siteid, timestamp) %>%
  summarise_all(mean) %>%
  ungroup() %>%
  mutate(
    timestamp_key = floor_index(timestamp, unit = "15 minutes"),
    period = "15 minutes"
  ) %>%
  select(-timestamp)

hourly_weather <- weather_raw %>%
  filter(siteid %in% sample_siteid$siteid) %>%
  as_tbl_time(index = timestamp) %>%
  arrange(timestamp) %>%
  group_by(siteid) %>%
  collapse_by("hourly") %>%
  ungroup() %>%
  group_by(siteid, timestamp) %>%
  summarise_all(mean) %>%
  ungroup() %>%
  mutate(
    timestamp_key = floor_index(timestamp, unit = "hour"),
    period = "hourly"
  ) %>%
  select(-timestamp)

daily_weather <- weather_raw %>%
  filter(siteid %in% sample_siteid$siteid) %>%
  as_tbl_time(index = timestamp) %>%
  arrange(timestamp) %>%
  group_by(siteid) %>%
  collapse_by("daily") %>%
  ungroup() %>%
  group_by(siteid, timestamp) %>%
  summarise_all(mean) %>%
  ungroup() %>%
  mutate(
    timestamp_key = floor_index(timestamp, unit = "day"),
    period = "daily"
  ) %>%
  select(-timestamp)

weather <- quarter_hourly_weather %>%
  bind_rows(hourly_weather) %>%
  bind_rows(daily_weather) %>%
  rename(timestamp = timestamp_key)

weather_tk <- weather %>%
  filter(period == "hourly") %>%
  tk_augment_timeseries_signature()

weather_tk <- weather_tk %>%
  transmute(
    temperature,
    siteid = fct_lump(as.factor(siteid), n = 8L),
    month = as.factor(month),
    day,
    hour,
    hour12,
    am.pm,
    wday,
    mday,
    week
  ) %>%
  distinct()

weather_mdl <- glm(
  temperature ~ .,
  data = data.frame(weather_tk %>% distinct(temperature, mday, month, hour))
)

hourly_series$temp_impute <- predict(weather_mdl, hourly_series)

weather_update <- hourly_series %>%
  select(
    timestamp_hourly = timestamp,
    temp_impute
  )

rm(quarter_hourly_weather)
rm(hourly_weather)
rm(daily_weather)
rm(weather_raw)
