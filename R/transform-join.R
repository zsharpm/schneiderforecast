

both <- training_data_raw %>%
  bind_rows(submission_format) %>%
  select(-value) %>%
  mutate(timestamp_hourly = floor_date(timestamp, unit = "hour")) %>%
  inner_join(timestuff, by = c("forecastid", "obs_id")) %>%
  inner_join(metadata, "siteid") %>%
  left_join(holidays, by = c("siteid", "date")) %>%
  left_join(fill_values, by = c("forecastid", "obs_id")) %>%
  left_join(weather, by = c("siteid", "timestamp", "period")) %>%
  left_join(weather_update, by = c("timestamp_hourly")) %>%
  mutate(
    forecastid = as.factor(forecastid),
    siteid = as.factor(siteid),
    period = as.factor(period),
    half = as.factor(half),
    quarter = as.factor(quarter),
    month = as.factor(month),
    wday = as.factor(wday),
    temperature = ifelse(is.na(temperature), temp_impute, temperature)
    ) %>%
  replace_na(
    list(holiday = FALSE)
  ) %>%
  select(
    -index.num,
    -timestamp,
    -year,
    -obs_id,
    -date,
    -timestamp_hourly,
    -temp_impute,
    -distance,
    -freq,
    -prophet_freq,
    -prophet_period
  )

trn <- both %>% filter(set == "train") %>% select(-set)

#outlier_threshold <- quantile(trn$value, probs = .999, na.rm = TRUE)

trn <- trn %>%
  na.omit() #%>%
  #mutate(value = ifelse(value > outlier_threshold, outlier_threshold, value))

tst <- both %>% filter(set == "test") %>% select(-set)

glimpse(trn)

nrow(tst) == nrow(submission_format)

map_df(trn, ~ sum(is.na(.))) %>% glimpse()

sum(is.na(trn))
sum(is.na(tst))

write_rds(trn, path = here::here("data/trn.rds"))
write_rds(tst, path = here::here("data/tst.rds"))

# trn %>%
#   nest(-forecastid) %>%
#   mutate(groupnum = 1L + floor(10L * (row_number() - 1L) / nrow(.))) %>%
#   unnest() %>%
#   nest(-groupnum) %>%
#   mutate(written = walk2(data, groupnum, ~ write_rds(.x, path = paste0("data/trn_", .y, ".rds"))))

