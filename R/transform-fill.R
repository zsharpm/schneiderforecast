

fill_values <- training_data_raw %>%
  bind_rows(submission_format) %>%
  select(obs_id, forecastid, timestamp, value_fill = value) %>%
  arrange(timestamp) %>%
  group_by(forecastid) %>%
  tidyr::fill(value_fill, .direction = "down") %>%
  ungroup() %>%
  filter(!is.na(value_fill)) %>%
  select(-timestamp) %>%
  rename(value = value_fill)
