

trn <- read_rds("data/trn.rds")

trn %>%
  nest(-forecastid, .key = "data")

mdl_ts <- trn %>%
  select(forecastid, timestamp, value) %>%
  as_tbl_time(index = timestamp) %>%
  nest(-forecastid, .key = "data") %>%
  mutate(ts = map(data, ~ tk_ts(.x, value, frequency = 24))) %>%
  mutate(model = map(ts, ~ auto.arima(.x))) %>%
  mutate(forecast = map(model, ~ forecast(.x, h = 192))) %>%
  mutate(sweep = map(forecast, ~ sw_sweep(.x, timetk_idx = TRUE, rename_index = "timestamp")))

predictions <- mdl_ts %>%
  unnest(sweep) %>%
  select(
    ForecastId = forecastid,
    Timestamp = timestamp,
    Value = value
  )

mdl_ts[1, ]$sweep[[1]] %>%
  ggplot(aes(x = timestamp, y = value, color = key)) +
  # 95% CI
  #geom_ribbon(aes(ymin = lo.95, ymax = hi.95), fill = "#D5DBFF", color = NA, size = 0) +
  # 80% CI
  #geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key),  fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  # Prediction
  #geom_line() +
  geom_point() +
  # Actuals
  #geom_line(aes(x = timestamp, y = value), color = tidyquant::palette_light()[[1]], data = trn %>% filter(forecastid == 40)) +
  #geom_point(aes(x = timestamp, y = value), color = tidyquant::palette_light()[[1]], data = trn %>% filter(forecastid == 40)) +
  # Aesthetics
  #scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq()


