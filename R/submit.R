

caret_preds <- predict(mdl2, newdata = tst)
h2o_preds <-  h2o.predict(h2o_glm, newdata = tst_h2o)

submit <- submission_format %>%
  mutate(Value = h2o_preds %>% as_tibble() %>% pull(predict)) %>%
  mutate(Value = exp(Value) - 1) %>%
  #mutate(Value = caret_preds) %>%
  select(
    obs_id,
    SiteId = siteid,
    Timestamp = timestamp,
    ForecastId = forecastid,
    Value
  )

nrow(submit) == nrow(submission_format)

ggplot(submit, aes(x = log(Value + 1))) + geom_histogram()
ggplot(submit, aes(x = Value)) + geom_histogram()

write_csv(submit, here::here(paste0("submissions/submission_", as.integer(Sys.time()), ".csv")))
