
temp_mdl <- train(
  temperature ~ .,
  data = data.frame(training_data %>% select(temperature, week, month, hour, surface) %>% na.omit()),
  method = "ranger",
  trControl = ctrl,
  tuneLength = 2L,
  importance = "impurity"
)

varImp(temp_mdl)

ggplot(temp_mdl$pred, aes(x = obs, y = pred)) +
  geom_abline(col = "green", alpha = .5, size = 1) +
  geom_point(alpha = .3) +
  #geom_smooth(method = "lm", se = FALSE, col = "red", lty = 2, lwd = 1, alpha = .5) +
  facet_wrap(~ Resample)
