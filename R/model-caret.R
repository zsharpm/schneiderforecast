library(tidyverse)
library(caret)
library(xgboost)
source("R/model-ctrl.R")

submission_format <- read_csv("data/Power_Laws_Forecasting_Energy_Consumption_-_Submission_Format.csv")
names(submission_format) <- str_to_lower(names(submission_format))

trn <- read_rds(here::here("data/trn.rds"))
tst <- read_rds(here::here("data/tst.rds"))

trn <- trn %>%
  group_by(forecastid) %>%
  sample_frac(0.05) %>%
  ungroup()

sample_forecastid <- trn %>%
  distinct(forecastid) %>%
  sample_n(100)

trn <- trn %>% filter(forecastid %in% sample_forecastid$forecastid)

tst <- tst %>% filter(forecastid %in% sample_forecastid$forecastid)

mdl <- train(
  value ~ .,
  data = data.frame(
    trn %>%
      select(
        value,
        #forecastid,
        #temperature,
        surface,
        #period,
        #basetemperature,
        #sampling,
        #month,
        hour,
        holiday,
        wday,
        yday
        ) #%>%
      #mutate(forecastid = as.character(forecastid))
    ),
  method = "xgbTree",
  #method = "glmnet",
  trControl = ctrl,
  tuneGrid = xgb_grid
  #tuneGrid = glmnet_grid
  #tuneLength = 10L
  )

mdl

varImp(mdl)

ggplot(mdl$pred, aes(x = obs, y = pred)) +
  geom_abline(col = "green", alpha = .5, size = 1) +
  geom_point(alpha = .3) +
  #geom_smooth(method = "lm", se = FALSE, col = "red", lty = 2, lwd = 1, alpha = .5) +
  facet_wrap(~ Resample)

mdl$pred %>% select(rowIndex, obs, pred) %>% mutate(err = abs(obs - pred)) %>% arrange(desc(err)) %>% top_n(20, err)

trn %>% filter(log(value) > 16.26, log(value) <= 16.28)

mdl2 <- train(
  forecastid ~ .,
  data = data.frame(
    trn %>%
      select(

      ) %>%
      mutate(
        value = ifelse(value == 0, "x0", "x1")
      )
  ),
  method = "xgbTree",
  #method = "glmnet",
  trControl = ctrl,
  tuneGrid = xgb_grid
)

varImp(mdl2)
