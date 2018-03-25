

# example arima fit on first split
roll_rs$splits[[1]] %>%
  analysis() %>%
  select(timestamp, value) %>%
  tk_ts(
    start = .$timestamp[[1]] %>% as.Date(),
    frequency = 365,
    silent = TRUE
  ) %>%
  auto.arima(.)

# get the min timestamp for the assessment set
get_date <- function(x) min(assessment(x)$timestamp)

# apply get_date function over all splits
start_date <- map(roll_rs$splits, get_date)

# combine
roll_rs$start_date <- do.call("c", start_date)

# function to fit arima models with analysis data in each split
fit_model <- function(x, ...) {

  x %>%
    analysis() %>%
    select(timestamp, value) %>%
    tk_ts(
      start = .$timestamp[[1]] %>% as.Date(),
      frequency = 365.25,
      silent = TRUE
    ) %>%
    auto.arima(...)

}

# fit arima models
roll_rs$arima <- map(roll_rs$splits, fit_model)

# example model fits
roll_rs$arima[[1]]
roll_rs$arima[[2]]

# check MAPE across all models within analysis period
roll_rs$interpolation <- map_dbl(
  roll_rs$arima,
  function(x)
    sw_glance(x)[["MAPE"]]
)

summary(roll_rs$interpolation)

# function to check MAPE across all models within assessment period
get_extrap <- function(split, mod) {

  n <- nrow(assessment(split))

  # get assessment data
  pred_dat <- assessment(split) %>%
    mutate(
      pred = as.vector(forecast(mod, h = n)$mean),
      pct_error = (value - pred ) / value * 100
    )
  mean(abs(pred_dat$pct_error))

}

# assessment MAPE
roll_rs$extrapolation <-
  map2_dbl(roll_rs$splits, roll_rs$arima, get_extrap)

# mean MAPE in assessment is about double the analysis period
summary(roll_rs$extrapolation)

# plot difference in MAPE in interpolation (analysis) and extrapolation (assessment)
roll_rs %>%
  select(interpolation, extrapolation, start_date) %>%
  as.data.frame %>%
  gather(error, MAPE, -start_date) %>%
  ggplot(aes(x = start_date, y = MAPE, col = error)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(legend.position = "top")
