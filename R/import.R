
holidays_raw <- read_csv("data/Power_Laws_Forecasting_Energy_Consumption_-_Holidays.csv")[, -1]
metadata_raw <- read_csv("data/Power_Laws_Forecasting_Energy_Consumption_-_Metadata.csv")
submission_forecast_period <- read_csv("data/Power_Laws_Forecasting_Energy_Consumption_-_Submission_Forecast_Period.csv")
submission_format <- read_csv("data/Power_Laws_Forecasting_Energy_Consumption_-_Submission_Format.csv")
training_data_raw <- read_csv("data/Power_Laws_Forecasting_Energy_Consumption_-_Training_data.csv")
#weather_raw <- read_csv("data/Power_Laws_Forecasting_Energy_Consumption_-_Weather.csv")[, -1]

names(holidays_raw) <- str_to_lower(names(holidays_raw))
names(metadata_raw) <- str_to_lower(names(metadata_raw))
names(submission_forecast_period) <- str_to_lower(names(submission_forecast_period))
names(submission_format) <- str_to_lower(names(submission_format))
names(training_data_raw) <- str_to_lower(names(training_data_raw))
#names(weather_raw) <- str_to_lower(names(weather_raw))

training_data_raw$set <- "train"
submission_format$set <- "test"

forecast_period <- tribble(
  ~ forecastperiodns, ~ period, ~ freq, ~ prophet_freq, ~ prophet_period,
  900000000000, "15 minutes", 96L, "1 minute", 900,
  3600000000000, "hourly", 24L, "1 hour", 60,
  86400000000000, "daily", 365L,  "day", 192
)

forecastid_to_site <- training_data_raw %>% distinct(forecastid, siteid) %>%
  inner_join(metadata, "siteid")

# transformer <- function(df) {
#
#   features <- df %>%
#     inner_join(submission_forecast_period, "forecastid") %>%
#     inner_join(forecast_period, "forecastperiodns") %>%
#     select(-forecastperiodns) %>%
#
#     select(
#       -quarter_hourly,
#       -hourly,
#       -daily
#     ) %>%
#     left_join(weather, by = c("siteid", "timestamp_key", "period")) %>%
#     mutate_if(is.ordered, as.character) %>%
#     mutate_if(is.character, as.factor) %>%
#     mutate_if(is.logical, as.integer) %>%
#     select(
#       obs_id,
#       timestamp,
#       value,
#       siteid,
#       forecastid,
#       surface,
#       basetemperature,
#       hour,
#       temperature,
#       distance,
#       fridayisdayoff,
#       saturdayisdayoff,
#       sundayisdayoff,
#       mondayisdayoff,
#       period,
#       year,
#       month,
#       week,
#       day,
#       wday,
#       holiday
#     )
# }
#
# # stop here
#
# trn <- transformer(training_data)
#
# transformer2 <- function(df) {
#
#   features_lag <- features %>%
#     select(
#       forecastid,
#       timestamp,
#       value,
#       temperature,
#       distance
#     ) %>%
#     #na.omit() %>%
#     group_by(forecastid) %>%
#     arrange(timestamp) %>%
#     mutate_all(funs(
#       lag1 = lag(., 1L),
#       lag2 = lag(., 2L),
#       lag4 = lag(., 4L),
#       lag8 = lag(., 8L),
#       lag12 = lag(., 12L),
#       lag24 = lag(., 24L),
#       lag48 = lag(., 48L),
#       lag96 = lag(., 96L)
#     )) %>%
#     ungroup() %>%
#     #na.omit() %>%
#     select(
#       forecastid,
#       timestamp,
#       contains("temperature_lag")
#     )
#
#   df %>%
#     inner_join(features_lag, by = c("forecastid", "timestamp"))
#
# }
#
# test_data <- transformer(submission_format)
# map_df(test_data, ~ sum(is.na(.))) %>% glimpse()





