
library(h2o)

h2o.shutdown()
h2o.init(enable_assertions = FALSE)

trn <- trn %>%
  select(
    value,
    #forecastid,
    surface,
    hour,
    holiday,
    wday,
    yday
  )

trn_h2o <- as.h2o(trn_agrgt)
tst_h2o <- as.h2o(tst %>% select(-value))

y <- "value"
x <- setdiff(names(trn_h2o), y)

g_rf <- h2o.grid(
  "randomForest",
  search_criteria = list(
    strategy = "RandomDiscrete",
    stopping_metric = "AUTO",
    stopping_tolerance = 0.001,
    stopping_rounds = 5,
    max_runtime_secs = 60 * 5
    ),
  hyper_params = list(
    ntrees = c(50, 100),
    mtries = c(2, 3, 4),
    sample_rate = c( 0.8, 0.95),
    col_sample_rate_per_tree = c(0.7, 0.8, 0.9, 1.0),
    max_depth = c(4L, 6L, 8L),
    min_rows = 5L
    ),
  x = x,
  y = y,
  training_frame = trn_h2o,
  nfolds = 3,
  stopping_metric = "AUTO",
  stopping_tolerance = 0,
  stopping_rounds = 4,
  score_tree_interval = 3
  )

h2o.getGrid(g_rf@grid_id, sort_by = "mse")

h2o_rf <- h2o.randomForest(
  x = x,
  y = y,
  training_frame = trn_h2o,
  col_sample_rate_per_tree = 0.9,
  max_depth = 8L,
  mtries = 8L,
  ntrees = 50L,
  min_rows = 5,
  sample_rate = 0.8
)

h2o.varimp(h2o_rf)

pred_h2o_rf <- predict(h2o_rf, tst_h2o)

# gbm ---------------------------------------------------------------------

g_gbm <- h2o.grid(
  "gbm",
  search_criteria = list(
    strategy = "RandomDiscrete",
    stopping_metric = "AUTO",
    stopping_tolerance = 0.001,
    stopping_rounds = 5,
    max_runtime_secs = 60 * 5
  ),
  hyper_params = list(
    distribution = "AUTO", #tweedie
    #tweedie_power = c(1, 1.5, 2),
    ntrees = c(100, 200),
    max_depth = c(4L, 6L),
    min_rows = 30L,
    learn_rate = c(0.1),
    histogram_type = "AUTO",
    nbins = 20L,
    nbins_top_level = 1024L,
    nbins_cats = 1024L,
    sample_rate = c(0.9),
    col_sample_rate_per_tree = c(0.9)

  ),
  x = x,
  y = y,
  training_frame = trn_h2o,
  nfolds = 5,
  stopping_metric = "AUTO",
  stopping_tolerance = 0,
  stopping_rounds = 4,
  score_tree_interval = 3
)

h2o.getGrid(g_gbm@grid_id, sort_by = "mse")

h2o_gbm_mdl <- h2o.gbm(
  y = y,
  x = x,
  training_frame = trn_h2o,
  ntrees = 100L,
  max_depth = 8L,
  min_rows = 30L,
  learn_rate = c(0.1),
  histogram_type = "AUTO",
  nbins = 20L,
  nbins_top_level = 1024L,
  nbins_cats = 1024L,
  sample_rate = 0.95,
  col_sample_rate_per_tree = 1
)

h2o.varimp(h2o_gbm_mdl)

pred_h2o_gbm <- predict(h2o_gbm_mdl, tst_h2o)

# glm ---------------------------------------------------------------------

g_tweedie <- h2o.grid(
  "glm",
  search_criteria = list(
    strategy = "RandomDiscrete",
    stopping_metric = "AUTO",
    stopping_tolerance = 0.001,
    stopping_rounds = 5,
    max_runtime_secs = 60 * 5
  ),
  hyper_params = list(
    #tweedie_variance_power = c(0, 0.5, 1, 2),
    #tweedie_link_power = c(0.8, 1, 1.,2),
    alpha = c(0.8, 0.9, 1)
    ),
  x = x,
  y = y,
  training_frame = trn_h2o,
  nfolds = 3L,
  lambda_search = TRUE,
  solver = "IRLSM",
  family = "gaussian",
  max_iterations = 100
  )

h2o.getGrid(g_tweedie@grid_id, sort_by = "mse")

h2o_glm <- h2o.glm(
  x = x,
  y = y,
  training_frame = trn_h2o,
  nfolds = 3L,
  family = "gaussian",
  lambda_search = TRUE,
  solver = "IRLSM",
  alpha = 1
)

h2o.varimp(h2o_glm)

# auto ml -----------------------------------------------------------------

h2o_auto_mdl <- h2o.automl(
  y = y,
  x = x,
  training_frame = trn_h2o,
  max_runtime_secs = 600
)

pred_h2o_gbm <- predict(h2o_gbm_mdl, tst_h2o)

# predict -----------------------------------------------------------------

pred <- as.data.frame(pred_h2o_rf)

submit <- tst_raw %>% select(id) %>% bind_cols(pred) %>% select(id, target = X1)

write_csv(submit, paste0("submissions/submission-", as.integer(Sys.time()), ".csv"))
