library(caret)
library(doMC)

registerDoMC(cores = 3L)

# train control -----------------------------------------------------------

ctrl <- trainControl(
  method = "LGOCV",
  number = 1,
  p = 0.7,
  #sampling = "smote",
  savePredictions = "final",
  verboseIter = FALSE,
  allowParallel = TRUE
)

# ranger ------------------------------------------------------------------

getModelInfo("ranger")$ranger$parameters

ranger_grid <- expand.grid(
  mtry = c(3L, 6L, 9L, 12L, 15L),
  splitrule = "variance",
  min.node.size = 5L
)

# xgb ---------------------------------------------------------------------

getModelInfo("xgbTree")$xgbTree$parameters

xgb_grid <- expand.grid(
  nrounds = c(100L),
  max_depth = c(4L),
  eta = c(0.3),
  gamma = c(0.01),
  colsample_bytree = c(0.75),
  subsample = c(0.50),
  min_child_weight = c(0)
)

# knn ---------------------------------------------------------------------

getModelInfo("knn")$knn$parameters

knn_grid <- data.frame(k = seq(1, 5, 1))

# glmnet ------------------------------------------------------------------

getModelInfo("glmnet")$glmnet$parameters

glmnet_grid <- expand.grid(
  alpha = c(0.05, 0.1, 0.3, 0.5, 0.7, 0.95),
  lambda = c(1E-6, 1E-5, 1E-4, 1E-3)
  )

# h2o gbm -----------------------------------------------------------------

getModelInfo("gbm_h2o")$gbm_h2o$parameters

h2o_grid <- expand.grid(
  ntrees = c(100L, 200L, 300L),
  max_depth = c(4L),
  min_rows = c(5L),
  learn_rate = c(0.3),
  col_sample_rate = 0.5
)

