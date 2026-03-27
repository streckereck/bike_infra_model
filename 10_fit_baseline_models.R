# Purpose:
# Fit a baseline Random Forest model using the blocked train/test split.
# - read split model table
# - select predictors
# - do simple train-based imputation
# - fit multiclass random forest
# - evaluate on held-out test set
# - save model and outputs

source("01_setup.R")

message("Running: 10_fit_baseline_models.R")

library(ranger)
library(yardstick)

# -----------------------------
# Read inputs
# -----------------------------
model_table <- readRDS(
  here::here("data_intermediate", paste0("model_table_split_", study_area_name, ".rds"))
)

# -----------------------------
# Drop geometry for modelling
# -----------------------------
model_df <- sf::st_drop_geometry(model_table)

# -----------------------------
# Check required columns
# -----------------------------
required_cols <- c("SegmentID", "block_id", "class", "split")
missing_cols <- setdiff(required_cols, names(model_df))

if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

# -----------------------------
# Split train / test
# -----------------------------
train_df <- model_df %>% dplyr::filter(split == "train")
test_df  <- model_df %>% dplyr::filter(split == "test")

# -----------------------------
# Choose predictors
# -----------------------------
# Keep this conservative for a baseline.
# Exclude IDs, outcome, split, and fields that are mostly QA / lineage.
exclude_cols <- c(
  "SegmentID",
  "source_wave",
  "stratum",
  "block_id",
  "reviewer",
  "class",
  "split"
)

predictor_cols <- setdiff(names(model_df), exclude_cols)

# Optional: inspect predictor list
message("Number of candidate predictors: ", length(predictor_cols))

# -----------------------------
# Coerce common field types
# -----------------------------
coerce_types <- function(df) {
  df %>%
    dplyr::mutate(
      class = factor(class),
      
      road_class = if ("road_class" %in% names(.)) factor(road_class) else NULL,
      highway = if ("highway" %in% names(.)) factor(highway) else NULL,
      surface_class = if ("surface_class" %in% names(.)) factor(surface_class) else NULL,
      replica_missing = if ("replica_missing" %in% names(.)) factor(replica_missing) else NULL,
      replica_traffic_context = if ("replica_traffic_context" %in% names(.)) factor(replica_traffic_context) else NULL,
      
      has_any_cycleway = if ("has_any_cycleway" %in% names(.)) as.factor(has_any_cycleway) else NULL,
      has_lane = if ("has_lane" %in% names(.)) as.factor(has_lane) else NULL,
      has_track = if ("has_track" %in% names(.)) as.factor(has_track) else NULL,
      is_unpaved = if ("is_unpaved" %in% names(.)) as.factor(is_unpaved) else NULL,
      is_bridge = if ("is_bridge" %in% names(.)) as.factor(is_bridge) else NULL,
      is_oneway = if ("is_oneway" %in% names(.)) as.factor(is_oneway) else NULL,
      bike_route_designated = if ("bike_route_designated" %in% names(.)) as.factor(bike_route_designated) else NULL,
      replica_volume_missing = if ("replica_volume_missing" %in% names(.)) as.factor(replica_volume_missing) else NULL,
      replica_speed_missing = if ("replica_speed_missing" %in% names(.)) as.factor(replica_speed_missing) else NULL,
      replica_low_stress = if ("replica_low_stress" %in% names(.)) as.factor(replica_low_stress) else NULL
    )
}

train_df <- coerce_types(train_df)
test_df  <- coerce_types(test_df)

# -----------------------------
# Train-based simple imputation
# -----------------------------
get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Build imputation values from training data only
train_x <- train_df[, predictor_cols, drop = FALSE]
test_x  <- test_df[, predictor_cols, drop = FALSE]

numeric_cols <- names(train_x)[vapply(train_x, is.numeric, logical(1))]
factor_cols  <- names(train_x)[vapply(train_x, function(x) is.factor(x) || is.character(x), logical(1))]
other_cols   <- setdiff(names(train_x), c(numeric_cols, factor_cols))

# Numeric medians
num_impute <- lapply(train_x[, numeric_cols, drop = FALSE], function(x) {
  stats::median(x, na.rm = TRUE)
})

# Factor/character modes
fac_impute <- lapply(train_x[, factor_cols, drop = FALSE], get_mode)

# Apply imputation
for (nm in numeric_cols) {
  train_x[[nm]][is.na(train_x[[nm]])] <- num_impute[[nm]]
  test_x[[nm]][is.na(test_x[[nm]])]   <- num_impute[[nm]]
}

for (nm in factor_cols) {
  # Convert characters to factors using combined levels from train + test after filling
  train_x[[nm]][is.na(train_x[[nm]]) | train_x[[nm]] == ""] <- fac_impute[[nm]]
  test_x[[nm]][is.na(test_x[[nm]]) | test_x[[nm]] == ""]   <- fac_impute[[nm]]
  
  all_levels <- unique(c(as.character(train_x[[nm]]), as.character(test_x[[nm]])))
  train_x[[nm]] <- factor(as.character(train_x[[nm]]), levels = all_levels)
  test_x[[nm]]  <- factor(as.character(test_x[[nm]]), levels = all_levels)
}

# Keep any other columns as-is
if (length(other_cols) > 0) {
  message("Other predictor columns retained as-is: ", paste(other_cols, collapse = ", "))
}

train_model <- dplyr::bind_cols(class = train_df$class, train_x)
test_model  <- dplyr::bind_cols(class = test_df$class, test_x)

# -----------------------------
# Fit random forest
# -----------------------------
set.seed(404)

rf_fit <- ranger::ranger(
  formula = class ~ .,
  data = train_model,
  probability = FALSE,
  importance = "impurity",
  num.trees = 500,
  mtry = max(1, floor(sqrt(ncol(train_x)))),
  min.node.size = 5,
  seed = 42
)

# -----------------------------
# Predict on test set
# -----------------------------
rf_pred <- predict(rf_fit, data = test_model)$predictions
rf_pred <- factor(rf_pred, levels = levels(train_model$class))

pred_tbl <- test_df %>%
  dplyr::select(SegmentID, block_id, class) %>%
  dplyr::mutate(pred_class = rf_pred)

# -----------------------------
# Evaluate
# -----------------------------
cm <- table(truth = pred_tbl$class, pred = pred_tbl$pred_class)

# as.data.frame.matrix(cm) %>% View()

acc <- mean(as.character(pred_tbl$class) == as.character(pred_tbl$pred_class))

# per-class recall
recall_by_class <- diag(prop.table(cm, margin = 1))
recall_tbl <- data.frame(
  class = names(recall_by_class),
  recall = as.numeric(recall_by_class)
)

# macro F1 using yardstick
macro_f1 <- yardstick::f_meas_vec(
  truth = pred_tbl$class,
  estimate = pred_tbl$pred_class,
  estimator = "macro"
)

metrics_tbl <- data.frame(
  metric = c("accuracy", "macro_f1"),
  value = c(acc, macro_f1)
)

message("Accuracy: ", round(acc, 3))
message("Macro F1: ", round(macro_f1, 3))

message("Confusion matrix:")
print(cm)

message("Recall by class:")
print(recall_tbl)

# -----------------------------
# Variable importance
# -----------------------------
varimp_tbl <- data.frame(
  variable = names(rf_fit$variable.importance),
  importance = as.numeric(rf_fit$variable.importance)
) %>%
  dplyr::arrange(dplyr::desc(importance))

# -----------------------------
# Write outputs
# -----------------------------
saveRDS(
  rf_fit,
  here::here("outputs", "models", paste0("rf_baseline_", study_area_name, ".rds"))
)

readr::write_csv(
  pred_tbl,
  here::here("outputs", "tables", paste0("rf_predictions_", study_area_name, ".csv"))
)

readr::write_csv(
  metrics_tbl,
  here::here("outputs", "tables", paste0("rf_metrics_", study_area_name, ".csv"))
)

readr::write_csv(
  recall_tbl,
  here::here("outputs", "tables", paste0("rf_recall_by_class_", study_area_name, ".csv"))
)

readr::write_csv(
  as.data.frame(cm),
  here::here("outputs", "tables", paste0("rf_confusion_matrix_", study_area_name, ".csv"))
)

readr::write_csv(
  varimp_tbl,
  here::here("outputs", "tables", paste0("rf_variable_importance_", study_area_name, ".csv"))
)

message("Done.")
