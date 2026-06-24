# Purpose:
# Fit a baseline Random Forest model using the blocked train/test split.
# - read split model table
# - select predictors from model_predictors
# - do simple train-based imputation
# - fit multiclass random forest
# - evaluate on held-out test set
# - save model and outputs

source("01_setup.R")

message("Running: 09_fit_baseline_models.R")

library(ranger)
library(yardstick)

# -----------------------------
# Read inputs
# -----------------------------
model_table <- readRDS(
  here::here("data_intermediate", paste0("model_table_split_", study_area_name, ".rds"))
)

model_df <- sf::st_drop_geometry(model_table)

# -----------------------------
# Basic checks
# -----------------------------
required_cols <- c("class", "split", model_predictors)
missing_cols <- setdiff(required_cols, names(model_df))

if (length(missing_cols) > 0) {
  stop("Missing required columns in model_table_split: ",
       paste(missing_cols, collapse = ", "))
}

# -----------------------------
# Split train / test
# -----------------------------
train_df <- model_df %>% dplyr::filter(split == "train")
test_df  <- model_df %>% dplyr::filter(split == "test")

all_class_levels <- levels(factor(model_df$class))

# -----------------------------
# Keep only outcome + predictors
# -----------------------------
train_model <- train_df %>%
  dplyr::select(class, dplyr::all_of(model_predictors))

test_model <- test_df %>%
  dplyr::select(class, dplyr::all_of(model_predictors))

# -----------------------------
# Light type cleaning
# -----------------------------
coerce_types <- function(df) {
  df %>%
    dplyr::mutate(
      class = factor(class, levels = all_class_levels),
      
      across(any_of(c("road_class", "highway", "surface_class",
                      "replica_missing", "replica_traffic_context")),
             ~ factor(.)),
      
      across(any_of(c("has_any_cycleway", "has_lane", "has_track",
                      "is_unpaved", "is_bridge", "is_oneway",
                      "bike_route_designated", "replica_volume_missing",
                      "replica_speed_missing", "replica_low_stress")),
             ~ factor(.)),
      
      across(any_of(c("lanes", "maxspeed",
                      "replica_vol_aadt", "replica_spd_average_speed_mph",
                      "replica_log_aadt")),
             ~ suppressWarnings(as.numeric(.)))
    )
}

train_model <- coerce_types(train_model)
test_model  <- coerce_types(test_model)

# -----------------------------
# Train-based simple imputation
# -----------------------------
get_mode <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

train_x <- train_model[, model_predictors, drop = FALSE]
test_x  <- test_model[, model_predictors, drop = FALSE]

numeric_cols <- names(train_x)[vapply(train_x, is.numeric, logical(1))]
factor_cols  <- names(train_x)[vapply(train_x, is.factor, logical(1))]

# numeric medians from training only
num_impute <- lapply(train_x[, numeric_cols, drop = FALSE], function(x) {
  stats::median(x, na.rm = TRUE)
})

# factor modes from training only
fac_impute <- lapply(train_x[, factor_cols, drop = FALSE], get_mode)

# apply numeric imputation
for (nm in numeric_cols) {
  train_x[[nm]][is.na(train_x[[nm]])] <- num_impute[[nm]]
  test_x[[nm]][is.na(test_x[[nm]])]   <- num_impute[[nm]]
}

# apply factor imputation and align levels
for (nm in factor_cols) {
  train_x[[nm]] <- as.character(train_x[[nm]])
  test_x[[nm]]  <- as.character(test_x[[nm]])
  
  train_x[[nm]][is.na(train_x[[nm]]) | train_x[[nm]] == ""] <- fac_impute[[nm]]
  test_x[[nm]][is.na(test_x[[nm]]) | test_x[[nm]] == ""]   <- fac_impute[[nm]]
  
  all_levels <- unique(c(train_x[[nm]], test_x[[nm]]))
  train_x[[nm]] <- factor(train_x[[nm]], levels = all_levels)
  test_x[[nm]]  <- factor(test_x[[nm]], levels = all_levels)
}

train_model <- dplyr::bind_cols(class = train_model$class, train_x)
test_model  <- dplyr::bind_cols(class = test_model$class, test_x)

# -----------------------------
# Fit random forest
# -----------------------------
set.seed(1223)

rf_fit <- ranger::ranger(
  formula = class ~ .,
  data = train_model,
  probability = FALSE,
  importance = "impurity",
  num.trees = 500,
  mtry = max(1, floor(sqrt(length(model_predictors)))),
  min.node.size = 5,
  seed = 42
)

# -----------------------------
# Predict on test set
# -----------------------------
rf_pred <- predict(rf_fit, data = test_model)$predictions
rf_pred <- factor(rf_pred, levels = all_class_levels)

pred_tbl <- test_df %>%
  dplyr::select(
    dplyr::all_of(model_id_cols),
    class,
    dplyr::all_of(postprocess_cols)
  ) %>%
  dplyr::mutate(
    class = factor(class, levels = all_class_levels),
    pred_class = factor(rf_pred, levels = all_class_levels)
  ) %>%
  apply_infra_postprocess_rules(
    pred_col = "pred_class",
    out_col = "pred_class_final"
  ) %>%
  dplyr::mutate(
    pred_class_final = factor(pred_class_final, levels = final_class_levels)
  )

# -----------------------------
# Evaluate
# -----------------------------
pred_tbl_eval <- pred_tbl %>%
  dplyr::mutate(
    class_eval = collapse_to_reference_class(class),
    pred_class_eval = collapse_to_reference_class(pred_class_final),
    
    class_eval = factor(class_eval),
    pred_class_eval = factor(pred_class_eval, levels = levels(class_eval))
  )

yardstick::accuracy_vec(
  truth = pred_tbl_eval$class,
  estimate = pred_tbl_eval$pred_class_eval
)

yardstick::f_meas_vec(
  truth = pred_tbl_eval$class,
  estimate = pred_tbl_eval$pred_class_eval
)

cm <- table(
  truth = pred_tbl$class,
  pred = pred_tbl$pred_class_final
)

cm_eval <- table(
  truth = pred_tbl_eval$class,
  pred = pred_tbl_eval$pred_class_final
)

override_summary <- pred_tbl %>%
  dplyr::count(
    pred_class_model,
    pred_class_final,
    sort = TRUE
  )

print(override_summary)

recall_tbl <- as.data.frame.matrix(prop.table(cm, margin = 1))
recall_by_class <- data.frame(
  class = rownames(cm),
  recall = diag(prop.table(cm, margin = 1))
)

macro_f1 <- yardstick::f_meas_vec(
  truth = pred_tbl$class,
  estimate = pred_tbl$pred_class,
  estimator = "macro"
)

# metrics_tbl <- data.frame(
#   metric = c("accuracy", "macro_f1"),
#   value = c(acc, macro_f1)
# )
# 
# message("Accuracy: ", round(acc, 3))
# message("Macro F1: ", round(macro_f1, 3))

message("Confusion matrix:")
print(cm)

message("Recall by class:")
print(recall_by_class)

message("Raw RF accuracy: ", round(mean(pred_tbl$class == pred_tbl$pred_class), 3))
# message("Final accuracy: ", round(acc, 3))

message("Post-processing changes:")
print(table(
  model = pred_tbl$pred_class,
  final = pred_tbl$pred_class_final,
  useNA = "ifany"
))

# -----------------------------
# Variable importance
# -----------------------------
varimp_tbl <- data.frame(
  variable = names(rf_fit$variable.importance),
  importance = as.numeric(rf_fit$variable.importance)
) %>%
  dplyr::arrange(dplyr::desc(importance))

# -----------------------------
# Save model bundle
# -----------------------------
model_bundle <- list(
  model = rf_fit,
  predictors = model_predictors,
  class_levels = all_class_levels,
  numeric_impute = num_impute,
  factor_impute = fac_impute
)

saveRDS(
  model_bundle,
  here::here("outputs", "models", paste0("rf_baseline_", study_area_name, ".rds"))
)

# -----------------------------
# Write outputs
# -----------------------------
readr::write_csv(
  pred_tbl,
  here::here("outputs", "tables", paste0("rf_predictions_", study_area_name, ".csv"))
)

# readr::write_csv(
#   metrics_tbl,
#   here::here("outputs", "tables", paste0("rf_metrics_", study_area_name, ".csv"))
# )

readr::write_csv(
  recall_by_class,
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
