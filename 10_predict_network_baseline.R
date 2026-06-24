# Purpose:
# Apply the baseline Random Forest model to the full enriched network.
# - read saved model bundle
# - read enriched network
# - apply training-time preprocessing
# - predict class for every edge
# - write spatial output for mapping / review

source("01_setup.R")

message("Running: 10_predict_network_baseline.R")

library(ranger)

# -----------------------------
# Read inputs
# -----------------------------
model_bundle <- readRDS(
  here::here("outputs", "models", paste0("rf_baseline_", study_area_name, ".rds"))
)

network_enriched <- readRDS(
  here::here("data_intermediate", paste0("network_enriched_", study_area_name, ".rds"))
)

rf_fit <- model_bundle$model
predictors <- model_bundle$predictors
class_levels <- model_bundle$class_levels
num_impute <- model_bundle$numeric_impute
fac_impute <- model_bundle$factor_impute

# -----------------------------
# Basic checks
# -----------------------------
missing_predictors <- setdiff(predictors, names(network_enriched))
if (length(missing_predictors) > 0) {
  stop("Missing predictors in network_enriched: ",
       paste(missing_predictors, collapse = ", "))
}

# -----------------------------
# Keep ID cols + predictors
# -----------------------------
network_pred <- network_enriched %>%
  dplyr::select(
    dplyr::all_of(network_id_cols),
    dplyr::all_of(unique(c(predictors, postprocess_cols)))
  )

# -----------------------------
# Drop geometry for modelling
# -----------------------------
network_x <- sf::st_drop_geometry(network_pred) %>%
  dplyr::select(dplyr::all_of(predictors))

# -----------------------------
# Apply same type coercion as training
# -----------------------------
network_x <- network_x %>%
  dplyr::mutate(
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

# -----------------------------
# Apply training-time imputation
# -----------------------------
numeric_cols <- names(network_x)[vapply(network_x, is.numeric, logical(1))]
factor_cols  <- names(network_x)[vapply(network_x, is.factor, logical(1))]

# numeric
for (nm in intersect(names(num_impute), numeric_cols)) {
  network_x[[nm]][is.na(network_x[[nm]])] <- num_impute[[nm]]
}

# factor
for (nm in intersect(names(fac_impute), factor_cols)) {
  network_x[[nm]] <- as.character(network_x[[nm]])
  network_x[[nm]][is.na(network_x[[nm]]) | network_x[[nm]] == ""] <- fac_impute[[nm]]
  
  # use observed levels plus imputation value
  all_levels <- unique(c(network_x[[nm]], fac_impute[[nm]]))
  network_x[[nm]] <- factor(network_x[[nm]], levels = all_levels)
}

# -----------------------------
# Predict
# -----------------------------
rf_pred <- predict(rf_fit, data = network_x)$predictions
rf_pred <- factor(rf_pred, levels = class_levels)

network_pred <- network_pred %>%
  dplyr::mutate(
    pred_class = rf_pred
  ) %>%
  apply_infra_postprocess_rules(
    pred_col = "pred_class",
    out_col = "pred_class_final"
  )

network_pred %>%
  sf::st_drop_geometry() %>%
  dplyr::count(pred_class_model, pred_class_final, sort = TRUE)

# -----------------------------
# Quick QA
# -----------------------------
message("Rows predicted: ", nrow(network_pred))

message("Predicted class distribution:")
print(table(network_pred$pred_class, useNA = "ifany"))

# -----------------------------
# Write outputs
# -----------------------------
saveRDS(
  network_pred,
  here::here("outputs", "models", paste0("network_predictions_rf_", study_area_name, ".rds"))
)

sf::st_write(
  network_pred,
  here::here("outputs", "maps", paste0("network_predictions_rf_", study_area_name, ".gpkg")),
  delete_dsn = TRUE,
  quiet = TRUE
)

message("Done.")
