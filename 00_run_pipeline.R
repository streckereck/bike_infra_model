# Purpose:
# Run the current modelling pipeline in order.
# This is an orchestrator only; each script reads/writes its own files.

message("Running full pipeline...")

source("01_setup.R")
source("02_define_study_area.R")
source("03_prepare_reference_data.R")
source("04_prepare_network_data.R")
source("05_build_enriched_network.R")
source("06_attach_network_predictors_to_points.R")
source("07_build_model_table.R")
source("08_split_train_test.R")
source("09_fit_baseline_models.R")
source("10_predict_network_baseline.R")
source("11_review_network_predictions.R")

message("Pipeline complete.")
