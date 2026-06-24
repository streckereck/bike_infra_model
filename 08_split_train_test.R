# Purpose:
# Create a blocked train/test split for modelling.
# - split by block_id, not by individual point
# - save split assignment for downstream modelling
# - print quick QA summaries

source("01_setup.R")

message("Running: 08_split_train_test.R")

# -----------------------------
# Settings
# -----------------------------
set.seed(42)
test_prop <- 0.20

# -----------------------------
# Read inputs
# -----------------------------
model_table <- readRDS(
  here::here("data_intermediate", paste0("model_table_", study_area_name, ".rds"))
)

# -----------------------------
# Basic checks
# -----------------------------
required_cols <- c("block_id", "class")
missing_cols <- setdiff(required_cols, names(model_table))

if (length(missing_cols) > 0) {
  stop("Missing required columns in model_table: ",
       paste(missing_cols, collapse = ", "))
}

# -----------------------------
# Get unique blocks
# -----------------------------
blocks <- model_table %>%
  sf::st_drop_geometry() %>%
  dplyr::distinct(block_id) %>%
  dplyr::filter(!is.na(block_id)) %>%
  dplyr::pull(block_id)

n_blocks <- length(blocks)

if (n_blocks < 2) {
  stop("Need at least 2 non-missing blocks to create a train/test split.")
}

n_test_blocks <- max(1, round(n_blocks * test_prop))
test_blocks <- sample(blocks, size = n_test_blocks)

# -----------------------------
# Assign split
# -----------------------------
model_table_split <- model_table %>%
  dplyr::mutate(
    split = dplyr::if_else(block_id %in% test_blocks, "test", "train"),
    split = factor(split, levels = c("train", "test"))
  )

# -----------------------------
# QA summaries
# -----------------------------
message("Unique blocks: ", n_blocks)
message("Test blocks selected: ", n_test_blocks)

message("Rows by split:")
print(table(model_table_split$split, useNA = "ifany"))

message("Class distribution by split:")
print(table(model_table_split$class, model_table_split$split, useNA = "ifany"))

message("Within-split class proportions:")
print(prop.table(table(model_table_split$class, model_table_split$split), margin = 2))

blocks_by_split <- model_table_split %>%
  sf::st_drop_geometry() %>%
  dplyr::distinct(block_id, split) %>%
  dplyr::count(split, name = "n_blocks")

message("Blocks by split:")
print(blocks_by_split)

# Optional: check whether any classes are missing entirely from train or test
class_split_tab <- table(model_table_split$class, model_table_split$split, useNA = "ifany")

missing_in_train <- rownames(class_split_tab)[class_split_tab[, "train"] == 0]
missing_in_test  <- rownames(class_split_tab)[class_split_tab[, "test"] == 0]

if (length(missing_in_train) > 0) {
  message("Classes missing from TRAIN: ", paste(missing_in_train, collapse = ", "))
}

if (length(missing_in_test) > 0) {
  message("Classes missing from TEST: ", paste(missing_in_test, collapse = ", "))
}

# -----------------------------
# Write outputs
# -----------------------------
saveRDS(
  model_table_split,
  here::here("data_intermediate", paste0("model_table_split_", study_area_name, ".rds"))
)

readr::write_csv(
  sf::st_drop_geometry(model_table_split),
  here::here("data_intermediate", paste0("model_table_split_", study_area_name, ".csv"))
)

saveRDS(
  test_blocks,
  here::here("data_intermediate", paste0("test_blocks_", study_area_name, ".rds"))
)

message("Done.")