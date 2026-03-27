# Purpose:
# Create a blocked train/test split for modelling.
# - split by block_id, not by individual point
# - save the split assignment for downstream modelling
# - print quick QA summaries

source("01_setup.R")

message("Running: 09_split_train_test.R")

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
# Check required fields
# -----------------------------
required_cols <- c("SegmentID", "block_id", "class")
missing_cols <- setdiff(required_cols, names(model_table))

if (length(missing_cols) > 0) {
  stop("Missing required columns in model_table: ",
       paste(missing_cols, collapse = ", "))
}

# -----------------------------
# Sample test blocks
# -----------------------------
blocks <- unique(model_table$block_id)
blocks <- blocks[!is.na(blocks)]

n_test_blocks <- max(1, round(length(blocks) * test_prop))
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
# Quick QA
# -----------------------------
message("Number of unique blocks: ", length(blocks))
message("Number of test blocks: ", n_test_blocks)

message("Rows by split:")
print(table(model_table_split$split, useNA = "ifany"))

message("Class distribution by split:")
print(table(model_table_split$class, model_table_split$split, useNA = "ifany"))

message("Unique blocks by split:")
print(table(unique(model_table_split[, c("block_id", "split")])$split))

# Optional proportions by class
message("Within-split class proportions:")
print(prop.table(table(model_table_split$class, model_table_split$split), margin = 2))

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

message("Done.")
