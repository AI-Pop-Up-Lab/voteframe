print("top of the file")

library(readr)

source("R/post_strat_module.R")

print("module loaded")

survey_path <- "denmark_data/data/election_mar24_pred.csv"
frame_path <- "denmark_data/mrp_outputs/mrp_extended_frame_raked.csv"
output_dir <- "mrp_outputs/mar24"

print("reading files")
survey <- read_csv(survey_path, show_col_types = FALSE)
frame <- read_csv(frame_path, show_col_types = FALSE)
print("files read")

result <- run_post_stratification(
  survey = survey,
  frame = frame,
  config = list(
    verbose = TRUE,
    n_sims = 250
  )
)

print(result$point_estimates)
print(result$quartile_table)
print(result$extended_frame)
print(result$stage_diagnostics)
print(result$aggregate_counts)
print(result$municipality_party_point)
print(result$municipality_party_draws)
print(result$municipality_party_quartiles)
print(result$share_draws_long)

write_post_strat_outputs(result, output_dir)

message("All CSV files written to: ", normalizePath(output_dir))
