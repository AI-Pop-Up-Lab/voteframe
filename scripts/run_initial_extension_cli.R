args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 4) {
  stop(
    paste(
      "Usage:",
      "Rscript scripts/run_initial_extension_cli.R <survey_csv> <frame_csv> <vote_totals_csv> <output_dir> [year]"
    )
  )
}

get_script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

  if (length(file_arg) == 0) {
    stop("Unable to determine script path for sourcing the module.")
  }

  normalizePath(sub("^--file=", "", file_arg[[1]]))
}

script_path <- get_script_path()
repo_root <- normalizePath(file.path(dirname(script_path), ".."))

source(file.path(repo_root, "R", "initial_extension_module.R"))

survey_path <- args[[1]]
frame_path <- args[[2]]
vote_totals_path <- args[[3]]
output_dir <- args[[4]]
year <- if (length(args) >= 5) as.integer(args[[5]]) else NULL

if (!is.null(year) && is.na(year)) {
  stop("year must be an integer when provided.")
}

survey <- readr::read_csv(survey_path, show_col_types = FALSE)
frame <- readr::read_csv(frame_path, show_col_types = FALSE)
vote_totals <- readr::read_csv(vote_totals_path, show_col_types = FALSE)

result <- run_initial_extension(
  skeleton_frame = frame,
  auxiliary_survey = survey,
  vote_totals = vote_totals,
  config = list(
    verbose = TRUE,
    year = year
  )
)

write_initial_extension_outputs(result, output_dir)

message("All CSV files written to: ", normalizePath(output_dir))
