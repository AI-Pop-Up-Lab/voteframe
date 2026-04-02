# voteframe

`voteframe` now contains two R modules plus thin CLI and Python wrappers.

## Modules

### 1. Initial Extension

This module takes three inputs:

- a skeleton frame
- an auxiliary survey
- municipality vote totals

Preferred columns:

- Skeleton frame: `municipality`, `gender`, `age_group`, `education_level`, `N`
- Auxiliary survey: `municipality`, `gender`, `age_group`, `education_level`, `past_vote`
- Municipality vote totals: `party`, `municipality`, `vote_share`

Accepted aliases:

- frame `education` -> `education_level`
- survey `education` -> `education_level`
- vote totals `vote_pct` or `pop_pct` -> `vote_share`
- survey `vote_in_2022` or `party` -> `past_vote`

Main entrypoints:

- `run_initial_extension(skeleton_frame, auxiliary_survey, vote_totals, config = list())`
- `write_initial_extension_outputs(result, output_dir)`

Files:

- [R/initial_extension_module.R](/Users/danjonaitis/Documents/GitHub/voteframe/R/initial_extension_module.R)
- [scripts/run_initial_extension_cli.R](/Users/danjonaitis/Documents/GitHub/voteframe/scripts/run_initial_extension_cli.R)

What it does:

- estimates `past_vote` probabilities for each frame cell using the auxiliary survey
- smooths sparse cells with demographic, municipality, and overall priors
- calibrates those probabilities within each municipality to match the municipality vote totals
- returns and writes an extended frame with one row per cell-by-party

Written outputs:

- `initial_extended_frame.csv`
- `initial_municipality_targets.csv`
- `initial_municipality_achieved.csv`
- `initial_calibration_diagnostics.csv`
- `initial_cell_summary.csv`

### 2. Secondary Extension / Post-stratification

This module takes two inputs:

- a survey with `past_vote` and `predicted_vote`
- a post-stratification frame with `expected_N_raked`

Preferred columns:

- Survey: `age_group`, `gender`, `municipality`, `education_level`, `past_vote`, `predicted_vote`
- Frame: `age_group`, `gender`, `municipality`, `education_level`, `past_vote`, `expected_N_raked`

Accepted aliases:

- `vote_in_2022` -> `past_vote`
- `party` -> `past_vote`
- `vote_2026` -> `predicted_vote`
- frame `education` is not used here; use `education_level`

Main entrypoints:

- `run_post_stratification(survey, frame, config = list())`
- `write_post_strat_outputs(result, output_dir)`

Files:

- [R/post_strat_module.R](/Users/danjonaitis/Documents/GitHub/voteframe/R/post_strat_module.R)
- [post_strat.r](/Users/danjonaitis/Documents/GitHub/voteframe/post_strat.r)
- [scripts/run_post_strat_cli.R](/Users/danjonaitis/Documents/GitHub/voteframe/scripts/run_post_strat_cli.R)

Outcome ordering:

- if `"Did not vote"` exists in `predicted_vote`, it is always first in the stick-breaking order
- all remaining outcomes are ordered from highest observed survey share to lowest

Written outputs:

- `mrp_point_estimates.csv`
- `mrp_quartile_table.csv`
- `mrp_extended_frame_predictions.csv`
- `mrp_stage_diagnostics.csv`
- `mrp_aggregate_counts.csv`
- `mrp_share_draws.csv`
- `mrp_share_draws_long.csv`
- `mrp_cell_party_probabilities.csv`
- `mrp_stickbreaking_conditional_probs.csv`
- `mrp_municipality_party_point_estimates.csv`
- `mrp_municipality_party_draws_long.csv`
- `mrp_municipality_party_quartiles.csv`

## Running From R

### Initial Extension

```r
source("R/initial_extension_module.R")

result <- run_initial_extension(
  skeleton_frame = frame_df,
  auxiliary_survey = survey_df,
  vote_totals = totals_df
)

write_initial_extension_outputs(result, "path/to/output_dir")
```

CLI:

```bash
Rscript scripts/run_initial_extension_cli.R /path/to/survey.csv /path/to/frame.csv /path/to/vote_totals.csv /path/to/output_dir
```

Optional year filter:

```bash
Rscript scripts/run_initial_extension_cli.R /path/to/survey.csv /path/to/frame.csv /path/to/vote_totals.csv /path/to/output_dir 2022
```

### Secondary Extension

```r
source("R/post_strat_module.R")

result <- run_post_stratification(
  survey = survey_df,
  frame = frame_df,
  config = list(
    verbose = TRUE,
    n_sims = 250
  )
)

write_post_strat_outputs(result, "path/to/output_dir")
```

CLI:

```bash
Rscript scripts/run_post_strat_cli.R /path/to/survey.csv /path/to/frame.csv /path/to/output_dir 250
```

Repo runner:

```bash
Rscript post_strat.r
```

## Running From Python

Python helpers live in [python_test](/Users/danjonaitis/Documents/GitHub/voteframe/python_test).

Script:

```bash
python3 python_test/run_post_strat_via_python.py
```

The script auto-detects whether your files match the initial or secondary schema
and then runs the corresponding R CLI.

Notebook:

- [python_test/run_post_strat_via_python.ipynb](/Users/danjonaitis/Documents/GitHub/voteframe/python_test/run_post_strat_via_python.ipynb)

The notebook reads the CSV names from `python_test/data`, previews them, detects
the workflow, and runs the matching R CLI wrapper.

## Notes

- The core R modules operate on in-memory data frames and only write files when asked.
- The Python helpers are wrappers around the R CLI scripts, not part of the core modeling code.
- The local R environment may still show `vglmer` / `Matrix` ABI warnings for the secondary module; if that happens, reinstall those R packages before production runs.
