# Python Test Harness

This folder lets you run either implemented `voteframe` R module from Python.

## Files To Add

Place your input CSVs in `python_test/data`.

For the initial extension, the folder should contain:

- a survey CSV
- a frame CSV
- a municipality vote totals CSV

For the secondary extension, the folder should contain:

- a survey CSV
- a frame CSV

The Python helpers classify files by filename. These names work best:

- `survey_*.csv`
- `frame*.csv`
- `election_results.csv` or `*totals*.csv`

## Expected Schemas

Initial extension:

- Survey: `municipality`, `gender`, `age_group`, `education_level`, `past_vote`
- Frame: `municipality`, `gender`, `age_group`, `education_level`, `N`
- Vote totals: `party`, `municipality`, `vote_share`

Accepted aliases for the initial extension:

- `education` -> `education_level`
- `vote_pct` or `pop_pct` -> `vote_share`

Secondary extension:

- Survey: `age_group`, `gender`, `municipality`, `education_level`, `past_vote`, `predicted_vote`
- Frame: `age_group`, `gender`, `municipality`, `education_level`, `past_vote`, `expected_N_raked`

Accepted aliases for the secondary extension:

- `vote_in_2022` -> `past_vote`
- `party` -> `past_vote`
- `vote_2026` -> `predicted_vote`

## Run

Script:

```bash
python3 python_test/run_post_strat_via_python.py
```

Optional flags:

```bash
python3 python_test/run_post_strat_via_python.py --n-sims 500
python3 python_test/run_post_strat_via_python.py --year 2022
python3 python_test/run_post_strat_via_python.py --survey /path/to/survey.csv --frame /path/to/frame.csv --vote-totals /path/to/election_results.csv
```

Notebook:

- [python_test/run_post_strat_via_python.ipynb](/Users/danjonaitis/Documents/GitHub/voteframe/python_test/run_post_strat_via_python.ipynb)

The notebook previews the detected files, prints their headers, determines
whether the data match the initial or secondary workflow, and then runs the
matching R CLI wrapper.

For the initial extension, the notebook also builds a predicted-vs-actual chart
from the municipality election results and the generated
`initial_municipality_party_quartiles.csv`, then reports IQR coverage,
Spearman correlation, bias, and RMSE.

## Output

Outputs are written to `python_test/output/` by default.
