from __future__ import annotations

import argparse
import csv
import subprocess
import sys
from pathlib import Path


def classify_csv(path: Path) -> str:
    name = path.stem.lower()

    if "survey" in name:
        return "survey"
    if "frame" in name:
        return "frame"
    if "total" in name or "totals" in name:
        return "municipality_vote_totals"
    if "vote" in name and "municip" in name:
        return "municipality_vote_totals"
    if "election" in name or "result" in name:
        return "municipality_vote_totals"

    return "unknown"


def read_header(path: Path) -> list[str]:
    with path.open(newline="", encoding="utf-8-sig") as handle:
        return next(csv.reader(handle))


def has_columns(header: list[str], required: set[str]) -> bool:
    return required.issubset(set(header))


def infer_mode(survey_path: Path, frame_path: Path, totals_path: Path | None) -> str:
    survey_header = read_header(survey_path)
    frame_header = read_header(frame_path)
    totals_header = read_header(totals_path) if totals_path else []

    secondary_survey_cols = {
        "age_group",
        "gender",
        "municipality",
        "education_level",
        "predicted_vote",
    }
    secondary_frame_cols = {
        "age_group",
        "gender",
        "municipality",
        "education_level",
        "expected_N_raked",
    }

    initial_survey_cols = {
        "age_group",
        "gender",
        "municipality",
        "education_level",
        "past_vote",
    }
    initial_frame_cols = {"municipality", "gender", "age_group", "N"}
    initial_totals_cols = {"party", "municipality"}

    looks_like_secondary = has_columns(survey_header, secondary_survey_cols) and has_columns(
        frame_header, secondary_frame_cols
    )
    looks_like_initial = (
        has_columns(survey_header, initial_survey_cols)
        and has_columns(frame_header, initial_frame_cols)
        and totals_path is not None
        and has_columns(totals_header, initial_totals_cols)
    )

    if looks_like_secondary:
        return "secondary"
    if looks_like_initial:
        return "initial"
    return "unknown"


def discover_inputs(data_dir: Path) -> tuple[Path, Path, Path | None]:
    csv_files = sorted(data_dir.glob("*.csv"))
    if not csv_files:
        raise FileNotFoundError(f"No CSV files found in {data_dir}")

    classified: dict[str, list[Path]] = {
        "survey": [],
        "frame": [],
        "municipality_vote_totals": [],
        "unknown": [],
    }
    for path in csv_files:
        classified[classify_csv(path)].append(path)

    survey_path = classified["survey"][0] if classified["survey"] else None
    frame_path = classified["frame"][0] if classified["frame"] else None
    totals_path = (
        classified["municipality_vote_totals"][0]
        if classified["municipality_vote_totals"]
        else None
    )

    if survey_path is None:
      raise FileNotFoundError("Could not identify a survey CSV in python_test/data.")
    if frame_path is None:
      raise FileNotFoundError("Could not identify a frame CSV in python_test/data.")

    return survey_path, frame_path, totals_path


def parse_args() -> argparse.Namespace:
    repo_root = Path(__file__).resolve().parents[1]
    data_dir = repo_root / "python_test" / "data"
    output_dir = repo_root / "python_test" / "output"

    parser = argparse.ArgumentParser(
        description="Run the voteframe R module from Python."
    )
    parser.add_argument(
        "--survey",
        type=Path,
        default=None,
        help="Path to the survey CSV. Defaults to auto-detection in python_test/data.",
    )
    parser.add_argument(
        "--frame",
        type=Path,
        default=None,
        help="Path to the frame CSV. Defaults to auto-detection in python_test/data.",
    )
    parser.add_argument(
        "--vote-totals",
        type=Path,
        default=None,
        help="Path to the municipality vote totals CSV for the initial extension.",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=output_dir,
        help="Directory for generated CSV outputs.",
    )
    parser.add_argument(
        "--n-sims",
        type=int,
        default=250,
        help="Number of simulation draws for the secondary extension.",
    )
    parser.add_argument(
        "--year",
        type=int,
        default=None,
        help="Optional year filter for municipality vote totals in the initial extension.",
    )
    parser.add_argument(
        "--data-dir",
        type=Path,
        default=data_dir,
        help="Directory to scan when inputs are auto-detected.",
    )

    return parser.parse_args()


def main() -> None:
    args = parse_args()
    repo_root = Path(__file__).resolve().parents[1]

    if args.survey is None or args.frame is None:
        survey_path, frame_path, discovered_totals = discover_inputs(args.data_dir)
        if args.survey is None:
            args.survey = survey_path
        if args.frame is None:
            args.frame = frame_path
        if args.vote_totals is None:
            args.vote_totals = discovered_totals

    mode = infer_mode(args.survey, args.frame, args.vote_totals)
    if mode == "unknown":
        raise ValueError(
            "Could not match the provided files to the implemented initial or secondary schemas."
        )

    args.output_dir.mkdir(parents=True, exist_ok=True)

    if mode == "initial":
        if args.vote_totals is None:
            raise FileNotFoundError(
                "Initial extension mode requires a municipality vote totals CSV."
            )
        runner = repo_root / "scripts" / "run_initial_extension_cli.R"
        cmd = [
            "Rscript",
            str(runner),
            str(args.survey),
            str(args.frame),
            str(args.vote_totals),
            str(args.output_dir),
        ]
        if args.year is not None:
            cmd.append(str(args.year))
    else:
        runner = repo_root / "scripts" / "run_post_strat_cli.R"
        cmd = [
            "Rscript",
            str(runner),
            str(args.survey),
            str(args.frame),
            str(args.output_dir),
            str(args.n_sims),
        ]

    print(f"Detected mode: {mode}")
    print("Running:", " ".join(cmd))
    subprocess.run(cmd, check=True, cwd=repo_root)
    print(f"Finished. Outputs are in {args.output_dir}")


if __name__ == "__main__":
    try:
        main()
    except (FileNotFoundError, ValueError) as exc:
        print(exc, file=sys.stderr)
        raise SystemExit(1) from exc
