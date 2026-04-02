library(dplyr)
library(purrr)
library(readr)
library(tibble)
library(tidyr)
library(vglmer)

default_initial_extension_config <- function() {
  list(
    eps = 1e-8,
    verbose = TRUE,
    n_sims = 250,
    min_n = 80,
    min_events = 15,
    seed = NULL,
    year = NULL,
    drop_other_gender = TRUE,
    survey_aliases = list(
      education_level = c("education"),
      past_vote = c("vote_in_2022")
    ),
    frame_aliases = list(
      education_level = c("education")
    ),
    totals_aliases = list(
      vote_share = c("vote_pct", "pop_pct")
    ),
    other_left = c(
      "O: Enhedslisten",
      "Ø: Enhedslisten",
      "Å: Alternativet",
      "B: Det Radikale Venstre"
    ),
    other_right = c(
      "C: Det Konservative Folkeparti",
      "D: Nye Borgerlige",
      "O: Dansk Folkeparti"
    ),
    other = c(
      "Independent candidate",
      "Another party",
      "Voted blank"
    )
  )
}

make_initial_logger <- function(verbose = TRUE) {
  function(...) {
    if (isTRUE(verbose)) {
      message(...)
    }
  }
}

resolve_initial_extension_config <- function(config = list()) {
  resolved <- modifyList(default_initial_extension_config(), config)
  resolved$msg <- make_initial_logger(resolved$verbose)
  resolved
}

rename_initial_alias_columns <- function(dat, alias_map) {
  for (canonical in names(alias_map)) {
    if (canonical %in% names(dat)) {
      next
    }

    aliases <- alias_map[[canonical]]
    alias_hits <- aliases[aliases %in% names(dat)]

    if (length(alias_hits) > 0) {
      names(dat)[match(alias_hits[[1]], names(dat))] <- canonical
    }
  }

  dat
}

drop_index_columns <- function(dat) {
  dat %>%
    select(-matches("^(\\.\\.\\.1|Unnamed: 0(\\.1)?|X)$"))
}

validate_initial_required_columns <- function(dat, required, data_name) {
  missing_cols <- setdiff(required, names(dat))

  if (length(missing_cols) > 0) {
    stop("Missing columns in ", data_name, ": ", paste(missing_cols, collapse = ", "))
  }
}

recode_initial_party <- function(party, config) {
  case_when(
    party %in% config$other_left ~ "Other - Left",
    party %in% config$other_right ~ "Other - Right",
    party %in% config$other ~ "Other",
    TRUE ~ party
  )
}

resolve_initial_party_order <- function(survey_model) {
  party_counts <- survey_model %>%
    count(past_vote, sort = TRUE) %>%
    pull(past_vote)

  if ("Did not vote" %in% party_counts) {
    c("Did not vote", setdiff(party_counts, "Did not vote"))
  } else {
    party_counts
  }
}

prepare_initial_vote_totals <- function(vote_totals, config) {
  totals_required <- c("party", "municipality", "vote_share")

  vote_totals <- vote_totals %>%
    drop_index_columns() %>%
    rename_initial_alias_columns(config$totals_aliases)

  validate_initial_required_columns(vote_totals, totals_required, "vote_totals")

  if ("year" %in% names(vote_totals)) {
    available_years <- sort(unique(vote_totals$year[!is.na(vote_totals$year)]))

    if (!is.null(config$year)) {
      vote_totals <- vote_totals %>%
        filter(year == config$year)
    } else if (length(available_years) > 1) {
      chosen_year <- max(available_years)
      config$msg("Using latest year from vote_totals: ", chosen_year)
      vote_totals <- vote_totals %>%
        filter(year == chosen_year)
    }
  }

  vote_totals <- vote_totals %>%
    mutate(
      party = recode_initial_party(as.character(party), config),
      municipality = as.character(municipality),
      vote_share = as.numeric(vote_share)
    ) %>%
    filter(
      !is.na(party),
      !is.na(municipality),
      !is.na(vote_share)
    ) %>%
    group_by(municipality, party) %>%
    summarise(
      vote_pct = sum(vote_share, na.rm = TRUE),
      .groups = "drop"
    )

  if (nrow(vote_totals) == 0) {
    stop("No usable election results remain after filtering.")
  }

  vote_totals
}

prepare_initial_survey_data <- function(survey, config) {
  survey_required <- c(
    "past_vote",
    "gender",
    "age_group",
    "education_level",
    "municipality"
  )

  survey <- survey %>%
    drop_index_columns() %>%
    rename_initial_alias_columns(config$survey_aliases)

  validate_initial_required_columns(survey, survey_required, "survey")

  if (isTRUE(config$drop_other_gender) && "gender" %in% names(survey)) {
    survey <- survey %>%
      filter(gender != "Other")
  }

  survey_model <- survey %>%
    mutate(
      past_vote = recode_initial_party(as.character(past_vote), config),
      gender = as.character(gender),
      age_group = as.character(age_group),
      education_level = as.character(education_level),
      municipality = as.character(municipality)
    ) %>%
    filter(if_all(all_of(survey_required), ~ !is.na(.x) & .x != ""))

  if (nrow(survey_model) == 0) {
    stop("No complete cases remain in survey after filtering model variables.")
  }

  party_order <- resolve_initial_party_order(survey_model)

  if (length(party_order) < 2) {
    stop("Need at least two outcome categories in past_vote after filtering.")
  }

  survey_model %>%
    mutate(
      past_vote = factor(past_vote, levels = party_order),
      gender = factor(gender),
      age_group = factor(age_group),
      education_level = factor(education_level),
      municipality = factor(municipality)
    )
}

prepare_initial_frame_data <- function(frame, survey_model, config) {
  frame_required <- c(
    "gender",
    "age_group",
    "education_level",
    "municipality",
    "N"
  )

  frame <- frame %>%
    drop_index_columns() %>%
    rename_initial_alias_columns(config$frame_aliases)

  validate_initial_required_columns(frame, frame_required, "frame")

  frame_pred <- frame %>%
    mutate(
      gender = as.character(gender),
      age_group = as.character(age_group),
      education_level = as.character(education_level),
      municipality = as.character(municipality),
      N = as.numeric(N)
    ) %>%
    filter(
      !is.na(gender),
      !is.na(age_group),
      !is.na(education_level),
      !is.na(municipality),
      !is.na(N),
      N > 0
    )

  if (nrow(frame_pred) == 0) {
    stop("No usable rows remain in frame after filtering.")
  }

  survey_levels <- list(
    gender = levels(survey_model$gender),
    age_group = levels(survey_model$age_group),
    education_level = levels(survey_model$education_level),
    municipality = levels(survey_model$municipality)
  )

  frame_unseen <- list(
    gender = setdiff(unique(frame_pred$gender), survey_levels$gender),
    age_group = setdiff(unique(frame_pred$age_group), survey_levels$age_group),
    education_level = setdiff(unique(frame_pred$education_level), survey_levels$education_level),
    municipality = setdiff(unique(frame_pred$municipality), survey_levels$municipality)
  )

  if (length(frame_unseen$gender) > 0) {
    stop("Unseen gender levels in frame: ", paste(frame_unseen$gender, collapse = ", "))
  }
  if (length(frame_unseen$age_group) > 0) {
    stop("Unseen age_group levels in frame: ", paste(frame_unseen$age_group, collapse = ", "))
  }
  if (length(frame_unseen$education_level) > 0) {
    stop("Unseen education_level levels in frame: ", paste(frame_unseen$education_level, collapse = ", "))
  }

  if (length(frame_unseen$municipality) > 0) {
    config$msg(
      "There are ", length(frame_unseen$municipality),
      " municipalities in the frame not seen in the survey. ",
      "These rows will be excluded from municipality-level summaries ",
      "because municipality becomes NA after factor alignment."
    )
  }

  frame_pred %>%
    mutate(
      gender = factor(gender, levels = survey_levels$gender),
      age_group = factor(age_group, levels = survey_levels$age_group),
      education_level = factor(education_level, levels = survey_levels$education_level),
      municipality = factor(municipality, levels = survey_levels$municipality)
    )
}

add_initial_interactions <- function(dat,
                                     age_edu_levels = NULL,
                                     age_gender_levels = NULL,
                                     gender_edu_levels = NULL) {
  age_edu_raw <- interaction(dat$age_group, dat$education_level, drop = TRUE, sep = "___")
  age_gender_raw <- interaction(dat$age_group, dat$gender, drop = TRUE, sep = "___")
  gender_edu_raw <- interaction(dat$gender, dat$education_level, drop = TRUE, sep = "___")

  dat$age_edu <- if (is.null(age_edu_levels)) {
    factor(age_edu_raw)
  } else {
    factor(as.character(age_edu_raw), levels = age_edu_levels)
  }

  dat$age_gender <- if (is.null(age_gender_levels)) {
    factor(age_gender_raw)
  } else {
    factor(as.character(age_gender_raw), levels = age_gender_levels)
  }

  dat$gender_edu <- if (is.null(gender_edu_levels)) {
    factor(gender_edu_raw)
  } else {
    factor(as.character(gender_edu_raw), levels = gender_edu_levels)
  }

  dat
}

make_initial_stage_data <- function(data, party_name, vote_lookup,
                                    age_edu_levels = NULL,
                                    age_gender_levels = NULL,
                                    gender_edu_levels = NULL) {
  lookup_sub <- vote_lookup %>%
    filter(party == party_name) %>%
    transmute(
      municipality_chr = as.character(municipality),
      vote_pct_stage = as.numeric(vote_pct)
    )

  joined <- data %>%
    select(-any_of(c(
      "vote_pct", "vote_pct_scaled", "vote_pct_stage",
      "age_edu", "age_gender", "gender_edu"
    ))) %>%
    mutate(municipality_chr = as.character(municipality)) %>%
    left_join(lookup_sub, by = "municipality_chr") %>%
    mutate(
      vote_pct = coalesce(vote_pct_stage, 0),
      vote_pct_scaled = as.numeric(scale(vote_pct)),
      vote_pct_scaled = if_else(is.na(vote_pct_scaled), 0, vote_pct_scaled)
    ) %>%
    select(-municipality_chr, -vote_pct_stage)

  add_initial_interactions(
    joined,
    age_edu_levels = age_edu_levels,
    age_gender_levels = age_gender_levels,
    gender_edu_levels = gender_edu_levels
  )
}

fit_initial_stage <- function(dat, party_name, config) {
  d <- dat %>%
    mutate(
      y = as.integer(past_vote == party_name),
      gender = droplevels(gender),
      age_group = droplevels(age_group),
      education_level = droplevels(education_level),
      municipality = droplevels(municipality),
      past_vote = droplevels(past_vote)
    )

  d <- add_initial_interactions(d)

  n_total <- nrow(d)
  n_event <- sum(d$y, na.rm = TRUE)
  use_interactions <- n_total >= 300

  fallback_prob <- if (n_total > 0) n_event / n_total else 0
  fallback_prob <- pmin(pmax(fallback_prob, config$eps), 1 - config$eps)

  if (n_total < config$min_n || n_event < config$min_events || (n_total - n_event) < config$min_events) {
    return(list(
      fit = NULL,
      fallback_prob = fallback_prob,
      n_total = n_total,
      n_event = n_event,
      status = "fallback_sparse"
    ))
  }

  formula_full <- if (use_interactions) {
    y ~ v_s(vote_pct) +
      (1 | municipality) +
      (1 | gender) +
      (1 | age_group) +
      (1 | education_level) +
      (1 | age_edu) +
      (1 | age_gender) +
      (1 | gender_edu)
  } else {
    y ~ v_s(vote_pct) +
      (1 | municipality) +
      (1 | gender) +
      (1 | age_group) +
      (1 | education_level)
  }

  fit_full <- tryCatch(
    vglmer(
      formula_full,
      data = d,
      family = "binomial",
      control = vglmer_control(iterations = 15000)
    ),
    error = function(e) {
      config$msg("fit_full failed for ", party_name)
      config$msg("  error: ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(fit_full)) {
    return(list(
      fit = fit_full,
      fallback_prob = fallback_prob,
      n_total = n_total,
      n_event = n_event,
      status = "fit_ok_full",
      age_edu_levels = levels(d$age_edu),
      age_gender_levels = levels(d$age_gender),
      gender_edu_levels = levels(d$gender_edu)
    ))
  }

  fit_simple <- tryCatch(
    vglmer(
      y ~ gender + age_group + education_level + vote_pct,
      data = d,
      family = "binomial",
      control = vglmer_control(iterations = 5000)
    ),
    error = function(e) {
      config$msg("fit_simple failed for ", party_name, ": ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(fit_simple)) {
    return(list(
      fit = fit_simple,
      fallback_prob = fallback_prob,
      n_total = n_total,
      n_event = n_event,
      status = "fit_ok_simple"
    ))
  }

  list(
    fit = NULL,
    fallback_prob = fallback_prob,
    n_total = n_total,
    n_event = n_event,
    status = "fallback_error"
  )
}

fit_initial_stickbreaking_models <- function(survey_model, parties, election_results, config) {
  sb_fits <- vector("list", length(parties) - 1)
  names(sb_fits) <- parties[seq_len(length(parties) - 1)]
  remaining_parties <- parties

  for (k in seq_len(length(parties) - 1)) {
    current_party <- remaining_parties[[1]]

    d_k_base <- survey_model %>%
      filter(past_vote %in% remaining_parties) %>%
      mutate(past_vote = droplevels(past_vote)) %>%
      droplevels()

    d_k <- make_initial_stage_data(d_k_base, current_party, election_results)
    stage_fit <- fit_initial_stage(d_k, current_party, config)
    sb_fits[[k]] <- stage_fit

    config$msg(
      "Stage ", k, "/", length(parties) - 1, " [", current_party, "] : ",
      stage_fit$status, " | n=", stage_fit$n_total,
      " | events=", stage_fit$n_event,
      " | fallback=", round(stage_fit$fallback_prob, 6)
    )

    remaining_parties <- remaining_parties[-1]
  }

  sb_fits
}

predict_initial_stage_point <- function(stage_obj, newdata, config) {
  if (is.null(stage_obj$fit)) {
    return(rep(stage_obj$fallback_prob, nrow(newdata)))
  }

  eta <- tryCatch(
    predict_MAVB(
      stage_obj$fit,
      newdata = newdata,
      samples = 1,
      summary = TRUE,
      allow_missing_levels = TRUE
    ),
    error = function(e) {
      config$msg("Point prediction failed; reverting to fallback: ", conditionMessage(e))
      e
    }
  )

  if (inherits(eta, "error")) {
    return(rep(stage_obj$fallback_prob, nrow(newdata)))
  }

  if (is.list(eta)) {
    if ("mean" %in% names(eta)) {
      eta <- eta$mean
    } else if ("fit" %in% names(eta)) {
      eta <- eta$fit
    } else if ("pred" %in% names(eta)) {
      eta <- eta$pred
    } else if (length(eta) == 1) {
      eta <- eta[[1]]
    } else {
      config$msg("Unknown list structure from predict_MAVB(); using fallback")
      return(rep(stage_obj$fallback_prob, nrow(newdata)))
    }
  }

  eta <- as.numeric(eta)
  p <- plogis(eta)
  pmin(pmax(p, config$eps), 1 - config$eps)
}

predict_initial_stage_draws <- function(stage_obj, newdata, config) {
  if (is.null(stage_obj$fit)) {
    return(matrix(stage_obj$fallback_prob, nrow = nrow(newdata), ncol = config$n_sims))
  }

  eta_draws <- tryCatch(
    predict_MAVB(
      stage_obj$fit,
      newdata = newdata,
      samples = config$n_sims,
      summary = FALSE,
      allow_missing_levels = TRUE
    ),
    error = function(e) e
  )

  if (inherits(eta_draws, "error")) {
    config$msg("Simulation prediction failed; reverting to fallback: ", conditionMessage(eta_draws))
    return(matrix(stage_obj$fallback_prob, nrow = nrow(newdata), ncol = config$n_sims))
  }

  eta_draws <- as.matrix(eta_draws)

  if (nrow(eta_draws) == config$n_sims && ncol(eta_draws) == nrow(newdata)) {
    eta_draws <- t(eta_draws)
  } else if (!(nrow(eta_draws) == nrow(newdata) && ncol(eta_draws) == config$n_sims)) {
    stop(
      "Unexpected dimensions from predict_MAVB(): got ",
      nrow(eta_draws), " x ", ncol(eta_draws),
      ", expected either ", nrow(newdata), " x ", config$n_sims,
      " or ", config$n_sims, " x ", nrow(newdata), "."
    )
  }

  p_draws <- plogis(eta_draws)
  pmin(pmax(p_draws, config$eps), 1 - config$eps)
}

normalize_initial_probability_matrix <- function(prob_mat) {
  n_parties <- ncol(prob_mat)
  prob_mat[prob_mat < 0] <- 0
  row_sums <- rowSums(prob_mat)

  bad_rows <- which(!is.finite(row_sums) | row_sums <= 0)
  if (length(bad_rows) > 0) {
    warning(length(bad_rows), " rows had invalid probability sums; replacing with uniform distribution.")
    prob_mat[bad_rows, ] <- 1 / n_parties
    row_sums[bad_rows] <- 1
  }

  prob_mat / row_sums
}

build_initial_probability_matrix <- function(pi_components, parties) {
  n_rows <- nrow(pi_components[[1]])
  n_parties <- length(parties)
  prob_mat <- matrix(0, nrow = n_rows, ncol = n_parties)
  colnames(prob_mat) <- parties
  remaining_mass <- rep(1, n_rows)

  for (k in seq_len(n_parties - 1)) {
    prob_mat[, k] <- remaining_mass * pi_components[[k]]
    remaining_mass <- remaining_mass * (1 - pi_components[[k]])
  }

  prob_mat[, n_parties] <- remaining_mass
  normalize_initial_probability_matrix(prob_mat)
}

compute_initial_point_estimates <- function(prob_mat, weights) {
  as_tibble(prob_mat) %>%
    mutate(weight = weights) %>%
    summarise(across(-weight, ~ weighted.mean(.x, w = weight, na.rm = TRUE))) %>%
    pivot_longer(
      everything(),
      names_to = "party",
      values_to = "point_estimate"
    ) %>%
    arrange(desc(point_estimate))
}

compute_initial_share_draws <- function(pi_draws, parties, weights) {
  share_draws <- matrix(NA_real_, nrow = ncol(pi_draws[[1]]), ncol = length(parties))
  colnames(share_draws) <- parties

  for (s in seq_len(ncol(pi_draws[[1]]))) {
    pi_components_s <- lapply(pi_draws, function(draw_mat) draw_mat[, s, drop = FALSE])
    prob_mat_s <- build_initial_probability_matrix(pi_components_s, parties)

    share_draws[s, ] <- apply(
      prob_mat_s,
      2,
      weighted.mean,
      w = weights,
      na.rm = TRUE
    )
  }

  share_draws
}

build_initial_quartile_table <- function(share_draws, parties, point_estimates) {
  tibble(
    party = parties,
    lower_quartile = apply(share_draws, 2, quantile, probs = 0.25, na.rm = TRUE),
    median = apply(share_draws, 2, quantile, probs = 0.50, na.rm = TRUE),
    upper_quartile = apply(share_draws, 2, quantile, probs = 0.75, na.rm = TRUE)
  ) %>%
    left_join(point_estimates, by = "party") %>%
    select(party, point_estimate, lower_quartile, median, upper_quartile) %>%
    arrange(desc(point_estimate))
}

build_initial_extended_frame <- function(prob_mat, frame_pred) {
  as_tibble(prob_mat) %>%
    mutate(cell_id = seq_len(nrow(frame_pred))) %>%
    pivot_longer(
      cols = -cell_id,
      names_to = "past_vote",
      values_to = "prob"
    ) %>%
    left_join(
      frame_pred %>%
        mutate(cell_id = seq_len(n())),
      by = "cell_id"
    ) %>%
    mutate(expected_N = N * prob) %>%
    select(
      cell_id,
      age_group,
      gender,
      municipality,
      education_level,
      N,
      past_vote,
      prob,
      expected_N
    )
}

compute_initial_stage_diagnostics <- function(sb_fits, parties) {
  tibble(
    stage = seq_len(length(parties) - 1),
    party = parties[seq_len(length(parties) - 1)],
    status = map_chr(sb_fits, "status"),
    n_total = map_dbl(sb_fits, "n_total"),
    n_event = map_dbl(sb_fits, "n_event"),
    fallback_prob = map_dbl(sb_fits, "fallback_prob")
  )
}

compute_initial_aggregate_counts <- function(extended_frame) {
  extended_frame %>%
    group_by(past_vote) %>%
    summarise(
      expected_total = sum(expected_N, na.rm = TRUE),
      expected_share = sum(expected_N, na.rm = TRUE) / sum(N[!duplicated(cell_id)]),
      .groups = "drop"
    ) %>%
    arrange(desc(expected_share))
}

compute_initial_municipality_point <- function(prob_mat, frame_pred, parties) {
  valid_rows <- !is.na(frame_pred$municipality)

  as_tibble(prob_mat[valid_rows, , drop = FALSE]) %>%
    mutate(
      municipality = as.character(frame_pred$municipality[valid_rows]),
      N = frame_pred$N[valid_rows]
    ) %>%
    pivot_longer(
      cols = all_of(parties),
      names_to = "past_vote",
      values_to = "prob"
    ) %>%
    mutate(expected_N = N * prob) %>%
    group_by(municipality, past_vote) %>%
    summarise(
      expected_N = sum(expected_N, na.rm = TRUE),
      total_N = sum(N, na.rm = TRUE),
      point_estimate = expected_N / total_N,
      .groups = "drop"
    )
}

compute_initial_municipality_draws <- function(pi_draws, frame_pred, parties) {
  valid_rows <- !is.na(frame_pred$municipality)
  municipality_valid <- as.character(frame_pred$municipality[valid_rows])
  weights_valid <- frame_pred$N[valid_rows]

  bind_rows(lapply(seq_len(ncol(pi_draws[[1]])), function(s) {
    pi_components_s <- lapply(pi_draws, function(draw_mat) draw_mat[, s, drop = FALSE])
    prob_mat_s <- build_initial_probability_matrix(pi_components_s, parties)

    as_tibble(prob_mat_s[valid_rows, , drop = FALSE]) %>%
      mutate(
        municipality = municipality_valid,
        N = weights_valid,
        draw = s
      ) %>%
      pivot_longer(
        cols = all_of(parties),
        names_to = "past_vote",
        values_to = "prob"
      ) %>%
      mutate(expected_N = N * prob) %>%
      group_by(draw, municipality, past_vote) %>%
      summarise(
        expected_N = sum(expected_N, na.rm = TRUE),
        total_N = sum(N, na.rm = TRUE),
        share = expected_N / total_N,
        .groups = "drop"
      )
  }))
}

compute_initial_municipality_quartiles <- function(municipality_party_draws, municipality_party_point) {
  municipality_party_draws %>%
    group_by(municipality, past_vote) %>%
    summarise(
      lower_quartile = quantile(share, probs = 0.25, na.rm = TRUE),
      median = quantile(share, probs = 0.50, na.rm = TRUE),
      upper_quartile = quantile(share, probs = 0.75, na.rm = TRUE),
      sd_draws = sd(share, na.rm = TRUE),
      n_distinct_draws = n_distinct(share),
      .groups = "drop"
    ) %>%
    left_join(municipality_party_point, by = c("municipality", "past_vote")) %>%
    select(
      municipality,
      past_vote,
      point_estimate,
      lower_quartile,
      median,
      upper_quartile,
      sd_draws,
      n_distinct_draws
    ) %>%
    arrange(municipality, desc(point_estimate))
}

build_initial_share_draws_long <- function(share_draws) {
  as_tibble(share_draws) %>%
    mutate(draw = seq_len(n())) %>%
    pivot_longer(
      cols = -draw,
      names_to = "past_vote",
      values_to = "share"
    )
}

run_initial_extension <- function(skeleton_frame, auxiliary_survey, vote_totals, config = list()) {
  config <- resolve_initial_extension_config(config)
  metadata_config <- config
  metadata_config$msg <- NULL

  if (!is.null(config$seed)) {
    set.seed(config$seed)
  }

  election_results <- prepare_initial_vote_totals(vote_totals, config)
  survey_model <- prepare_initial_survey_data(auxiliary_survey, config)
  parties <- levels(survey_model$past_vote)
  frame_pred <- prepare_initial_frame_data(skeleton_frame, survey_model, config)

  survey_extra_parties <- setdiff(parties, unique(election_results$party))
  if (length(survey_extra_parties) > 0) {
    config$msg(
      "These parties appear in the survey but not in vote totals; they will use vote_pct = 0 when joined: ",
      paste(sort(survey_extra_parties), collapse = ", ")
    )
  }

  sb_fits <- fit_initial_stickbreaking_models(survey_model, parties, election_results, config)

  pi_mat <- matrix(NA_real_, nrow = nrow(frame_pred), ncol = length(parties) - 1)
  colnames(pi_mat) <- parties[seq_len(length(parties) - 1)]

  for (k in seq_len(length(parties) - 1)) {
    stage_newdata <- make_initial_stage_data(
      frame_pred,
      parties[[k]],
      election_results,
      age_edu_levels = sb_fits[[k]]$age_edu_levels,
      age_gender_levels = sb_fits[[k]]$age_gender_levels,
      gender_edu_levels = sb_fits[[k]]$gender_edu_levels
    )
    pi_mat[, k] <- predict_initial_stage_point(sb_fits[[k]], stage_newdata, config)
  }

  pi_components <- lapply(seq_len(ncol(pi_mat)), function(k) pi_mat[, k, drop = FALSE])
  prob_mat <- build_initial_probability_matrix(pi_components, parties)
  point_estimates <- compute_initial_point_estimates(prob_mat, frame_pred$N)

  pi_draws <- vector("list", length(parties) - 1)
  for (k in seq_len(length(parties) - 1)) {
    config$msg("Simulation draws for stage ", k, "/", length(parties) - 1, " [", parties[[k]], "]")
    stage_newdata <- make_initial_stage_data(
      frame_pred,
      parties[[k]],
      election_results,
      age_edu_levels = sb_fits[[k]]$age_edu_levels,
      age_gender_levels = sb_fits[[k]]$age_gender_levels,
      gender_edu_levels = sb_fits[[k]]$gender_edu_levels
    )
    pi_draws[[k]] <- predict_initial_stage_draws(sb_fits[[k]], stage_newdata, config)
  }

  share_draws <- compute_initial_share_draws(pi_draws, parties, frame_pred$N)
  quartile_table <- build_initial_quartile_table(share_draws, parties, point_estimates)
  extended_frame <- build_initial_extended_frame(prob_mat, frame_pred)
  stage_diagnostics <- compute_initial_stage_diagnostics(sb_fits, parties)
  aggregate_counts <- compute_initial_aggregate_counts(extended_frame)
  municipality_party_point <- compute_initial_municipality_point(prob_mat, frame_pred, parties)
  municipality_party_draws <- compute_initial_municipality_draws(pi_draws, frame_pred, parties)
  municipality_party_quartiles <- compute_initial_municipality_quartiles(
    municipality_party_draws,
    municipality_party_point
  )
  share_draws_long <- build_initial_share_draws_long(share_draws)

  list(
    point_estimates = point_estimates,
    quartile_table = quartile_table,
    extended_frame = extended_frame,
    stage_diagnostics = stage_diagnostics,
    aggregate_counts = aggregate_counts,
    share_draws = as_tibble(share_draws),
    share_draws_long = share_draws_long,
    cell_party_probabilities = as_tibble(prob_mat),
    stickbreaking_conditional_probs = as_tibble(pi_mat),
    municipality_party_point = municipality_party_point,
    municipality_party_draws = municipality_party_draws,
    municipality_party_quartiles = municipality_party_quartiles,
    election_results = election_results,
    metadata = list(
      parties = parties,
      config = metadata_config
    )
  )
}

write_initial_extension_outputs <- function(result, output_dir) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  write_csv(result$point_estimates, file.path(output_dir, "initial_point_estimates.csv"))
  write_csv(result$quartile_table, file.path(output_dir, "initial_quartile_table.csv"))
  write_csv(result$extended_frame, file.path(output_dir, "initial_extended_frame.csv"))
  write_csv(result$stage_diagnostics, file.path(output_dir, "initial_stage_diagnostics.csv"))
  write_csv(result$aggregate_counts, file.path(output_dir, "initial_aggregate_counts.csv"))
  write_csv(result$share_draws, file.path(output_dir, "initial_share_draws.csv"))
  write_csv(result$share_draws_long, file.path(output_dir, "initial_share_draws_long.csv"))
  write_csv(result$cell_party_probabilities, file.path(output_dir, "initial_cell_party_probabilities.csv"))
  write_csv(
    result$stickbreaking_conditional_probs,
    file.path(output_dir, "initial_stickbreaking_conditional_probs.csv")
  )
  write_csv(
    result$municipality_party_point,
    file.path(output_dir, "initial_municipality_party_point_estimates.csv")
  )
  write_csv(
    result$municipality_party_draws,
    file.path(output_dir, "initial_municipality_party_draws_long.csv")
  )
  write_csv(
    result$municipality_party_quartiles,
    file.path(output_dir, "initial_municipality_party_quartiles.csv")
  )

  invisible(result)
}
