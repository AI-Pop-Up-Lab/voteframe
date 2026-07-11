library(dplyr)
library(purrr)
library(readr)
library(tibble)
library(tidyr)
library(vglmer)

default_us_extension_config <- function() {
  list(
    eps = 1e-8,
    verbose = TRUE,
    n_sims = 250,
    min_n = 80,
    min_events = 15,
    seed = NULL,
    drop_other_gender = TRUE,
    survey_aliases = list(
      education_level = c("education", "educ")
    ),
    frame_aliases = list(
      education_level = c("education", "educ")
    ),
    # Fixed configuration naming and keys to align with downstream functions
    share_col_mapping = list(
      "Democratic"     = c(cong = "dem_share",       pres = "state_pres_dem_share"),
      "Republican"   = c(cong = "rep_share",       pres = "state_pres_rep_share"),
      "Other"        = c(cong = "other_share",     pres = "state_pres_other_share"),
      "Did not vote" = c(cong = "no_vote_share",   pres = "state_pres_no_vote_share")
    )
  )
}

make_us_logger <- function(verbose = TRUE) {
  function(...) {
    if (isTRUE(verbose)) {
      message(...)
    }
  }
}

resolve_us_extension_config <- function(config = list()) {
  resolved <- modifyList(default_us_extension_config(), config)
  resolved$msg <- make_us_logger(resolved$verbose)
  resolved
}

rename_us_alias_columns <- function(dat, alias_map) {
  for (canonical in names(alias_map)) {
    if (canonical %in% names(dat)) next
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

validate_us_required_columns <- function(dat, required, data_name) {
  missing_cols <- setdiff(required, names(dat))
  if (length(missing_cols) > 0) {
    stop("Missing columns in ", data_name, ": ", paste(missing_cols, collapse = ", "))
  }
}

resolve_us_party_order <- function(survey_model) {
  party_counts <- survey_model %>%
    count(past_vote, sort = TRUE) %>%
    pull(past_vote)
  
  if ("Did not vote" %in% party_counts) {
    c("Did not vote", setdiff(party_counts, "Did not vote"))
  } else {
    party_counts
  }
}

prepare_us_area_shares <- function(area_shares, config) {
  required_cols <- c("state_cd", "state_abbrv")
  
  # Adjusted mapping access to use correct keys
  for (mapping in config$share_col_mapping) {
    required_cols <- c(required_cols, mapping["cong"], mapping["pres"])
  }
  required_cols <- unique(required_cols)
  
  area_shares <- drop_index_columns(area_shares)
  validate_us_required_columns(area_shares, required_cols, "area_level_vote_shares")
  
  area_shares %>%
    mutate(
      state_cd = as.character(state_cd),
      state_abbrv = as.character(state_abbrv)
    ) %>%
    distinct()
}

prepare_us_survey_data <- function(survey, config) {
  survey_required <- c(
    "past_vote", "gender", "race", "age_group", "education_level", 
    "state_abbrv", "state_cd"
  )

  survey <- survey %>%
    drop_index_columns() %>%
    rename_us_alias_columns(config$survey_aliases)

  validate_us_required_columns(survey, survey_required, "survey")

  if (isTRUE(config$drop_other_gender) && "gender" %in% names(survey)) {
    survey <- survey %>% filter(gender != "Other")
  }

  survey_model <- survey %>%
    mutate(
      past_vote = as.character(past_vote),
      gender = as.character(gender),
      race = as.character(race),
      age_group = as.character(age_group),
      education_level = as.character(education_level),
      state_abbrv = as.character(state_abbrv),
      state_cd = as.character(state_cd)
    ) %>%
    filter(if_all(all_of(survey_required), ~ !is.na(.x) & .x != ""))

  if (nrow(survey_model) == 0) {
    stop("No complete cases remain in survey after filtering model variables.")
  }

  party_order <- resolve_us_party_order(survey_model)

  if (length(party_order) < 2) {
    stop("Need at least two outcome categories in past_vote after filtering.")
  }

  survey_model %>%
    mutate(
      past_vote = factor(past_vote, levels = party_order),
      gender = factor(gender),
      race = factor(race),
      age_group = factor(age_group),
      education_level = factor(education_level),
      state_abbrv = factor(state_abbrv),
      state_cd = factor(state_cd)
    )
}

prepare_us_frame_data <- function(frame, survey_model, config) {
  frame_required <- c(
    "gender", "race", "age_group", "education_level", 
    "state_abbrv", "state_cd", "N"
  )

  frame <- frame %>%
    drop_index_columns() %>%
    rename_us_alias_columns(config$frame_aliases)

  validate_us_required_columns(frame, frame_required, "frame")

  frame_pred <- frame %>%
    mutate(
      gender = as.character(gender),
      race = as.character(race),
      age_group = as.character(age_group),
      education_level = as.character(education_level),
      state_abbrv = as.character(state_abbrv),
      state_cd = as.character(state_cd),
      N = as.numeric(N)
    ) %>%
    filter(!is.na(gender), !is.na(race), !is.na(age_group), !is.na(education_level), 
           !is.na(state_abbrv), !is.na(state_cd), !is.na(N), N > 0)

  survey_levels <- list(
    gender = levels(survey_model$gender),
    race = levels(survey_model$race),
    age_group = levels(survey_model$age_group),
    education_level = levels(survey_model$education_level),
    state_abbrv = levels(survey_model$state_abbrv),
    state_cd = levels(survey_model$state_cd)
  )

  frame_unseen <- list(
    gender = setdiff(unique(frame_pred$gender), survey_levels$gender),
    race = setdiff(unique(frame_pred$race), survey_levels$race),
    age_group = setdiff(unique(frame_pred$age_group), survey_levels$age_group),
    education_level = setdiff(unique(frame_pred$education_level), survey_levels$education_level)
  )

  if (length(frame_unseen$gender) > 0) stop("Unseen gender levels in frame: ", paste(frame_unseen$gender, collapse = ", "))
  if (length(frame_unseen$race) > 0) stop("Unseen race levels in frame: ", paste(frame_unseen$race, collapse = ", "))
  if (length(frame_unseen$age_group) > 0) stop("Unseen age_group levels in frame: ", paste(frame_unseen$age_group, collapse = ", "))
  if (length(frame_unseen$education_level) > 0) stop("Unseen education levels in frame: ", paste(frame_unseen$education_level, collapse = ", "))

  frame_pred %>%
    mutate(
      gender = factor(gender, levels = survey_levels$gender),
      race = factor(race, levels = survey_levels$race),
      age_group = factor(age_group, levels = survey_levels$age_group),
      education_level = factor(education_level, levels = survey_levels$education_level),
      state_abbrv = factor(state_abbrv, levels = survey_levels$state_abbrv),
      state_cd = factor(state_cd, levels = survey_levels$state_cd)
    )
}

add_us_interactions <- function(dat, 
                                race_edu_levels = NULL, 
                                race_gender_levels = NULL, 
                                gender_edu_levels = NULL,
                                race_age_levels = NULL,
                                age_edu_levels = NULL,
                                age_gender_levels = NULL) {
  race_edu_raw    <- interaction(dat$race, dat$education_level, drop = TRUE, sep = "___")
  race_gender_raw <- interaction(dat$race, dat$gender, drop = TRUE, sep = "___")
  gender_edu_raw  <- interaction(dat$gender, dat$education_level, drop = TRUE, sep = "___")

  race_age_raw    <- interaction(dat$race, dat$age_group, drop = TRUE, sep = "___")
  age_edu_raw     <- interaction(dat$age_group, dat$education_level, drop = TRUE, sep = "___")
  age_gender_raw  <- interaction(dat$age_group, dat$gender, drop = TRUE, sep = "___")

  dat$race_edu    <- if (is.null(race_edu_levels)) factor(race_edu_raw) else factor(as.character(race_edu_raw), levels = race_edu_levels)
  dat$race_gender <- if (is.null(race_gender_levels)) factor(race_gender_raw) else factor(as.character(race_gender_raw), levels = race_gender_levels)
  dat$gender_edu  <- if (is.null(gender_edu_levels)) factor(gender_edu_raw) else factor(as.character(gender_edu_raw), levels = gender_edu_levels)
  
  dat$race_age    <- if (is.null(race_age_levels)) factor(race_age_raw) else factor(as.character(race_age_raw), levels = race_age_levels)
  dat$age_edu     <- if (is.null(age_edu_levels)) factor(age_edu_raw) else factor(as.character(age_edu_raw), levels = age_edu_levels)
  dat$age_gender  <- if (is.null(age_gender_levels)) factor(age_gender_raw) else factor(as.character(age_gender_raw), levels = age_gender_levels)

  dat
}

make_us_stage_data <- function(data, party_name, area_shares, config,
                               race_edu_levels = NULL,
                               race_gender_levels = NULL,
                               gender_edu_levels = NULL,
                               race_age_levels = NULL,
                               age_edu_levels = NULL,
                               age_gender_levels = NULL) {
  
  if (!(party_name %in% names(config$share_col_mapping))) {
    stop("Party '", party_name, "' not found in config$share_col_mapping.")
  }
  
  share_cols <- config$share_col_mapping[[party_name]]
  cong_col <- share_cols[["cong"]]
  pres_col <- share_cols[["pres"]]

  lookup_sub <- area_shares %>%
    transmute(
      state_cd_chr = as.character(state_cd),
      cong_share_stage = as.numeric(!!sym(cong_col)),
      pres_share_stage = as.numeric(!!sym(pres_col))
    )

  joined <- data %>%
    select(-any_of(c(
      "cong_share", "pres_share", "cong_share_scaled", "pres_share_scaled", 
      "race_edu", "race_gender", "gender_edu", "race_age", "age_edu", "age_gender"
    ))) %>%
    mutate(state_cd_chr = as.character(state_cd)) %>%
    left_join(lookup_sub, by = "state_cd_chr") %>%
    mutate(
      cong_share = coalesce(cong_share_stage, 0),
      pres_share = coalesce(pres_share_stage, 0),
      cong_share_scaled = as.numeric(scale(cong_share)),
      pres_share_scaled = as.numeric(scale(pres_share)),
      cong_share_scaled = if_else(is.na(cong_share_scaled), 0, cong_share_scaled),
      pres_share_scaled = if_else(is.na(pres_share_scaled), 0, pres_share_scaled)
    ) %>%
    select(-state_cd_chr, -cong_share_stage, -pres_share_stage)

  add_us_interactions(
    joined,
    race_edu_levels = race_edu_levels,
    race_gender_levels = race_gender_levels,
    gender_edu_levels = gender_edu_levels,
    race_age_levels = race_age_levels,
    age_edu_levels = age_edu_levels,
    age_gender_levels = age_gender_levels
  )
}

fit_us_stage <- function(dat, party_name, config) {
  d <- dat %>%
    mutate(
      y = as.integer(past_vote == party_name),
      gender = droplevels(gender),
      race = droplevels(race),
      age_group = droplevels(age_group),
      education_level = droplevels(education_level),
      state_abbrv = droplevels(state_abbrv),
      state_cd = droplevels(state_cd),
      past_vote = droplevels(past_vote)
    )

  d <- add_us_interactions(d)

  n_total <- nrow(d)
  n_event <- sum(d$y, na.rm = TRUE)
  use_interactions <- n_total >= 300

  fallback_prob <- if (n_total > 0) n_event / n_total else 0
  fallback_prob <- pmin(pmax(fallback_prob, config$eps), 1 - config$eps)

  if (n_total < config$min_n || n_event < config$min_events || (n_total - n_event) < config$min_events) {
    return(list(fit = NULL, fallback_prob = fallback_prob, n_total = n_total, n_event = n_event, status = "fallback_sparse"))
  }

  formula_full <- if (use_interactions) {
    y ~ v_s(cong_share) + v_s(pres_share) +
      (1 | state_abbrv) + (1 | state_cd) +
      (1 | gender) + (1 | race) + (1 | age_group) + (1 | education_level) +
      (1 | race_edu) + (1 | race_gender) + (1 | gender_edu) +
      (1 | race_age) + (1 | age_edu) + (1 | age_gender)
  } else {
    y ~ v_s(cong_share) + v_s(pres_share) +
      (1 | state_abbrv) + (1 | state_cd) +
      (1 | gender) + (1 | race) + (1 | age_group) + (1 | education_level)
  }

  fit_full <- tryCatch(
    vglmer(formula_full, data = d, family = "binomial", control = vglmer_control(iterations = 15000)),
    error = function(e) {
      config$msg("fit_full failed for ", party_name, ". Error: ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(fit_full)) {
    return(list(
      fit = fit_full, fallback_prob = fallback_prob, n_total = n_total, n_event = n_event, status = "fit_ok_full",
      race_edu_levels = levels(d$race_edu), race_gender_levels = levels(d$race_gender), gender_edu_levels = levels(d$gender_edu),
      race_age_levels = levels(d$race_age), age_edu_levels = levels(d$age_edu), age_gender_levels = levels(d$age_gender)
    ))
  }

  fit_simple <- tryCatch(
    vglmer(y ~ gender + race + age_group + education_level + cong_share + pres_share, data = d, family = "binomial", control = vglmer_control(iterations = 5000)),
    error = function(e) {
      config$msg("fit_simple failed for ", party_name, ": ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(fit_simple)) {
    return(list(fit = fit_simple, fallback_prob = fallback_prob, n_total = n_total, n_event = n_event, status = "fit_ok_simple"))
  }

  list(fit = NULL, fallback_prob = fallback_prob, n_total = n_total, n_event = n_event, status = "fallback_error")
}

fit_us_stickbreaking_models <- function(survey_model, parties, area_shares, config) {
  sb_fits <- vector("list", length(parties) - 1)
  names(sb_fits) <- parties[seq_len(length(parties) - 1)]
  remaining_parties <- parties
  
  for (k in seq_len(length(parties) - 1)) {
    current_party <- remaining_parties[[1]]
    
    d_k_base <- survey_model %>%
      filter(past_vote %in% remaining_parties) %>%
      mutate(past_vote = droplevels(past_vote)) %>%
      droplevels()
    
    d_k <- make_us_stage_data(d_k_base, current_party, area_shares, config)
    stage_fit <- fit_us_stage(d_k, current_party, config)
    sb_fits[[k]] <- stage_fit
    
    config$msg("Stage ", k, "/", length(parties) - 1, " [", current_party, "] : ", stage_fit$status)
    remaining_parties <- remaining_parties[-1]
  }
  sb_fits
}

predict_us_stage_point <- function(stage_obj, newdata, config) {
  if (is.null(stage_obj$fit)) return(rep(stage_obj$fallback_prob, nrow(newdata)))
  
  eta <- tryCatch(predict_MAVB(stage_obj$fit, newdata = newdata, samples = 1, summary = TRUE, allow_missing_levels = TRUE),
                  error = function(e) { config$msg("Point pred failed: ", conditionMessage(e)); e })
  
  if (inherits(eta, "error")) return(rep(stage_obj$fallback_prob, nrow(newdata)))
  
  if (is.list(eta)) {
    if ("mean" %in% names(eta)) eta <- eta$mean
    else if ("fit" %in% names(eta)) eta <- eta$fit
    else if ("pred" %in% names(eta)) eta <- eta$pred
    else if (length(eta) == 1) eta <- eta[[1]]
    else return(rep(stage_obj$fallback_prob, nrow(newdata)))
  }
  
  pmin(pmax(plogis(as.numeric(eta)), config$eps), 1 - config$eps)
}

predict_us_stage_draws <- function(stage_obj, newdata, config) {
  if (is.null(stage_obj$fit)) return(matrix(stage_obj$fallback_prob, nrow = nrow(newdata), ncol = config$n_sims))
  
  eta_draws <- tryCatch(predict_MAVB(stage_obj$fit, newdata = newdata, samples = config$n_sims, summary = FALSE, allow_missing_levels = TRUE),
                        error = function(e) e)
  
  if (inherits(eta_draws, "error")) return(matrix(stage_obj$fallback_prob, nrow = nrow(newdata), ncol = config$n_sims))
  
  eta_draws <- as.matrix(eta_draws)
  if (nrow(eta_draws) == config$n_sims && ncol(eta_draws) == nrow(newdata)) {
    eta_draws <- t(eta_draws)
  }
  
  pmin(pmax(plogis(eta_draws), config$eps), 1 - config$eps)
}

normalize_us_probability_matrix <- function(prob_mat) {
  n_parties <- ncol(prob_mat)
  prob_mat[prob_mat < 0] <- 0
  row_sums <- rowSums(prob_mat)
  bad_rows <- which(!is.finite(row_sums) | row_sums <= 0)
  if (length(bad_rows) > 0) {
    prob_mat[bad_rows, ] <- 1 / n_parties
    row_sums[bad_rows] <- 1
  }
  prob_mat / row_sums
}

build_us_probability_matrix <- function(pi_components, parties) {
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
  normalize_us_probability_matrix(prob_mat)
}

compute_us_point_estimates <- function(prob_mat, weights) {
  as_tibble(prob_mat) %>%
    mutate(weight = weights) %>%
    summarise(across(-weight, ~ weighted.mean(.x, w = weight, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "party", values_to = "point_estimate") %>%
    arrange(desc(point_estimate))
}

compute_us_share_draws <- function(pi_draws, parties, weights) {
  share_draws <- matrix(NA_real_, nrow = ncol(pi_draws[[1]]), ncol = length(parties))
  colnames(share_draws) <- parties
  for (s in seq_len(ncol(pi_draws[[1]]))) {
    pi_components_s <- lapply(pi_draws, function(draw_mat) draw_mat[, s, drop = FALSE])
    prob_mat_s <- build_us_probability_matrix(pi_components_s, parties)
    share_draws[s, ] <- apply(prob_mat_s, 2, weighted.mean, w = weights, na.rm = TRUE)
  }
  share_draws
}

build_us_extended_frame <- function(prob_mat, frame_pred) {
  as_tibble(prob_mat) %>%
    mutate(cell_id = seq_len(nrow(frame_pred))) %>%
    pivot_longer(cols = -cell_id, names_to = "past_vote", values_to = "prob") %>%
    left_join(frame_pred %>% mutate(cell_id = seq_len(n())), by = "cell_id") %>%
    mutate(expected_N = N * prob) %>%
    select(cell_id, race, gender, age_group, state_abbrv, state_cd, education_level, N, past_vote, prob, expected_N)
}

compute_us_cd_point <- function(prob_mat, frame_pred, parties) {
  valid_rows <- !is.na(frame_pred$state_cd)
  
  as_tibble(prob_mat[valid_rows, , drop = FALSE]) %>%
    mutate(
      state_cd = as.character(frame_pred$state_cd[valid_rows]),
      N = frame_pred$N[valid_rows]
    ) %>%
    pivot_longer(cols = all_of(parties), names_to = "past_vote", values_to = "prob") %>%
    mutate(expected_N = N * prob) %>%
    group_by(state_cd, past_vote) %>%
    summarise(
      expected_N = sum(expected_N, na.rm = TRUE),
      total_N = sum(N, na.rm = TRUE),
      point_estimate = expected_N / total_N,
      .groups = "drop"
    )
}

compute_us_cd_draws <- function(pi_draws, frame_pred, parties) {
  # 1. Locate Congressional District column
  cd_col <- intersect(c("state_cd", "cd", "CD", "district_id", "district"), colnames(frame_pred))[1]
  if (is.na(cd_col)) {
    cd_col <- colnames(frame_pred)[grepl("cd|district", colnames(frame_pred), ignore.case = TRUE)][1]
  }
  if (is.na(cd_col) || !cd_col %in% colnames(frame_pred)) {
    stop("Error: Could not find a Congressional District column in your post-stratification frame.")
  }
  
  # 2. Check if draws exist
  if (length(pi_draws) == 0 || is.null(pi_draws[[1]])) {
    stop("Error: pi_draws is empty. No simulation draws were found to process.")
  }
  
  test_mat <- pi_draws[[1]]
  N_frame <- nrow(frame_pred)
  
  if (nrow(test_mat) == N_frame) {
    n_sims <- ncol(test_mat)
    transposed <- FALSE
  } else if (ncol(test_mat) == N_frame) {
    n_sims <- nrow(test_mat)
    transposed <- TRUE
  } else {
    n_sims <- ncol(test_mat)
    transposed <- FALSE
  }
  
  # CRITICAL LOUD FLASHING LIGHT SAFETY CHECK
  if (is.null(n_sims) || n_sims == 0) {
    stop("Error: The number of simulated draws is 0. Ensure 'n_sims = 100' is explicitly included in your runner's config list.")
  }
  
  valid_rows <- !is.na(frame_pred[[cd_col]])
  
  # 3. Aggregate across simulation slices
  draws_list <- lapply(seq_len(n_sims), function(s) {
    pi_components_s <- lapply(pi_draws, function(draw_mat) {
      if (transposed) {
        return(matrix(draw_mat[s, valid_rows], ncol = 1))
      } else {
        return(matrix(draw_mat[valid_rows, s], ncol = 1))
      }
    })
    
    prob_mat_s <- build_us_probability_matrix(pi_components_s, parties)
    colnames(prob_mat_s) <- parties
    
    # Use as_tibble to prevent R from changing spaces to periods in column names
    prob_df <- as_tibble(prob_mat_s)
    prob_df[[cd_col]] <- as.character(frame_pred[[cd_col]][valid_rows])
    prob_df$N <- frame_pred$N[valid_rows]
    prob_df$draw_id <- s
    
    prob_df %>%
      pivot_longer(cols = all_of(parties), names_to = "past_vote", values_to = "prob") %>%
      mutate(expected_N = N * prob) %>%
      group_by(.data[[cd_col]], past_vote, draw_id) %>%
      summarise(
        draw_share = sum(expected_N, na.rm = TRUE) / sum(N, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  bind_rows(draws_list)
}

run_us_extension <- function(skeleton_frame, auxiliary_survey, area_level_vote_shares, config = list()) {
  config <- resolve_us_extension_config(config)
  metadata_config <- config
  metadata_config$msg <- NULL
  if (!is.null(config$seed)) set.seed(config$seed)
  
  area_shares <- prepare_us_area_shares(area_level_vote_shares, config)
  survey_model <- prepare_us_survey_data(auxiliary_survey, config)
  parties <- levels(survey_model$past_vote)
  frame_pred <- prepare_us_frame_data(skeleton_frame, survey_model, config)
  
  sb_fits <- fit_us_stickbreaking_models(survey_model, parties, area_shares, config)
  
  pi_mat <- matrix(NA_real_, nrow = nrow(frame_pred), ncol = length(parties) - 1)
  colnames(pi_mat) <- parties[seq_len(length(parties) - 1)]
  
  for (k in seq_len(length(parties) - 1)) {
    # Fixed: Added age interaction levels to point calculation block
    stage_newdata <- make_us_stage_data(
      frame_pred, parties[[k]], area_shares, config,
      race_edu_levels = sb_fits[[k]]$race_edu_levels,
      race_gender_levels = sb_fits[[k]]$race_gender_levels,
      gender_edu_levels = sb_fits[[k]]$gender_edu_levels,
      race_age_levels = sb_fits[[k]]$race_age_levels,
      age_edu_levels = sb_fits[[k]]$age_edu_levels,
      age_gender_levels = sb_fits[[k]]$age_gender_levels
    )
    pi_mat[, k] <- predict_us_stage_point(sb_fits[[k]], stage_newdata, config)
  }
  
  pi_components <- lapply(seq_len(ncol(pi_mat)), function(k) pi_mat[, k, drop = FALSE])
  prob_mat <- build_us_probability_matrix(pi_components, parties)
  point_estimates <- compute_us_point_estimates(prob_mat, frame_pred$N)
  
  pi_draws <- vector("list", length(parties) - 1)
  for (k in seq_len(length(parties) - 1)) {
    config$msg("Sim draws for stage ", k, "/", length(parties) - 1, " [", parties[[k]], "]")
    # Fixed: Added age interaction levels to draw simulation block
    stage_newdata <- make_us_stage_data(
      frame_pred, parties[[k]], area_shares, config,
      race_edu_levels = sb_fits[[k]]$race_edu_levels,
      race_gender_levels = sb_fits[[k]]$race_gender_levels,
      gender_edu_levels = sb_fits[[k]]$gender_edu_levels,
      race_age_levels = sb_fits[[k]]$race_age_levels,
      age_edu_levels = sb_fits[[k]]$age_edu_levels,
      age_gender_levels = sb_fits[[k]]$age_gender_levels
    )
    pi_draws[[k]] <- predict_us_stage_draws(sb_fits[[k]], stage_newdata, config)
  }
  
  share_draws <- compute_us_share_draws(pi_draws, parties, frame_pred$N)
  extended_frame <- build_us_extended_frame(prob_mat, frame_pred)
  cd_party_point <- compute_us_cd_point(prob_mat, frame_pred, parties)

  config$msg("Compiling CD-level draws across all simulations...")
  cd_party_draws <- compute_us_cd_draws(pi_draws, frame_pred, parties)
  
  list(
    point_estimates = point_estimates,
    extended_frame = extended_frame,
    share_draws = as_tibble(share_draws),
    cell_party_probabilities = as_tibble(prob_mat),
    cd_party_point = cd_party_point,
    cd_party_draws = cd_party_draws, # <--- CRITICAL FIX: Add this line here!
    metadata = list(parties = parties, config = metadata_config)
  )
}

write_initial_extension_outputs <- function(result, output_dir) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Defensive helper: forces matrices/vectors into data frames on the fly
  ensure_df <- function(obj) {
    if (is.data.frame(obj)) {
      return(obj)
    } else {
      return(as.data.frame(obj))
    }
  }

  message("Writing data frames to disk...")
  write_csv(ensure_df(result$point_estimates), file.path(output_dir, "initial_point_estimates.csv"))
  write_csv(ensure_df(result$extended_frame), file.path(output_dir, "initial_extended_frame.csv"))
  write_csv(ensure_df(result$share_draws), file.path(output_dir, "initial_national_share_draws.csv"))
  write_csv(ensure_df(result$cell_party_probabilities), file.path(output_dir, "initial_cell_party_probabilities.csv"))
  write_csv(ensure_df(result$cd_party_point), file.path(output_dir, "initial_cd_party_point_estimates.csv"))
  write_csv(ensure_df(result$cd_party_draws), file.path(output_dir, "initial_cd_party_draws.csv"))

  message("All files successfully saved!")
  invisible(result)
}



# ==============================================================================
# RUNNER SCRIPT (Adjust these paths to point to your local files)
# ==============================================================================

# 1. Define your file paths
survey_file_path <- "/Users/danjonaitis/Documents/GitHub/voteframe/python_test/data/america/survey.csv"
frame_file_path  <- "/Users/danjonaitis/Documents/GitHub/voteframe/python_test/data/america/stratification_frame.csv"
shares_file_path <- "/Users/danjonaitis/Documents/GitHub/voteframe/python_test/data/america/area_level_vote_shares.csv"
save_directory   <- "/Users/danjonaitis/Documents/GitHub/voteframe/python_test/data/america/output"

# 2. Load the data into your R session
message("Loading datasets...")
user_survey <- read_csv(survey_file_path)
user_frame  <- read_csv(frame_file_path)
user_shares <- read_csv(shares_file_path)

# 3. Execute the full multi-stage stickbreaking pipeline
message("Starting the MRP estimation process...")
mrp_results <- run_us_extension(
  skeleton_frame          = user_frame,
  auxiliary_survey        = user_survey,
  area_level_vote_shares  = user_shares,
  config                  = list(seed = 2026, verbose = TRUE, n_sims = 250)
)

# 4. Write all compiled data frames and draw matrices back to your computer
message("Writing output CSVs to disk...")
write_initial_extension_outputs(mrp_results, output_dir = save_directory)

message("All processes complete! Check your output directory.")