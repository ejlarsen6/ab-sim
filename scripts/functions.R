build_matchup_cache <- function(pitcher_id, batter_id, pitch_choice_models, outcome_models, ref_data, common_pitches) {
  
  counts <- c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2")
  prev_pitches <- c("NONE", common_pitches)
  
  pitch_grid <- expand.grid(
    count_state = counts,
    prev_pitch_type = prev_pitches,
    stringsAsFactors = FALSE
  ) |> mutate(pitcher = factor(pitcher_id, levels = levels(ref_data$pitcher)))
  
  pitch_preds <- map_dfc(names(pitch_choice_models), function(p_name) {
    mod <- pitch_choice_models[[p_name]]
    p_vals <- tryCatch({
      predict(mod, newdata = pitch_grid, type = "response", allow.new.levels = TRUE)
    }, error = function(e) {
      predict(mod, newdata = pitch_grid, type = "response", re.form = NA)
    })
    tibble(!!p_name := as.numeric(p_vals))
  })
  
  pitch_cache <- bind_cols(pitch_grid, pitch_preds)
  
  outcome_grid <- expand.grid(
    count_state = counts,
    pitch_type = common_pitches,
    prev_pitch_type = prev_pitches,
    stringsAsFactors = FALSE
  ) |> mutate(
    pitcher = factor(pitcher_id, levels = levels(ref_data$pitcher)),
    batter = factor(batter_id, levels = levels(ref_data$batter))
  )
  
  outcome_preds <- map_dfc(names(outcome_models), function(out_name) {
    mod <- outcome_models[[out_name]]
    o_vals <- tryCatch({
      predict(mod, newdata = outcome_grid, type = "response", allow.new.levels = TRUE)
    }, error = function(e) {
      predict(mod, newdata = outcome_grid, type = "response", re.form = NA)
    })
    tibble(!!out_name := as.numeric(o_vals))
  })
  
  outcome_cache <- bind_cols(outcome_grid, outcome_preds)
  
  list(pitch = pitch_cache, outcome = outcome_cache)
}

simulate_at_bat <- function(cache, common_pitches) {
  balls <- 0
  strikes <- 0
  prev_pitch <- "NONE"
  
  while (TRUE) {
    count_str <- paste0(balls, "-", strikes)
    
    row_match <- cache$pitch |> 
      filter(count_state == count_str, prev_pitch_type == prev_pitch)
    
    pitcher_arsenal <- intersect(common_pitches, colnames(cache$pitch))
    
    raw_pitch_probs <- as.numeric(row_match[1, pitcher_arsenal])
    if (any(is.na(raw_pitch_probs)) || sum(raw_pitch_probs) == 0) {
      pitch_probs <- rep(1 / length(pitcher_arsenal), length(pitcher_arsenal))
    } else {
      pitch_probs <- raw_pitch_probs / sum(raw_pitch_probs)
    }
    names(pitch_probs) <- pitcher_arsenal
    
    curr_pitch <- sample(pitcher_arsenal, size = 1, prob = pitch_probs)
  
    out_match <- cache$outcome |> 
      filter(count_state == count_str, pitch_type == curr_pitch, prev_pitch_type == prev_pitch)
    
    outcomes_list <- c("called_strike", "whiff", "ball", "foul", "hit", "in_play_out")
    raw_outcome_probs <- as.numeric(out_match[1, outcomes_list])
    
    outcome_probs <- raw_outcome_probs / sum(raw_outcome_probs)
    names(outcome_probs) <- outcomes_list
    
    outcome_sampled <- sample(outcomes_list, size = 1, prob = outcome_probs)
    
    if (outcome_sampled == "ball") {
      balls <- balls + 1
      if (balls == 4) return("Walk")
    } else if (outcome_sampled %in% c("called_strike", "whiff")) {
      strikes <- strikes + 1
      if (strikes == 3) return("Strikeout")
    } else if (outcome_sampled == "foul") {
      if (strikes < 2) strikes <- strikes + 1
    } else if (outcome_sampled == "hit") {
      return("Hit")
    } else if (outcome_sampled == "in_play_out") {
      return("Out")
    }
    prev_pitch <- curr_pitch
    cat("Pitch thrown:", curr_pitch, "| Result:", outcome_sampled, "| Count:", count_str, "\n")
  }
}