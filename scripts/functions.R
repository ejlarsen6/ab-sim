predict_normalized <- function(models_list, newdata) {
  # Predict raw probabilities across all models in the list
  raw_preds <- map_dfc(names(models_list), function(name) {
    mod <- models_list[[name]]
    p_vals <- tryCatch({
      predict(mod, newdata = newdata, type = "response", allow.new.levels = TRUE)
    }, error = function(e) {
      predict(mod, newdata = newdata, type = "response", re.form = NA)
    })
    tibble(!!name := as.numeric(p_vals))
  })
  
  # Normalize row-wise so probabilities sum to 1.0
  row_sums <- rowSums(raw_preds)
  row_sums[row_sums == 0] <- 1 # Prevent division by zero
  
  norm_preds <- raw_preds |> 
    mutate(across(everything(), ~ .x / row_sums))
  
  return(norm_preds)
}

build_matchup_cache <- function(pitcher_id, pitch_choice_models, stage1_models, stage2_models, ref_data, common_pitches) {
  
  pitcher_pitches <- ref_data |> 
    filter(pitcher == pitcher_id) |> 
    pull(pitch_type) |> 
    unique()
  
  valid_pitches <- intersect(pitcher_pitches, common_pitches)
  if (length(valid_pitches) == 0) valid_pitches <- common_pitches
  
  # Pitch Choice Grid
  pitch_grid <- expand_grid(
    count_state = count_states,
    outs_state = outs_states,
    base_state = base_states,
    prev_pitch_type = c("NONE", valid_pitches),
    pitcher = pitcher_id
  ) |> 
    mutate(
      count_state = factor(count_state, levels = count_states),
      outs_state = factor(outs_state, levels = outs_states),
      base_state = factor(base_state, levels = base_states),
      prev_pitch_type = factor(prev_pitch_type, levels = c("NONE", common_pitches)),
      pitcher = factor(pitcher)
    )
  
  raw_pitch_preds <- map_dfc(names(pitch_choice_models), function(mod_name) {
    mod <- pitch_choice_models[[mod_name]]
    preds <- tryCatch({
      predict(mod, newdata = pitch_grid, type = "response", allow.new.levels = TRUE)
    }, error = function(e) {
      predict(mod, newdata = pitch_grid, type = "response", re.form = NA)
    })
    tibble(!!mod_name := as.numeric(preds))
  })
  
  pitch_row_sums <- rowSums(raw_pitch_preds)
  pitch_row_sums[pitch_row_sums == 0] <- 1
  norm_pitch_preds <- raw_pitch_preds |> mutate(across(everything(), ~ .x / pitch_row_sums))
  pitch_cache <- bind_cols(pitch_grid, norm_pitch_preds)
  
  # Stages 1 & 2 Outcome Grid
  stage1_grid <- expand_grid(
    count_state = count_states,
    pitch_type = valid_pitches,
    prev_pitch_type = c("NONE", valid_pitches),
    pitcher = pitcher_id
  ) |> 
    mutate(
      count_state = factor(count_state, levels = count_states),
      pitch_type = factor(pitch_type, levels = common_pitches),
      prev_pitch_type = factor(prev_pitch_type, levels = c("NONE", common_pitches)),
      pitcher = factor(pitcher)
    )
  
  raw_s1 <- map_dfc(names(stage1_models), function(m) {
    preds <- predict(stage1_models[[m]], newdata = stage1_grid, type = "response", allow.new.levels = TRUE)
    tibble(!!m := as.numeric(preds))
  })
  s1_sums <- rowSums(raw_s1); s1_sums[s1_sums == 0] <- 1
  stage1_cache <- bind_cols(stage1_grid, raw_s1 |> mutate(across(everything(), ~ .x / s1_sums)))
  
  raw_s2 <- map_dfc(names(stage2_models), function(m) {
    preds <- predict(stage2_models[[m]], newdata = stage1_grid, type = "response", allow.new.levels = TRUE)
    tibble(!!m := as.numeric(preds))
  })
  s2_sums <- rowSums(raw_s2); s2_sums[s2_sums == 0] <- 1
  stage2_cache <- bind_cols(stage1_grid, raw_s2 |> mutate(across(everything(), ~ .x / s2_sums)))
  
  list(
    pitch = pitch_cache,
    stage1 = stage1_cache,
    stage2 = stage2_cache
  )
}

simulate_at_bat <- function(cache, common_pitches, batter_id) {
  balls <- 0
  strikes <- 0
  prev_pitch <- "NONE"
  pitch_sequence <- list()
  pitch_num <- 1
  
  # Available pitch names present in the pitch cache
  cache_pitch_cols <- colnames(cache$pitch)[colnames(cache$pitch) %in% common_pitches]
  if (length(cache_pitch_cols) == 0) cache_pitch_cols <- common_pitches
  
  stage1_list <- c("ball", "called_strike", "whiff", "foul", "in_play")
  stage2_list <- c("single", "double", "triple", "home_run", "field_out")
  
  while (TRUE) {
    count_str <- paste0(balls, "-", strikes)
    
    # Sample Pitch Choice
    row_match <- cache$pitch |> 
      filter(count_state == count_str, prev_pitch_type == prev_pitch)
    
    valid_cols <- intersect(cache_pitch_cols, colnames(row_match))
    
    if (nrow(row_match) > 0 && length(valid_cols) > 0) {
      raw_pitch_probs <- as.numeric(row_match[1, valid_cols])
      names(raw_pitch_probs) <- valid_cols
    } else {
      raw_pitch_probs <- numeric(0)
    }
    
    if (length(raw_pitch_probs) == 0 || any(is.na(raw_pitch_probs)) || sum(raw_pitch_probs) == 0) {
      pitch_probs <- rep(1 / length(common_pitches), length(common_pitches))
      names(pitch_probs) <- common_pitches
    } else {
      pitch_probs <- raw_pitch_probs / sum(raw_pitch_probs)
    }
    
    curr_pitch <- sample(names(pitch_probs), size = 1, prob = pitch_probs)
    
    # Pitch Result
    s1_match <- cache$stage1 |> 
      filter(count_state == count_str, pitch_type == curr_pitch, prev_pitch_type == prev_pitch)
    
    valid_s1_cols <- intersect(stage1_list, colnames(s1_match))
    
    if (nrow(s1_match) > 0 && length(valid_s1_cols) > 0) {
      raw_s1_probs <- as.numeric(s1_match[1, valid_s1_cols])
      names(raw_s1_probs) <- valid_s1_cols
    } else {
      raw_s1_probs <- numeric(0)
    }
    
    if (length(raw_s1_probs) == 0 || any(is.na(raw_s1_probs)) || sum(raw_s1_probs) == 0) {
      s1_probs <- rep(1 / length(stage1_list), length(stage1_list))
      names(s1_probs) <- stage1_list
    } else {
      s1_probs <- raw_s1_probs / sum(raw_s1_probs)
    }
    
    s1_sampled <- sample(names(s1_probs), size = 1, prob = s1_probs)
    
    count_before <- count_str
    terminal_event <- NULL
    bip_sampled <- NA
    
    if (s1_sampled == "ball") {
      balls <- balls + 1
      if (balls == 4) terminal_event <- "Walk"
    } else if (s1_sampled %in% c("called_strike", "whiff")) {
      strikes <- strikes + 1
      if (strikes == 3) terminal_event <- "Strikeout"
    } else if (s1_sampled == "foul") {
      if (strikes < 2) strikes <- strikes + 1
    } else if (s1_sampled == "in_play") {
      # Stage 2: Batted-Ball Sub-Model
      s2_match <- cache$stage2 |> 
        filter(count_state == count_str, pitch_type == curr_pitch, prev_pitch_type == prev_pitch)
      
      valid_s2_cols <- intersect(stage2_list, colnames(s2_match))
      
      if (nrow(s2_match) > 0 && length(valid_s2_cols) > 0) {
        raw_s2_probs <- as.numeric(s2_match[1, valid_s2_cols])
        names(raw_s2_probs) <- valid_s2_cols
      } else {
        raw_s2_probs <- numeric(0)
      }
      
      if (length(raw_s2_probs) == 0 || any(is.na(raw_s2_probs)) || sum(raw_s2_probs) == 0) {
        s2_probs <- rep(1 / length(stage2_list), length(stage2_list))
        names(s2_probs) <- stage2_list
      } else {
        s2_probs <- raw_s2_probs / sum(raw_s2_probs)
      }
      
      bip_sampled <- sample(names(s2_probs), size = 1, prob = s2_probs)
      
      if (bip_sampled == "single") {
        terminal_event <- "Single"
      } else if (bip_sampled == "double") {
        terminal_event <- "Double"
      } else if (bip_sampled == "triple") {
        terminal_event <- "Triple"
      } else if (bip_sampled == "home_run") {
        terminal_event <- "Home_Run"
      } else if (bip_sampled == "field_out") {
        terminal_event <- "Field_Out"
      }
    }
    
    pitch_sequence[[pitch_num]] <- tibble(
      batter_id    = batter_id,
      pitch_num    = pitch_num,
      prev_pitch   = prev_pitch,
      pitch_type   = curr_pitch,
      count_before = count_before,
      outcome      = s1_sampled,
      bip_outcome  = ifelse(is.na(bip_sampled), "none", bip_sampled),
      pa_result    = ifelse(is.null(terminal_event), "In_Progress", terminal_event)
    )
    
    if (!is.null(terminal_event)) {
      return(list(
        event = terminal_event,
        log   = bind_rows(pitch_sequence)
      ))
    }
    
    prev_pitch <- curr_pitch
    pitch_num <- pitch_num + 1
  }
}

simulate_half_inning <- function(pitcher_cache, common_pitches, lineup_batter_ids = 1:9) {
  outs <- 0
  runs <- 0
  bases <- c(0, 0, 0)
  batter_idx <- 1
  pa_counter <- 1
  inning_logs <- list()
  
  while (outs < 3) {
    current_batter_id <- lineup_batter_ids[((batter_idx - 1) %% length(lineup_batter_ids)) + 1]
    
    ab_result <- simulate_at_bat(pitcher_cache, common_pitches, current_batter_id)
    event <- ab_result$event
    
    pa_log <- ab_result$log |> mutate(pa_number = pa_counter, outs_before = outs)
    inning_logs[[pa_counter]] <- pa_log
    
    # Base running & scoring logic
    if (event == "Walk") {
      if (bases[1] == 0) bases[1] <- 1
      else if (bases[2] == 0) bases[2] <- 1
      else if (bases[3] == 0) bases[3] <- 1
      else runs <- runs + 1
    } else if (event == "Single") {
      runs <- runs + bases[3]; bases[3] <- bases[2]; bases[2] <- bases[1]; bases[1] <- 1
    } else if (event == "Double") {
      runs <- runs + bases[3] + bases[2]; bases[3] <- bases[1]; bases[2] <- 1; bases[1] <- 0
    } else if (event == "Triple") {
      runs <- runs + sum(bases); bases[3] <- 1; bases[2] <- 0; bases[1] <- 0
    } else if (event == "Home_Run") {
      runs <- runs + sum(bases) + 1; bases <- c(0, 0, 0)
    } else if (event %in% c("Strikeout", "Field_Out")) {
      outs <- outs + 1
    }
    
    batter_idx <- batter_idx + 1
    pa_counter <- pa_counter + 1
  }
  
  list(
    runs = runs,
    log  = bind_rows(inning_logs)
  )
}

get_player_id <- function(full_name) {
  id <-
    players |> 
    filter(name_full == full_name) |> 
    pull(key_mlbam)
  return(id)
}

get_team_lineup <- function(team_abbr, ref_data, n = 9) {
  # Filter rows where the specified team was actively batting 
  team_batting_data <- ref_data |> 
    filter(
      (home_team == team_abbr & inning_topbot == "Bot") | 
        (away_team == team_abbr & inning_topbot == "Top")
    )
  
  if (nrow(team_batting_data) == 0) {
    team_batting_data <- ref_data |> 
      filter(home_team == team_abbr | away_team == team_abbr)
  }
  
  # Count occurrences per batter to isolate the team's regular 9 starters
  lineup_ids <- team_batting_data |> 
    group_by(batter) |> 
    summarise(pa_count = n(), .groups = "drop") |> 
    arrange(desc(pa_count)) |> 
    head(n) |> 
    pull(batter)
  
  if (length(lineup_ids) < n) {
    warning(paste("Warning: Only found", length(lineup_ids), "unique batters for team abbreviation:", team_abbr))
  }
  
  return(lineup_ids)
}

