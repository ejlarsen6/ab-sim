library(tidyverse)
library(glmmTMB)

# Global factor universes
count_states <- c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2")

# Subsample data safely
set.seed(2026)
max_rows <- 700000
if (nrow(pitch_data_filtered) > max_rows) {
  message("Subsampling training data to ", max_rows, " rows for memory efficiency...")
  training_subset <- pitch_data_filtered |> sample_n(max_rows)
} else {
  training_subset <- pitch_data_filtered
}

# --- PITCH CHOICE MODELS (Streamlined) ---
pitch_choice_models <- map(common_pitches, function(p_type) {
  df <- training_subset |> 
    mutate(
      target = ifelse(pitch_type == p_type, 1, 0),
      count_state = factor(count_state, levels = count_states)
    )
  
  glmmTMB(
    target ~ count_state + (1 | pitcher),
    data = df,
    family = binomial(link = "logit"),
    control = glmmTMBControl(optCtrl = list(iter.max = 500, eval.max = 500))
  )
}, .progress = TRUE)

names(pitch_choice_models) <- common_pitches

# --- STAGE 1 MODELS (Pitch Outcomes) ---
stage1_outcomes <- c("ball", "called_strike", "whiff", "foul", "in_play")

stage1_models <- map(set_names(stage1_outcomes), function(out_name) {
  df_train <- training_subset |> 
    mutate(
      target = as.integer(outcome == out_name),
      count_state = factor(count_state, levels = count_states),
      pitch_type  = factor(pitch_type, levels = common_pitches)
    )
  
  tryCatch({
    glmmTMB(
      target ~ count_state + pitch_type + (1 | pitcher),
      data = df_train,
      family = binomial(link = "logit"),
      control = glmmTMBControl(optCtrl = list(iter.max = 500, eval.max = 500), profile = TRUE)
    )
  }, error = function(e) {
    warning(sprintf("Stage 1 model failed for %s -> %s", out_name, e$message))
    return(NULL)
  })
}) |> compact()


# --- STAGE 2 MODELS (Batted-Ball Sub-Models) ---
in_play_data <- training_subset |> filter(description == "hit_into_play")
bip_outcomes <- c("single", "double", "triple", "home_run", "field_out")

stage2_models <- map(set_names(bip_outcomes), function(bip_name) {
  df_train <- in_play_data |> 
    mutate(
      target = as.integer(events == bip_name),
      count_state = factor(count_state, levels = count_states),
      pitch_type  = factor(pitch_type, levels = common_pitches)
    )
  
  tryCatch({
    glmmTMB(
      target ~ count_state + pitch_type + (1 | pitcher),
      data = df_train,
      family = binomial(link = "logit"),
      control = glmmTMBControl(optCtrl = list(iter.max = 500, eval.max = 500), profile = TRUE)
    )
  }, error = function(e) {
    warning(sprintf("Stage 2 model failed for %s -> %s", bip_name, e$message))
    return(NULL)
  })
}) |> compact()