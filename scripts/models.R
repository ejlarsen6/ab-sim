pitch_choice_models <- map(common_pitches, function(p_type) {
  df <- pitch_data_filtered |>  mutate(target = ifelse(pitch_type == p_type, 1, 0))

  glmmTMB(
    target ~ count_state + prev_pitch_type + (1 | pitcher),
    data = df,
    family = binomial(link = "logit")
  )
}, .progress = TRUE)

names(pitch_choice_models) <- common_pitches

# Macro outcomes
stage1_outcomes <- c("ball", "called_strike", "whiff", "foul", "in_play")

stage1_models <- map(set_names(stage1_outcomes), function(out_name) {
  df_train <- pitch_data |> 
    mutate(target = as.integer(stage1_outcome == out_name))
  
  glmmTMB(
    target ~ count_state + outs_state + base_state + pitch_type + prev_pitch_type + (1 | pitcher),
    data = df_train,
    family = binomial(link = "logit")
  )
})

names(stage1_models) <- stage1_outcomes

# Batted Ball Outcomes (in-play only)
in_play_data <- pitch_data_mutated |> filter(stage1_outcome == "in_play")
bip_outcomes <- c("single", "double", "triple", "home_run", "field_out")

stage2_models <- map(set_names(bip_outcomes), function(bip_name) {
  df_train <- in_play_data |> 
    mutate(target = as.integer(bip_outcome == bip_name))
  
  glmmTMB(
    target ~ count_state + outs_state + base_state + pitch_type + prev_pitch_type + (1 | pitcher),
    data = df_train,
    family = binomial(link = "logit")
  )
})

names(stage2_models) <- stage2_outcomes