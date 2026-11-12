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
  df_train <- pitch_data_filtered |> 
    mutate(target = as.integer(outcome == out_name))
  
  if (length(unique(df_train$target)) < 2) {
    warning(paste("Skipping Stage 1 model for:", out_name, "- target has only one unique value."))
    return(NULL)
  }
  
  tryCatch({
    glmmTMB(
      target ~ count_state + outs_state + base_state + pitch_type + prev_pitch_type + (1 | pitcher),
      data = df_train,
      family = binomial(link = "logit"),
      control = glmmTMBControl(optCtrl = list(iter.max = 1000, eval.max = 1000))
    )
  }, error = function(e) {
    warning(paste("Model fitting failed for Stage 1:", out_name, "->", e$message))
    return(NULL)
  })
}) |> compact() # Filter out NULLs

# Batted Ball Outcomes (in-play only)
in_play_data <- pitch_data_filtered |> filter(outcome == "in_play")
bip_outcomes <- c("single", "double", "triple", "home_run", "field_out")

stage2_models <- map(set_names(bip_outcomes), function(bip_name) {
  df_train <- in_play_data |> 
    mutate(target = as.integer(description == bip_name))
  
  if (length(unique(df_train$target)) < 2) {
    warning(paste("Skipping Stage 2 model for:", bip_name, "- target has only one unique value in-play."))
    return(NULL)
  }
  
  tryCatch({
    glmmTMB(
      target ~ count_state + outs_state + base_state + pitch_type + prev_pitch_type + (1 | pitcher),
      data = df_train,
      family = binomial(link = "logit"),
      control = glmmTMBControl(optCtrl = list(iter.max = 1000, eval.max = 1000))
    )
  }, error = function(e) {
    warning(paste("Model fitting failed for Stage 2:", bip_name, "->", e$message))
    return(NULL)
  })
}) |> compact() # Filter out NULLs

