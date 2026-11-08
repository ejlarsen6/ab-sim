outcome_models <- map(outcomes, function(out) {
  df <- pitch_data %>% mutate(target = ifelse(outcome == out, 1, 0))

  glmmTMB(
    target ~ count_state + pitch_type + prev_pitch_type +
      (1 | pitcher),
    data = df,
    family = binomial(link = "logit")
  )
}, .progress = TRUE)

names(outcome_models) <- outcomes

plan(sequential)

pitch_choice_models <- map(common_pitches, function(p_type) {
  df <- pitch_data_filtered |>  mutate(target = ifelse(pitch_type == p_type, 1, 0))

  glmmTMB(
    target ~ count_state + prev_pitch_type + (1 | pitcher),
    data = df,
    family = binomial(link = "logit")
  )
}, .progress = TRUE)

names(pitch_choice_models) <- common_pitches
