sample_pitcher_id <- pitch_data |> count(pitcher, sort = TRUE) |> slice(1) |> pull(pitcher) |> as.character()
sample_batter_id  <- pitch_data |> count(batter, sort = TRUE) |> slice(1) |> pull(batter) |> as.character()

cat("Building matchup probability cache...\n")
matchup_cache <- build_matchup_cache(
  sample_pitcher_id, sample_batter_id, 
  pitch_choice_models, models, pitch_data, common_pitches
)

N_SIMS <- 1000
cat("Running", N_SIMS, "simulations using cache...\n")

set.seed(441)
start_time <- Sys.time()
batch_results <- replicate(N_SIMS, simulate_at_bat(matchup_cache, common_pitches))
end_time <- Sys.time()

print(end_time - start_time)
print(table(batch_results) / N_SIMS)

N_SIMS <- 5000
set.seed(441)
sim_results <- replicate(N_SIMS, simulate_at_bat(matchup_cache, common_pitches))

sim_summary <- tibble(Outcome = sim_results) |> 
  count(Outcome) |> 
  mutate(Simulated_Pct = n / N_SIMS)

real_summary <- pitch_data |> 
  filter(pitcher == sample_pitcher_id, batter == sample_batter_id) |> 
  group_by(game_pk, at_bat_number) |> 
  filter(pitch_number == max(pitch_number)) |> 
  ungroup() |> 
  mutate(Macro_Outcome = case_when(
    events %in% c("strikeout", "strikeout_looking") ~ "Strikeout",
    events %in% c("walk", "intent_walk") ~ "Walk",
    events %in% c("single", "double", "triple", "home_run") ~ "Hit",
    events %in% c("field_out", "force_out", "grounded_into_double_play", "sac_fly") ~ "Out",
    TRUE ~ NA_character_
  )) |> 
  filter(!is.na(Macro_Outcome)) |> 
  count(Macro_Outcome) |> 
  mutate(Actual_Pct = n / sum(n))

validation_df <- sim_summary |> 
  left_join(real_summary, by = c("Outcome" = "Macro_Outcome")) |> 
  select(Outcome, Simulated_Pct, Actual_Pct)

print(validation_df)

pitcher_effects_long <- imap_dfr(models, function(mod, outcome_name) {
  re_df <- ranef(mod)[[1]]
  
  tibble(
    pitcher = rownames(re),
    outcome = outcome_name,
    random_intercept = re$`(Intercept)`
  )
})

pitcher_effects_wide <- pitcher_effects_long |> 
  pivot_wider(
    names_from = outcome, 
    values_from = random_intercept
  ) |> 
  mutate(pitcher = as.numeric(pitcher))

pitcher_summary_table <- pitcher_effects_wide |> 
  left_join(players, by = c("pitcher" = "key_mlbam")) |> 
  select(pitcher, name_full, everything())

head(pitcher_summary_table)