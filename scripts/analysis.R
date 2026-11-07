N_SIMS = 1000

sample_pitcher_id <- get_player_id("Aaron Ashby")
cubs_lineup <- get_team_lineup("CHC", pitch_data)


cat("Building matchup probability cache...\n")
matchup_cache <- build_matchup_cache(
  sample_pitcher_id, 
  pitch_choice_models, outcome_models, pitch_data, common_pitches
)

pitcher_effects_long <- imap_dfr(outcome_models, function(mod, outcome_name) {
  re_df <- ranef(mod)[[1]]
  
  re_df |> 
    as.data.frame() |> 
    rownames_to_column(var = "pitcher") |> 
    mutate(
      outcome = outcome_name,
      random_intercept = `X.Intercept.`
    ) |> 
    select(pitcher, outcome, random_intercept)
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

head(pitcher_summary_table |> filter(!is.na(name_full)))

head(pitcher_summary_table)

cat("Building matchup cache for pitcher:", sample_pitcher_id, "...\n")
pitcher_cache <- build_matchup_cache(
  pitcher_id = sample_pitcher_id,
  pitch_choice_models = pitch_choice_models,
  outcome_models = outcome_models,
  ref_data = pitch_data,
  common_pitches = common_pitches
)

# Run 1,000 half-inning simulations
set.seed(2026)
cat("Running", N_SIMS, "half-inning simulations...\n")

simulation_results <- map_dbl(1:N_SIMS, function(i) {
  if (i %% 100 == 0) cat("Simulation batch:", i, "/", N_SIMS, "\n")
  
  simulate_half_inning(
    pitcher_cache = pitcher_cache,
    common_pitches = common_pitches,
    lineup_batter_ids = cubs_lineup
  )
})

# Calculate Summary Statistics
inning_summary <- tibble(runs = simulation_results) |> 
  summarise(
    mean_runs         = mean(runs),
    median_runs       = median(runs),
    sd_runs           = sd(runs),
    prob_score_0      = mean(runs == 0),
    prob_score_1      = mean(runs == 1),
    prob_score_2_plus = mean(runs >= 2),
    max_runs_recorded = max(runs)
  )

print(inning_summary)
