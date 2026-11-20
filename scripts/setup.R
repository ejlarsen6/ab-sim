library(tidyverse)
library(lme4)
library(purrr)
library(glmmTMB)
library(furrr)
library(RhpcBLASctl)
library(baseballr)
library(nnet)

sc26 <- read_rds('./data/statcast2026.rds')

pitch_data <-
  sc26 |> 
  filter(!is.na(pitch_type), !is.na(events) | !is.na(description)) |> 
  filter(!pitch_type %in% c("PO", "UN", "EP")) |> 
  filter(effective_speed >= 70) |> 
  arrange(game_pk, at_bat_number, pitch_number) |> 
  group_by(game_pk, at_bat_number) |> 
  mutate(
    count_balls = balls,
    count_strikes = strikes,
    count_state = paste0(count_balls, "-", count_strikes),

    prev_pitch_type = lag(pitch_type, default = "NONE"),
    
    outcome = case_when(
      description == "called_strike" ~ "called_strike",
      description %in% c("swinging_strike", "swinging_strike_blocked") ~ "whiff",
      description == "ball" ~ "ball",
      description == "foul" ~ "foul",
      description == "hit_into_play" ~ "in_play",
      events == "walk" ~ "ball",
      events == "strikeout" ~ "whiff",
      
      TRUE ~ NA_character_
    )
  ) |> 
  ungroup() |> 
  filter(count_balls < 4, count_strikes < 3, !is.na(outcome)) |> 
  mutate(
    outcome = relevel(factor(outcome), ref = 'ball'),
    pitcher = as.factor(pitcher),
    batter = as.factor(batter),
    pitch_type = as.factor(pitch_type),
    prev_pitch_type = as.factor(prev_pitch_type)
  )

outcomes <- c(
  "called_strike", "whiff", "ball", "foul", 
  "single", "double", "triple", "home_run", "field_out"
)

common_pitches <- pitch_data |> 
  count(pitch_type, sort = TRUE) |> 
  filter(n > 5000) |> 
  pull(pitch_type) |> 
  as.character()

valid_pitchers <-
  pitch_data |> 
  group_by(pitcher) |> 
  summarize(n = n(), .groups = "drop") |> 
  filter(n >= 150) |> # Only pitchers with at least ~ 10 innings
  pull(pitcher) |> 
  unique()

pitch_data_filtered <- pitch_data |> 
  filter(pitch_type %in% common_pitches) |> 
  filter(pitcher %in% valid_pitchers) |> 
  mutate(outs_state = outs_when_up,
         first = as.integer(!is.na(on_1b)),
         second = as.integer(!is.na(on_2b)),
         third = as.integer(!is.na(on_3b)),
         ) |> 
  mutate(
    base_state = paste(first,second,third)
  )

players <-
  chadwick_player_lu() |> 
  filter(mlb_played_first >= 2000) |> 
  mutate(
    name_full = paste(name_first, name_last)
  ) |> 
  select(key_mlbam, name_full)