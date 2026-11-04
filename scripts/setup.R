library(tidyverse)
library(lme4)
library(purrr)
library(glmmTMB)
library(furrr)
library(RhpcBLASctl)
library(baseballr)

RhpcBLASctl::blas_set_num_threads(1)
Sys.setenv(OMP_NUM_THREADS = 1)

available_cores <- availableCores() - 1
plan(multisession, workers = 2)

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
      description %in% c("called_strike") ~ "called_strike",
      description %in% c("swinging_strike", "foul_tip", "swinging_strike_blocked") ~ "whiff",
      description %in% c("ball", "blocked_ball") ~ "ball",
      description %in% c("foul") ~ "foul",
      events %in% c("single", "double", "triple", "home_run") ~ "hit",
      events %in% c("field_out", "force_out", "grounded_into_double_play", "sac_fly") ~ "in_play_out",
      TRUE ~ "foul"
    )
  ) |> 
  ungroup() |> 
  filter(count_balls < 4, count_strikes < 3) |> 
  mutate(
    outcome = relevel(factor(outcome), ref = 'ball'),
    pitcher = as.factor(pitcher),
    batter = as.factor(batter),
    pitch_type = as.factor(pitch_type),
    prev_pitch_type = as.factor(prev_pitch_type)
  )

outcomes <- c("called_strike", "whiff", "ball", "foul", "hit", "in_play_out")

common_pitches <- pitch_data |> 
  count(pitch_type, sort = TRUE) |> 
  filter(n > 5000) |> 
  pull(pitch_type) |> 
  as.character()

pitch_data_filtered <- pitch_data |> 
  filter(pitch_type %in% common_pitches)