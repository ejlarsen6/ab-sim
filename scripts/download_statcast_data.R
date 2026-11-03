devtools::install_github(repo = "saberpowers/sabRmetrics")
devtools::install_github(repo = "BillPetti/baseballr")

library(baseballr)
library(dplyr)
library(purrr)
library(furrr)

# Process Data in Week-long Chunks
season_start <- as.Date("2026-03-26")
season_end   <- as.Date("2026-10-04")

date_chunks <- tibble(
  start = seq(season_start, season_end, by = "7 days")
) |> 
  mutate(
    end = start + 6,
    end = if_else(end > season_end, season_end, end)
  )

download_week <- function(start_dt, end_dt) {
  message(paste("Fetching pitches from", start_dt, "to", end_dt, "..."))
  
  tryCatch({
    data <- statcast_search(
      start_date = as.character(start_dt),
      end_date = as.character(end_dt),
      player_type = "pitcher" # pitches thrown by pitchers
    )
    
    # Return data frame if valid rows are found
    if (!is.null(data) && nrow(data) > 0) {
      return(data)
    } else {
      return(NULL)
    }
  }, error = function(e) {
    message(paste("Error or timeout on dates:", start_dt, "to", end_dt))
    return(NULL)
  })
}


plan(multisession, workers = 4)

full_season_pbp <- future_map2_dfr(date_chunks$start, date_chunks$end, download_week)

glimpse(full_season_pbp)

# Saves full season RDS data to data/ directory
saveRDS(full_season_pbp, "../data/statcast2026.rds")

