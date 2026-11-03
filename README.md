# Baseball At-Bat Simulation Engine

A high-performance Monte Carlo simulation engine built in R that uses pitch-by-pitch Statcast data to analyze pitcher tendencies, sequencing strategies, and macro at-bat outcomes ($K\%$, $BB\%$, $\text{Hit}\%$, $\text{Out}\%$).

---

## Project Overview

This project simulates baseball at-bats at the pitch level using a two-stage stochastic decision framework:
1. **Pitch Selection:** Models what pitch a specific pitcher chooses based on the complete game state.
2. **Pitch Outcome:** Models the result of that pitch (called strike, whiff, ball, foul, hit, or in-play out).

To achieve high-speed Monte Carlo simulations without computational bottlenecks, the engine precomputes state transition probabilities into a matchup lookup cache before executing batch simulations.

---

## Repository Structure

```text
baseball-simulation-engine/
│
├── data/                      # Local data directory (statcast2026.rds)
├── scripts/                   # Modular R pipeline
│   ├── setup.R             # Environment setup, data cleaning, state engineering
│   ├── models.R            # glmmTMB 1-vs-rest model fitting
│   ├── functions.R         # Precomputed cache builder & simulation logic
│   ├── analysis.R          # Execution, batch runs, and outcome analysis
|   └── download_data.R     # Downloads 2026 regular season Statcast data
│
├── outputs/                   # Generated plots, heatmaps, and summaries
├── run_pipeline.R             # Master execution script
├── .gitignore                 # Excludes large data files and R session caches
└── README.md                  # Project documentation
```

---

## Model Architecture & Features

* **Complete Game-State Context:** Tracks count states (`0-0` to `3-2`), out states (`0-2`), baserunner configurations (`000` to `111`), and previous pitch types for tunneling/sequencing.
* **Pitcher Random Effects:** Uses `glmmTMB` with `(1 | pitcher)` random intercepts to capture pitcher-specific baselines while training on league-wide data.
* **1-vs-Rest Binary Classifier Architecture:** Avoids dynamic multinomial complexity by evaluating separate binary logistic regressions normalized into a valid probability distribution.
* **Precomputed State Cache:** Maps all possible pitch and outcome scenarios upfront to allow thousands of at-bat simulations in seconds.

---

## Getting Started

### Prerequisites

Ensure you have R and the following packages installed:
```R
install.packages(c("tidyverse", "lme4", "purrr", "glmmTMB", "furrr", "RhpcBLASctl"))
```

### Running the Engine

1. Place `statcast2026.rds` into the `data/` directory.
2. Execute the full pipeline via the master script:
```R
source("run_pipeline.R")
```
