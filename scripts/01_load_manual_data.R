# scripts/01_load_manual_data.R

# Purpose:
# Load all manually compiled source files from data/manual.
# This script only reads files, standardizes column names, and checks row counts.

source("scripts/00_setup.R")

# -------------------------------------------------------------------------
# Required manual files
# -------------------------------------------------------------------------

manual_files <- list(
  giannis_stats = "giannis_reg_szn_stats.csv",
  giannis_awards = "giannis_awards_milestones.csv",
  giannis_on_off = "giannis_on_off_since_first_mvp.csv",
  giannis_accomplishments = "giannis_bucks_accomplishments.csv",
  bucks_team_results = "bucks_giannis_era_team_results.csv",
  bucks_playoffs = "bucks_playoff_results.csv",
  transaction_timeline = "bucks_transaction_timeline.csv",
  future_picks = "bucks_future_picks.csv",
  current_roster = "bucks_current_roster.csv",
  young_pieces = "bucks_current_young_pieces.csv",
  draft_prospects = "bucks_incoming_draft_prospects.csv",
  databallr_profiles = "bucks_databallr_player_profiles.csv",
  databallr_playtypes = "bucks_databallr_playtypes.csv"
)

# -------------------------------------------------------------------------
# Check files exist
# -------------------------------------------------------------------------

missing_files <- manual_files[
  !file.exists(file.path(dir_manual, unlist(manual_files)))
]

if (length(missing_files) > 0) {
  stop(
    glue(
      "Missing manual files in data/manual: {paste(unlist(missing_files), collapse = ', ')}"
    )
  )
}

# -------------------------------------------------------------------------
# Load files
# -------------------------------------------------------------------------

giannis_stats_raw <- read_csv(
  file.path(dir_manual, manual_files$giannis_stats),
  show_col_types = FALSE
) |>
  clean_names()

giannis_awards_raw <- read_csv(
  file.path(dir_manual, manual_files$giannis_awards),
  show_col_types = FALSE
) |>
  clean_names()

giannis_on_off_raw <- read_csv(
  file.path(dir_manual, manual_files$giannis_on_off),
  show_col_types = FALSE
) |>
  clean_names()

giannis_accomplishments_raw <- read_csv(
  file.path(dir_manual, manual_files$giannis_accomplishments),
  show_col_types = FALSE
) |>
  clean_names()

bucks_team_results_raw <- read_csv(
  file.path(dir_manual, manual_files$bucks_team_results),
  show_col_types = FALSE
) |>
  clean_names()

bucks_playoffs_raw <- read_csv(
  file.path(dir_manual, manual_files$bucks_playoffs),
  show_col_types = FALSE
) |>
  clean_names()

transaction_timeline_raw <- read_csv(
  file.path(dir_manual, manual_files$transaction_timeline),
  show_col_types = FALSE
) |>
  clean_names()

future_picks_raw <- read_csv(
  file.path(dir_manual, manual_files$future_picks),
  show_col_types = FALSE
) |>
  clean_names()

current_roster_raw <- read_csv(
  file.path(dir_manual, manual_files$current_roster),
  show_col_types = FALSE
) |>
  clean_names()

young_pieces_raw <- read_csv(
  file.path(dir_manual, manual_files$young_pieces),
  show_col_types = FALSE
) |>
  clean_names()

draft_prospects_raw <- read_csv(
  file.path(dir_manual, manual_files$draft_prospects),
  show_col_types = FALSE
) |>
  clean_names()

databallr_profiles_raw <- read_csv(
  file.path(dir_manual, manual_files$databallr_profiles),
  show_col_types = FALSE
) |>
  clean_names()

databallr_playtypes_raw <- read_csv(
  file.path(dir_manual, manual_files$databallr_playtypes),
  show_col_types = FALSE
) |>
  clean_names()

# -------------------------------------------------------------------------
# Load summary
# -------------------------------------------------------------------------

loaded_summary <- tibble(
  dataset = names(manual_files),
  file = unlist(manual_files),
  rows = c(
    nrow(giannis_stats_raw),
    nrow(giannis_awards_raw),
    nrow(giannis_on_off_raw),
    nrow(giannis_accomplishments_raw),
    nrow(bucks_team_results_raw),
    nrow(bucks_playoffs_raw),
    nrow(transaction_timeline_raw),
    nrow(future_picks_raw),
    nrow(current_roster_raw),
    nrow(young_pieces_raw),
    nrow(draft_prospects_raw),
    nrow(databallr_profiles_raw),
    nrow(databallr_playtypes_raw)
  )
)

print(loaded_summary)

# -------------------------------------------------------------------------
# Quick column checks
# -------------------------------------------------------------------------

expected_columns <- list(
  giannis_accomplishments = c("accomplishment", "value", "notes"),
  databallr_profiles = c(
    "player", "pos", "dpm", "o_dpm", "d_dpm",
    "orapm", "drapm", "rapm", "min", "shots", "rts",
    "three_pr", "three_pct", "ftr", "onball"
  ),
  databallr_playtypes = c("player", "playtype", "freq", "rts_impact")
)

check_required_cols <- function(data, required, dataset_name) {
  missing_cols <- setdiff(required, names(data))

  if (length(missing_cols) > 0) {
    warning(
      glue(
        "{dataset_name} is missing expected columns: {paste(missing_cols, collapse = ', ')}"
      )
    )
  }
}

check_required_cols(
  giannis_accomplishments_raw,
  expected_columns$giannis_accomplishments,
  "giannis_accomplishments_raw"
)

check_required_cols(
  databallr_profiles_raw,
  expected_columns$databallr_profiles,
  "databallr_profiles_raw"
)

check_required_cols(
  databallr_playtypes_raw,
  expected_columns$databallr_playtypes,
  "databallr_playtypes_raw"
)

message("Manual data loaded.")
