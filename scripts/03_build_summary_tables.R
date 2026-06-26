# scripts/03_build_summary_tables.R

# Purpose:
# Build polished summary tables for the report and save them to outputs/tables.

source("scripts/02_clean_manual_data.R")

# -------------------------------------------------------------------------
# 1. Giannis awards / milestones table
# -------------------------------------------------------------------------

giannis_awards_table <- giannis_awards_clean |>
  select(season, event_type, event, notes) |>
  gt() |>
  tab_header(
    title = "Giannis Era Milestones",
    subtitle = "Key awards, transactions, and franchise moments"
  ) |>
  cols_label(
    season = "Season",
    event_type = "Type",
    event = "Event",
    notes = "Notes"
  ) |>
  gt_bucks_theme()

gtsave(giannis_awards_table, file.path(dir_tables, "giannis_awards_table.html"))

# -------------------------------------------------------------------------
# 2. Bucks team results table
# -------------------------------------------------------------------------

team_results_table <- bucks_team_results_clean |>
  select(
    season, wins, losses, win_pct, seed, playoff_result,
    ortg, drtg, net, srs, pace
  ) |>
  gt() |>
  tab_header(
    title = "Bucks Year-by-Year Team Results",
    subtitle = "Regular-season record, ratings, and playoff finish during the Giannis era"
  ) |>
  cols_label(
    season = "Season",
    wins = "W",
    losses = "L",
    win_pct = "Win%",
    seed = "Seed",
    playoff_result = "Playoff Result",
    ortg = "ORTG",
    drtg = "DRTG",
    net = "Net",
    srs = "SRS",
    pace = "Pace"
  ) |>
  fmt_percent(columns = win_pct, decimals = 1) |>
  fmt_number(columns = c(ortg, drtg, net, srs, pace), decimals = 1) |>
  gt_bucks_theme()

gtsave(team_results_table, file.path(dir_tables, "team_results_table.html"))

# -------------------------------------------------------------------------
# 3. Transaction timeline table
# -------------------------------------------------------------------------

transaction_summary_table <- transaction_timeline_clean |>
  transmute(
    date,
    season,
    move = transaction_type,
    outgoing = players_out,
    incoming = players_in,
    picks_out,
    picks_in,
    notes
  ) |>
  gt() |>
  tab_header(
    title = "Milwaukee's Window-Shaping Moves",
    subtitle = "Major draft, trade, and roster decisions from Giannis' arrival through the reset"
  ) |>
  cols_label(
    date = "Date",
    season = "Season",
    move = "Move",
    outgoing = "Players Out",
    incoming = "Players In",
    picks_out = "Picks Out",
    picks_in = "Picks In",
    notes = "Notes"
  ) |>
  fmt_date(columns = date, date_style = "yMMMd") |>
  sub_missing(columns = everything(), missing_text = "") |>
  gt_bucks_theme()

gtsave(transaction_summary_table, file.path(dir_tables, "transaction_summary_table.html"))

# -------------------------------------------------------------------------
# 4. Future picks table
# -------------------------------------------------------------------------

future_picks_table <- future_picks_clean |>
  select(
    year, round, status, bucks_pick_result,
    outgoing_to, incoming_from, report_label
  ) |>
  gt(groupname_col = "year") |>
  tab_header(
    title = "Future Pick Control",
    subtitle = "The reset added Miami upside, but prior obligations still limit clean control"
  ) |>
  cols_label(
    round = "Round",
    status = "Status",
    bucks_pick_result = "Bucks Pick Result",
    outgoing_to = "Outgoing To",
    incoming_from = "Incoming From",
    report_label = "Report Label"
  ) |>
  sub_missing(columns = everything(), missing_text = "") |>
  gt_bucks_theme()

gtsave(future_picks_table, file.path(dir_tables, "future_picks_table.html"))

# -------------------------------------------------------------------------
# 5. Current roster table
# -------------------------------------------------------------------------

current_roster_table <- current_roster_clean |>
  select(player, pos, age, height, weight, salary_raw, roster_bucket) |>
  gt() |>
  tab_header(
    title = "Current Bucks Roster Snapshot",
    subtitle = "Age, size, salary, and broad roster bucket"
  ) |>
  cols_label(
    player = "Player",
    pos = "Pos",
    age = "Age",
    height = "Height",
    weight = "Weight",
    salary_raw = "Salary",
    roster_bucket = "Roster Bucket"
  ) |>
  fmt_number(columns = age, decimals = 1) |>
  sub_missing(columns = everything(), missing_text = "") |>
  gt_bucks_theme()

gtsave(current_roster_table, file.path(dir_tables, "current_roster_table.html"))

# -------------------------------------------------------------------------
# 6. Young NBA pieces table
# -------------------------------------------------------------------------

young_pieces_table <- young_pieces_clean |>
  select(player, player_type, ppg, rpg, apg, ts_pct, usg_pct, mpg) |>
  gt() |>
  tab_header(
    title = "Young NBA Pieces",
    subtitle = "Recent production for Milwaukee's young and bridge pieces"
  ) |>
  cols_label(
    player = "Player",
    player_type = "Role Label",
    ppg = "PPG",
    rpg = "RPG",
    apg = "APG",
    ts_pct = "TS%",
    usg_pct = "USG%",
    mpg = "MPG"
  ) |>
  fmt_number(columns = c(ppg, rpg, apg, mpg), decimals = 1) |>
  fmt_percent(columns = c(ts_pct, usg_pct), decimals = 1) |>
  gt_bucks_theme()

gtsave(young_pieces_table, file.path(dir_tables, "young_pieces_table.html"))

# -------------------------------------------------------------------------
# 7. Draft prospects table
# -------------------------------------------------------------------------

draft_prospects_table <- draft_prospects_clean |>
  select(
    player, prospect_type, ppg, rpg, apg, tov,
    two_fg_pct, three_fg_pct, ft_pct, mock_pos
  ) |>
  gt() |>
  tab_header(
    title = "Incoming Draft Prospects",
    subtitle = "Manual prospect production snapshot"
  ) |>
  cols_label(
    player = "Player",
    prospect_type = "Prospect Type",
    ppg = "PPG",
    rpg = "RPG",
    apg = "APG",
    tov = "TOV",
    two_fg_pct = "2FG%",
    three_fg_pct = "3FG%",
    ft_pct = "FT%",
    mock_pos = "Mock Position"
  ) |>
  fmt_number(columns = c(ppg, rpg, apg, tov), decimals = 1) |>
  fmt_percent(columns = c(two_fg_pct, three_fg_pct, ft_pct), decimals = 1) |>
  gt_bucks_theme()

gtsave(draft_prospects_table, file.path(dir_tables, "draft_prospects_table.html"))

# -------------------------------------------------------------------------
# 8. Giannis on/off table
# -------------------------------------------------------------------------

giannis_on_off_table <- giannis_on_off_clean |>
  select(
    status, minutes, ortg, drtg, net,
    two_fg_pct, three_fg_pct, opp_two_fg_pct, opp_three_fg_pct
  ) |>
  gt() |>
  tab_header(
    title = "Giannis On/Off Since First MVP",
    subtitle = "Milwaukee's minutes with and without Giannis after he became the franchise's MVP-level anchor"
  ) |>
  cols_label(
    status = "Status",
    minutes = "Minutes",
    ortg = "ORTG",
    drtg = "DRTG",
    net = "Net",
    two_fg_pct = "2FG%",
    three_fg_pct = "3FG%",
    opp_two_fg_pct = "Opp 2FG%",
    opp_three_fg_pct = "Opp 3FG%"
  ) |>
  fmt_number(columns = c(minutes), decimals = 0) |>
  fmt_number(columns = c(ortg, drtg, net), decimals = 1) |>
  fmt_percent(
    columns = c(two_fg_pct, three_fg_pct, opp_two_fg_pct, opp_three_fg_pct),
    decimals = 1
  ) |>
  gt_bucks_theme()

gtsave(giannis_on_off_table, file.path(dir_tables, "giannis_on_off_table.html"))

# -------------------------------------------------------------------------
# Save table object list
# -------------------------------------------------------------------------

report_tables <- list(
  giannis_awards_table = giannis_awards_table,
  team_results_table = team_results_table,
  transaction_summary_table = transaction_summary_table,
  future_picks_table = future_picks_table,
  current_roster_table = current_roster_table,
  young_pieces_table = young_pieces_table,
  draft_prospects_table = draft_prospects_table,
  giannis_on_off_table = giannis_on_off_table
)

saveRDS(report_tables, file.path(dir_processed, "report_tables.rds"))

message("Summary tables built and saved to outputs/tables.")
