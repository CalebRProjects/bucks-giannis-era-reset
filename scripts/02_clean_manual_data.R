# scripts/02_clean_manual_data.R

# Purpose:
# Clean manually compiled source data into processed analysis-ready files.

source("scripts/01_load_manual_data.R")

# -------------------------------------------------------------------------
# 1. Giannis regular-season stats
# -------------------------------------------------------------------------

giannis_stats_clean <- giannis_stats_raw |>
  mutate(
    season = as.character(season),
    season_start = season_start_year(season),
    age = as.numeric(age),
    gp = as.numeric(gp),
    ppg = as.numeric(ppg),
    rpg = as.numeric(rpg),
    apg = as.numeric(apg),
    tov = as.numeric(tov),
    stl = as.numeric(stl),
    blk = as.numeric(blk),
    fga = as.numeric(fga),
    ts_pct = parse_pct(ts_percent),
    rts_pct = as.numeric(r_ts_percent),
    era_phase = case_when(
      season_start <= 2016 ~ "Development",
      season_start <= 2018 ~ "MVP rise",
      season_start <= 2021 ~ "Peak/title window",
      season_start <= 2024 ~ "Post-title chase",
      TRUE ~ "Final Milwaukee years"
    )
  ) |>
  arrange(season_start)

save_processed(giannis_stats_clean, "giannis_stats_clean.csv")

# -------------------------------------------------------------------------
# 2. Giannis awards and milestones
# -------------------------------------------------------------------------

giannis_awards_clean <- giannis_awards_raw |>
  mutate(
    season = as.character(season),
    season_start = season_start_year(season),
    event_type = clean_text(event_type),
    event = clean_text(event) |>
      str_remove(",$"),
    notes = clean_text(notes)
  ) |>
  arrange(season_start)

save_processed(giannis_awards_clean, "giannis_awards_clean.csv")

# -------------------------------------------------------------------------
# 3. Giannis on/off since first MVP
# -------------------------------------------------------------------------

giannis_on_off_clean <- giannis_on_off_raw |>
  mutate(
    status = str_to_title(clean_text(status)),
    minutes = as.numeric(minutes),
    ortg = as.numeric(ortg),
    drtg = as.numeric(drtg),
    net = as.numeric(net),
    two_fg_pct = parse_pct(x2fg_percent),
    three_fg_pct = parse_pct(x3fg_percent),
    opp_two_fg_pct = parse_pct(o2fg_percent),
    opp_three_fg_pct = parse_pct(o3fg_percent)
  )

save_processed(giannis_on_off_clean, "giannis_on_off_clean.csv")

# -------------------------------------------------------------------------
# 4. Bucks team results
# -------------------------------------------------------------------------

bucks_team_results_clean <- bucks_team_results_raw |>
  mutate(
    season = as.character(season),
    season_start = season_start_year(season),
    wins = as.numeric(wins),
    losses = as.numeric(losses),
    win_pct = parse_pct(win_pct),
    seed = as.numeric(seed),
    playoff_result = clean_text(playoff_result),
    playoff_result = replace_na(playoff_result, "Missed playoffs"),
    coach = clean_text(coach),
    ortg = as.numeric(ortg),
    rortg = as.numeric(r_ortg),
    drtg = as.numeric(drtg),
    rdrtg = as.numeric(r_drtg),
    net = as.numeric(net),
    srs = as.numeric(srs),
    pace = as.numeric(pace),
    rpace = as.numeric(r_pace),
    era_phase = case_when(
      season_start <= 2016 ~ "Development",
      season_start <= 2018 ~ "MVP rise",
      season_start <= 2021 ~ "Peak/title window",
      season_start <= 2024 ~ "Post-title chase",
      TRUE ~ "Final Milwaukee years"
    )
  ) |>
  arrange(season_start)

save_processed(bucks_team_results_clean, "bucks_team_results_clean.csv")

# -------------------------------------------------------------------------
# 5. Bucks playoff results
# -------------------------------------------------------------------------

bucks_playoffs_clean <- bucks_playoffs_raw |>
  mutate(
    season = as.character(season),
    season_start = season_start_year(season),
    round_reached = clean_text(round_reached),
    result = clean_text(result),
    opponent = clean_text(opponent),
    record = clean_text(record),
    notes = clean_text(notes),
    playoff_level = case_when(
      str_detect(round_reached, regex("Finals", ignore_case = TRUE)) ~ 4,
      str_detect(round_reached, regex("ECF", ignore_case = TRUE)) ~ 3,
      str_detect(round_reached, regex("ECSF", ignore_case = TRUE)) ~ 2,
      str_detect(round_reached, regex("ECR1", ignore_case = TRUE)) ~ 1,
      TRUE ~ NA_real_
    )
  ) |>
  arrange(season_start)

save_processed(bucks_playoffs_clean, "bucks_playoff_results_clean.csv")

# -------------------------------------------------------------------------
# 6. Transaction timeline
# -------------------------------------------------------------------------

transaction_timeline_clean <- transaction_timeline_raw |>
  rename(
    picks_in = any_of(c("pick_in", "picks_in"))
  ) |>
  mutate(
    date = ymd(date),
    season = as.character(season),
    season_start = season_start_year(season),
    transaction_type = clean_text(transaction_type),
    players_out = clean_text(players_out),
    players_in = clean_text(players_in),
    picks_out = clean_text(picks_out),
    picks_in = clean_text(picks_in),
    notes = clean_text(notes),
    timeline_group = case_when(
      str_detect(transaction_type, regex("Draft", ignore_case = TRUE)) ~ "Draft",
      str_detect(transaction_type, regex("Trade", ignore_case = TRUE)) ~ "Trade",
      str_detect(transaction_type, regex("Signing|Departure|Waive", ignore_case = TRUE)) ~ "Roster move",
      TRUE ~ "Other"
    )
  ) |>
  arrange(date)

save_processed(transaction_timeline_clean, "transaction_timeline_clean.csv")

# -------------------------------------------------------------------------
# 7. Future picks
# -------------------------------------------------------------------------

future_picks_clean <- future_picks_raw |>
  mutate(
    year = as.integer(year),
    round = clean_text(round),
    status = clean_text(status),
    bucks_pick_result = clean_text(bucks_pick_result),
    outgoing_to = clean_text(outgoing_to),
    incoming_from = clean_text(incoming_from),
    protection_or_swap = clean_text(protection_or_swap),
    report_label = clean_text(report_label),
    pick_bucket = case_when(
      str_detect(status, regex("Incoming", ignore_case = TRUE)) ~ "Incoming",
      str_detect(status, regex("Retained", ignore_case = TRUE)) ~ "Retained",
      str_detect(status, regex("Swap", ignore_case = TRUE)) ~ "Swap/limited control",
      str_detect(status, regex("Outgoing", ignore_case = TRUE)) ~ "Outgoing",
      TRUE ~ "Other"
    )
  ) |>
  arrange(year, round, status)

save_processed(future_picks_clean, "future_picks_clean.csv")

# -------------------------------------------------------------------------
# 8. Current roster
# -------------------------------------------------------------------------

current_roster_clean <- current_roster_raw |>
  mutate(
    player = clean_text(player),
    pos = clean_text(pos),
    age = as.numeric(age),
    height = clean_text(height),
    weight = as.numeric(weight),
    salary_raw = clean_text(salary),
    salary_millions = parse_money_millions(salary),
    timeline_group = case_when(
      age <= 22 ~ "Development",
      age <= 26 ~ "Bridge/core",
      age <= 30 ~ "Prime veteran",
      age > 30 ~ "Older veteran",
      TRUE ~ "Unknown"
    ),
    roster_bucket = case_when(
      player %in% c("Tyler Herro", "Kel'el Ware", "Jaime Jaquez Jr.", "Kasparas Jakucionis") ~ "Trade return",
      player %in% c("Brayden Burries", "Nate Ament") ~ "2026 draft",
      age <= 26 ~ "Young/bridge piece",
      TRUE ~ "Veteran"
    )
  ) |>
  arrange(age)

save_processed(current_roster_clean, "current_roster_clean.csv")

# -------------------------------------------------------------------------
# 9. Young NBA pieces
# -------------------------------------------------------------------------

young_pieces_clean <- young_pieces_raw |>
  mutate(
    player = clean_text(player),
    ppg = as.numeric(ppg),
    rpg = as.numeric(rpg),
    apg = as.numeric(apg),
    spg = as.numeric(spg),
    bpg = as.numeric(bpg),
    ts_pct = parse_pct(ts_percent),
    usg_pct = parse_pct(usg_percent),
    mpg = as.numeric(mpg),
    player_type = case_when(
      player == "Tyler Herro" ~ "Scoring bridge",
      player == "Kel'el Ware" ~ "Developmental big",
      player == "Jaime Jaquez Jr." ~ "Connector wing",
      player == "Ryan Rollins" ~ "Guard evaluation",
      player == "Ousmane Dieng" ~ "Upside forward",
      player == "AJ Green" ~ "Movement shooter",
      player == "Andre Jackson Jr." ~ "Defense/energy wing",
      TRUE ~ "Young piece"
    )
  ) |>
  arrange(desc(ppg))

save_processed(young_pieces_clean, "young_pieces_clean.csv")

# -------------------------------------------------------------------------
# 10. Incoming draft prospects
# -------------------------------------------------------------------------

draft_prospects_clean <- draft_prospects_raw |>
  mutate(
    player = clean_text(player),
    ppg = as.numeric(ppg),
    rpg = as.numeric(rpg),
    apg = as.numeric(apg),
    tov = as.numeric(tov),
    two_fg_pct = parse_pct(x2fg_percent),
    three_fg_pct = parse_pct(x3fg_percent),
    ft_pct = parse_pct(ft_percent),
    two_fga = as.numeric(x2fga),
    three_fga = as.numeric(x3fga),
    fta = as.numeric(fta),
    mock_pos = clean_text(mock_pos),
    prospect_type = case_when(
      player == "Nate Ament" ~ "Big wing upside",
      player == "Brayden Burries" ~ "Scoring guard/wing",
      TRUE ~ "Draft prospect"
    )
  ) |>
  arrange(player)

save_processed(draft_prospects_clean, "draft_prospects_clean.csv")

# -------------------------------------------------------------------------
# 11. Combined new-core table
# -------------------------------------------------------------------------

young_nba_for_core <- young_pieces_clean |>
  transmute(
    player,
    source_group = "Young NBA piece",
    ppg,
    rpg,
    apg,
    ts_pct,
    usg_pct,
    mpg,
    role_label = player_type
  )

draft_for_core <- draft_prospects_clean |>
  transmute(
    player,
    source_group = "Incoming draft prospect",
    ppg,
    rpg,
    apg,
    ts_pct = NA_real_,
    usg_pct = NA_real_,
    mpg = NA_real_,
    role_label = prospect_type
  )

new_core_clean <- bind_rows(
  young_nba_for_core,
  draft_for_core
) |>
  left_join(
    current_roster_clean |>
      select(player, age, pos, salary_millions, roster_bucket),
    by = "player"
  ) |>
  arrange(age, desc(ppg))

save_processed(new_core_clean, "new_core_clean.csv")

message("Manual data cleaned and saved to data/processed.")
