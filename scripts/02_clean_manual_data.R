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
# 3B. Giannis Bucks accomplishments
# -------------------------------------------------------------------------

giannis_accomplishments_clean <- giannis_accomplishments_raw |>
  mutate(
    accomplishment = clean_text(accomplishment),
    value = clean_text(value),
    notes = clean_text(notes),

    # Clean display labels
    accomplishment = case_when(
      accomplishment == "NBA Cup MVPs" ~ "NBA Cup MVP",
      accomplishment == "MIP" ~ "Most Improved Player",
      TRUE ~ accomplishment
    ),

    # Clean note labels for readability
    notes = case_when(
      accomplishment == "All-NBA selections" ~ "7x First Team, 2x Second Team",
      accomplishment == "All-Defense selections" ~ "4x First Team, 1x Second Team",
      accomplishment == "25+ PPG seasons" ~ "2017-18 through 2025-26",
      accomplishment == "40-point games" ~ "56 regular season, 8 playoffs",
      accomplishment == "50-point games" ~ "9 regular season, 1 playoffs",
      accomplishment == "60-point games" ~ "1 regular season",
      TRUE ~ notes
    ),

    value_num = suppressWarnings(as.numeric(value)),

    accomplishment_group = case_when(
      accomplishment %in% c(
        "Championships",
        "NBA Cup Championships"
      ) ~ "Team accomplishments",

      accomplishment %in% c(
        "MVPs",
        "Finals MVPs",
        "NBA Cup MVP",
        "Most Improved Player",
        "Defensive Player of the Year"
      ) ~ "Major awards",

      accomplishment %in% c(
        "All-NBA selections",
        "All-Star selections",
        "All-Defense selections"
      ) ~ "All-league recognition",

      accomplishment %in% c(
        "25+ PPG seasons",
        "40-point games",
        "50-point games",
        "60-point games",
        "Playoff series wins",
        "Playoff wins"
      ) ~ "Scoring and playoff production",

      TRUE ~ "Other"
    ),

    accomplishment_group = factor(
      accomplishment_group,
      levels = c(
        "Team accomplishments",
        "Major awards",
        "All-league recognition",
        "Scoring and playoff production",
        "Other"
      )
    ),

    accomplishment_order = case_when(
      accomplishment == "Championships" ~ 1,
      accomplishment == "NBA Cup Championships" ~ 2,

      accomplishment == "MVPs" ~ 1,
      accomplishment == "Finals MVPs" ~ 2,
      accomplishment == "Defensive Player of the Year" ~ 3,
      accomplishment == "Most Improved Player" ~ 4,
      accomplishment == "NBA Cup MVP" ~ 5,

      accomplishment == "All-Star selections" ~ 1,
      accomplishment == "All-NBA selections" ~ 2,
      accomplishment == "All-Defense selections" ~ 3,

      accomplishment == "25+ PPG seasons" ~ 1,
      accomplishment == "Playoff series wins" ~ 2,
      accomplishment == "Playoff wins" ~ 3,
      accomplishment == "40-point games" ~ 4,
      accomplishment == "50-point games" ~ 5,
      accomplishment == "60-point games" ~ 6,

      TRUE ~ 99
    )
  ) |>
  arrange(accomplishment_group, accomplishment_order)

save_processed(giannis_accomplishments_clean, "giannis_accomplishments_clean.csv")

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
    ),
    future_relevant = case_when(
      player %in% c("Kyle Kuzma", "Myles Turner", "Kevin Porter Jr.") ~ FALSE,
      TRUE ~ TRUE
    )
  ) |>
  arrange(age)

save_processed(current_roster_clean, "current_roster_clean.csv")

# Functional/future-relevant roster age summary for article framing
roster_age_summary_clean <- current_roster_clean |>
  summarise(
    full_roster_avg_age = mean(age, na.rm = TRUE),
    future_relevant_avg_age = mean(age[future_relevant], na.rm = TRUE),
    full_roster_count = n(),
    future_relevant_count = sum(future_relevant, na.rm = TRUE)
  )

save_processed(roster_age_summary_clean, "roster_age_summary_clean.csv")

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
      player == "Nate Ament" ~ "Big shooting wing upside",
      player == "Brayden Burries" ~ "Two-way combo guard",
      TRUE ~ "Draft prospect"
    )
  ) |>
  arrange(player)

save_processed(draft_prospects_clean, "draft_prospects_clean.csv")

# -------------------------------------------------------------------------
# 10B. DataBallr player profiles
# -------------------------------------------------------------------------

databallr_profiles_clean <- databallr_profiles_raw |>
  mutate(
    player = clean_text(player),
    pos = clean_text(pos),
    across(
      c(
        dpm, o_dpm, d_dpm, orapm, drapm, rapm, min,
        shots, rts, stov, owmiss, potast, astefg, passtov,
        three_pr, three_pr_pctile, three_pct, three_pct_pctile,
        ftr, teammiss, onball, pa_1m, rimast,
        dfga, diff, drb_pct, stl, offd, blk, rimdfga, rim_diff,
        lballs, stop_pct, rtov, defl
      ),
      ~ suppressWarnings(as.numeric(.x))
    ),
    role_label = case_when(
      player == "Tyler Herro" ~ "Offensive stabilizer",
      player == "Jaime Jaquez Jr." ~ "Connector wing",
      player == "Kel'el Ware" ~ "Upside big",
      player == "Kasparas Jakucionis" ~ "Skill guard bet",
      TRUE ~ "Young piece"
    ),
    main_read = case_when(
      player == "Tyler Herro" ~ "High-volume scoring and spacing bridge with defensive limitations",
      player == "Jaime Jaquez Jr." ~ "Safe rotation profile with on-ball feel and shooting questions",
      player == "Kel'el Ware" ~ "Athletic big with shooting, rebounding, and rim-protection indicators",
      player == "Kasparas Jakucionis" ~ "Shooting signal with guard-skill development questions",
      TRUE ~ NA_character_
    ),
    swing_skill = case_when(
      player == "Tyler Herro" ~ "Defense and playmaking scalability",
      player == "Jaime Jaquez Jr." ~ "Three-point shooting",
      player == "Kel'el Ware" ~ "Motor, strength, and defensive consistency",
      player == "Kasparas Jakucionis" ~ "Creation and finishing",
      TRUE ~ NA_character_
    )
  )

save_processed(databallr_profiles_clean, "databallr_profiles_clean.csv")

# -------------------------------------------------------------------------
# 10C. DataBallr playtypes
# -------------------------------------------------------------------------

databallr_playtypes_clean <- databallr_playtypes_raw |>
  mutate(
    player = clean_text(player),
    playtype = clean_text(playtype),
    freq = as.numeric(freq),
    rts_impact = as.numeric(rts_impact),
    impact_bucket = case_when(
      rts_impact >= 5 ~ "Strong positive",
      rts_impact > 0 ~ "Positive",
      rts_impact == 0 ~ "Neutral",
      rts_impact < 0 ~ "Negative",
      TRUE ~ "Unknown"
    )
  )

save_processed(databallr_playtypes_clean, "databallr_playtypes_clean.csv")

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
      select(player, age, pos, salary_millions, roster_bucket, future_relevant),
    by = "player"
  ) |>
  left_join(
    databallr_profiles_clean |>
      select(player, databallr_role = role_label, main_read, swing_skill),
    by = "player"
  ) |>
  mutate(
    role_label = coalesce(databallr_role, role_label)
  ) |>
  select(-databallr_role) |>
  arrange(age, desc(ppg))

save_processed(new_core_clean, "new_core_clean.csv")

message("Manual data cleaned and saved to data/processed.")
