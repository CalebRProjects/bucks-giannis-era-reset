# scripts/03_build_summary_tables.R

# Purpose:
# Build curated summary tables for the Substack report and save appendix tables.

source("scripts/02_clean_manual_data.R")

# -------------------------------------------------------------------------
# Save helpers
# -------------------------------------------------------------------------

save_gt_table <- function(gt_obj, file_stub, table_dir = dir_tables) {
  html_path <- file.path(table_dir, paste0(file_stub, ".html"))
  png_path  <- file.path(table_dir, paste0(file_stub, ".png"))

  gt::gtsave(gt_obj, html_path)
  gt::gtsave(gt_obj, png_path)

  message("Saved table: ", file_stub, " (.html, .png)")
}


# -------------------------------------------------------------------------
# 1. Giannis accomplishments table
# -------------------------------------------------------------------------

giannis_accomplishments_table <- giannis_accomplishments_clean |>
  arrange(accomplishment_group, accomplishment_order) |>
  select(accomplishment_group, accomplishment, value, notes) |>
  gt(groupname_col = "accomplishment_group") |>
  tab_header(
    title = md("**Giannis’ Milwaukee Résumé**"),
    subtitle = "The case for the greatest Buck ever is not complicated"
  ) |>
  cols_label(
    accomplishment = "Accomplishment",
    value = "Count",
    notes = "Context"
  ) |>
  cols_width(
    accomplishment ~ px(230),
    value ~ px(90),
    notes ~ px(430)
  ) |>
  tab_options(
    row_group.background.color = "#EFE6D8",
    row_group.border.top.color = GRID,
    row_group.border.bottom.color = GRID,
    data_row.padding = px(7),
    table.font.names = c("Arial", "Helvetica", "sans-serif"),
    table.font.size = px(15),
    heading.title.font.size = px(22),
    heading.subtitle.font.size = px(14)
  ) |>
  tab_style(
    style = cell_text(
      weight = "bold",
      color = TEXT_DARK,
      size = px(18),
      align = "center"
    ),
    locations = cells_body(columns = value)
  ) |>
  tab_style(
    style = cell_text(
      weight = "bold",
      color = TEXT_DARK,
      transform = "uppercase",
      size = px(12)
    ),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = TEXT_DARK),
    locations = cells_column_labels(columns = everything())
  ) |>
  tab_style(
    style = cell_text(color = TEXT_DARK),
    locations = cells_body(columns = c(accomplishment, notes))
  ) |>
  tab_style(
    style = cell_text(color = TEXT_LIGHT, size = px(13)),
    locations = cells_body(columns = notes)
  ) |>
  sub_missing(columns = everything(), missing_text = "") |>
  gt_bucks_theme()

save_gt_table(giannis_accomplishments_table, "giannis_accomplishments_table")

# -------------------------------------------------------------------------
# Window-shaping moves table
# -------------------------------------------------------------------------

window_moves_table <- tibble::tribble(
  ~year, ~move, ~phase, ~why_it_mattered,
  2013, "Drafted Giannis Antetokounmpo", "Foundation", "The swing that changed the franchise.",
  2013, "Acquired Khris Middleton", "Foundation", "The eventual co-star arrived around the same time.",
  2017, "Traded for Eric Bledsoe", "First push", "Milwaukee started spending around Giannis.",
  2018, "Signed Brook Lopez", "Core build", "Lopez gave the title core its spacing and defensive backbone.",
  2020, "Signed Bobby Portis", "Title build", "A low-cost signing became a major part of the team’s identity.",
  2020, "Traded for Jrue Holiday", "Title build", "The all-in move that pushed them over the top.",
  2021, "Won the championship", "Peak", "The payoff for years of aggressive building.",
  2023, "Traded for Damian Lillard", "Final swing", "Milwaukee moved off Jrue to pair Dame and Giannis.",
  2025, "Moved Khris Middleton for Kyle Kuzma", "Core breaks", "The original title core no longer intact.",
  2025, "Waived Damian Lillard and signed Myles Turner", "Scramble", "The retool turned into a scramble.",
  2026, "Traded Giannis Antetokounmpo", "Reset", "The era officially ended.",
  2026, "Drafted Brayden Burries and Nate Ament", "Reset", "The next timeline started with two lottery picks."
) |>
  gt() |>
  tab_header(
    title = md("**The Moves That Built and Ended the Giannis Era**"),
    subtitle = "Milwaukee’s Giannis era moved from discovery, to title spending, to one last scramble before the resetting"
  ) |>
  cols_label(
    year = "Year",
    move = "Move",
    phase = "Phase",
    why_it_mattered = "Why It Mattered"
  ) |>
  cols_width(
    year ~ px(70),
    move ~ px(285),
    phase ~ px(160),
    why_it_mattered ~ px(510)
  ) |>
  tab_options(
    table.background.color = BG,
    heading.background.color = BG,
    column_labels.background.color = BG,
    table.border.top.color = GRID,
    table.border.bottom.color = GRID,
    column_labels.border.top.color = GRID,
    column_labels.border.bottom.color = GRID,
    table.font.names = c("Arial", "Helvetica", "sans-serif"),
    table.font.size = px(15),
    heading.title.font.size = px(22),
    heading.subtitle.font.size = px(14),
    data_row.padding = px(8)
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = TEXT_DARK),
    locations = cells_column_labels(columns = everything())
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = TEXT_DARK),
    locations = cells_body(columns = year)
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = BUCKS_GREEN),
    locations = cells_body(columns = phase)
  ) |>
  tab_style(
    style = cell_text(color = TEXT_MID, size = px(13)),
    locations = cells_body(columns = why_it_mattered)
  ) |>
  sub_missing(columns = everything(), missing_text = "") |>
  gt_bucks_theme()

save_gt_table(window_moves_table, "window_moves_table")

# -------------------------------------------------------------------------
# 2. Miami return profile table
# -------------------------------------------------------------------------

miami_return_profile_table <- databallr_profiles_clean |>
  left_join(
    current_roster_clean |>
      select(player, age),
    by = "player"
  ) |>
  mutate(
    what_pops = case_when(
      player == "Tyler Herro" ~ "20.5 PPG, 4.8 RPG, 4.1 APG (60.2 TS%)",
      player == "Jaime Jaquez Jr." ~ "15.4 PPG, 5.0 RPG, 4.7 APG (57.0 TS%)",
      player == "Kel'el Ware" ~ "11.1 PPG, 9.0 RPG (2.8 ORPG), 0.7 APG (61.6 TS%)",
      player == "Kasparas Jakucionis" ~ "6.2 PPG, 2.6 RPG, 2.6 APG, 17.8 MPG (61.4 TS%)",
      TRUE ~ main_read
    ),
    main_question = case_when(
      player == "Tyler Herro" ~ "Is he part of the next core or the next player moved?",
      player == "Jaime Jaquez Jr." ~ "How does he fit into this balance of youth and contributing players?",
      player == "Kel'el Ware" ~ "Can the motor, strength, and defensive consistency catch up to the tools?",
      player == "Kasparas Jakucionis" ~ "Can he become an actual advantage creator, not just a shooter/passer?",
      TRUE ~ swing_skill
    )
  ) |>
  arrange(player) |>
  select(player, age, pos, what_pops, main_question) |>
  gt() |>
  tab_header(
    title = md("**What Milwaukee Got From Miami**"),
    subtitle = "Scoring, connective play, frontcourt upside, guard skill, and draft capital"
  ) |>
  cols_label(
    player = "Player",
    age = "Age",
    pos = "Pos",
    what_pops = "Last Season",
    main_question = "Main Question"
  ) |>
  cols_width(
    player ~ px(140),
    age ~ px(70),
    pos ~ px(60),
    what_pops ~ px(300),
    main_question ~ px(360)
  ) |>
  fmt_number(columns = age, decimals = 1) |>
  tab_options(
    table.background.color = BG,
    heading.background.color = BG,
    column_labels.background.color = BG,
    table.border.top.color = GRID,
    table.border.bottom.color = GRID,
    column_labels.border.top.color = GRID,
    column_labels.border.bottom.color = GRID,
    table.font.names = c("Arial", "Helvetica", "sans-serif"),
    table.font.size = px(15),
    heading.title.font.size = px(22),
    heading.subtitle.font.size = px(14),
    data_row.padding = px(8)
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = TEXT_DARK),
    locations = cells_body(columns = player)
  ) |>
  tab_style(
    style = cell_text(color = TEXT_MID, size = px(13)),
    locations = cells_body(columns = c(what_pops, main_question))
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = TEXT_DARK),
    locations = cells_column_labels(columns = everything())
  ) |>
  sub_missing(columns = everything(), missing_text = "") |>
  gt_bucks_theme()

save_gt_table(miami_return_profile_table, "miami_return_profile_table")

# -------------------------------------------------------------------------
# 2B. Miami return DataBallr metrics table
# -------------------------------------------------------------------------

miami_return_metrics_table <- databallr_profiles_clean |>
  mutate(
    player = factor(
      player,
      levels = c("Tyler Herro", "Jaime Jaquez Jr.", "Kel'el Ware", "Kasparas Jakucionis")
    )
  ) |>
  arrange(player) |>
  select(
    player, dpm, o_dpm, d_dpm,
    shots, rts, three_pr, three_pct,
    potast, onball, drb_pct, blk
  ) |>
  gt() |>
  tab_header(
    title = md("**DataBallr Snapshot: Miami Return**"),
    subtitle = "Selected impact, shooting, creation, and frontcourt indicators"
  ) |>
  cols_label(
    player = "Player",
    dpm = "DPM",
    o_dpm = "O-DPM",
    d_dpm = "D-DPM",
    shots = "Shots",
    rts = "rTS",
    three_pr = "3Pr",
    three_pct = "3P%",
    potast = "PotAst",
    onball = "On-Ball",
    drb_pct = "DRB%",
    blk = "BLK"
  ) |>
  fmt_number(
    columns = c(
      dpm, o_dpm, d_dpm, shots, rts,
      three_pr, three_pct, potast, onball, drb_pct, blk
    ),
    decimals = 1
  ) |>
  tab_options(
    table.background.color = BG,
    heading.background.color = BG,
    column_labels.background.color = BG,
    table.border.top.color = GRID,
    table.border.bottom.color = GRID,
    column_labels.border.top.color = GRID,
    column_labels.border.bottom.color = GRID,
    table.font.names = c("Arial", "Helvetica", "sans-serif"),
    table.font.size = px(14),
    heading.title.font.size = px(22),
    heading.subtitle.font.size = px(14),
    data_row.padding = px(7)
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = TEXT_DARK),
    locations = cells_body(columns = player)
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = TEXT_DARK),
    locations = cells_column_labels(columns = everything())
  ) |>
  sub_missing(columns = everything(), missing_text = "") |>
  gt_bucks_theme()

save_gt_table(miami_return_metrics_table, "miami_return_metrics_table")

# -------------------------------------------------------------------------
# 3. Playoff results table
# -------------------------------------------------------------------------

playoff_results_article_table <- bucks_playoffs_clean |>
  mutate(
    article_read = case_when(
      season == "2016-17" ~ "First playoff taste of the Giannis era",
      season == "2017-18" ~ "Still building around the rising star",
      season == "2018-19" ~ "First true title-level team",
      season == "2019-20" ~ "Elite regular season did not translate in the bubble",
      season == "2020-21" ~ "Title breakthrough",
      season == "2021-22" ~ "Middleton injury changed the ceiling",
      season == "2022-23" ~ "58-win team, but cracks showed",
      season == "2023-24" ~ "Giannis injury, Dame era never stabilized",
      season == "2024-25" ~ "Dame Achilles, window effectively closed",
      TRUE ~ notes
    )
  ) |>
  select(season, result, opponent, record, article_read) |>
  gt() |>
  tab_header(
    title = md("**The Playoff Ceiling Closed Quickly After the Title**"),
    subtitle = "Milwaukee won the championship in 2021, then never got back past Round 2"
  ) |>
  cols_label(
    season = "Season",
    result = "Result",
    opponent = "Opponent",
    record = "Series",
    article_read = "What It Meant"
  ) |>
  cols_width(
    season ~ px(90),
    result ~ px(120),
    opponent ~ px(120),
    record ~ px(90),
    article_read ~ px(430)
  ) |>
  tab_options(
    table.background.color = BG,
    heading.background.color = BG,
    column_labels.background.color = BG,
    table.border.top.color = GRID,
    table.border.bottom.color = GRID,
    column_labels.border.top.color = GRID,
    column_labels.border.bottom.color = GRID,
    table.font.names = c("Arial", "Helvetica", "sans-serif"),
    table.font.size = px(15),
    heading.title.font.size = px(22),
    heading.subtitle.font.size = px(14),
    data_row.padding = px(7)
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = TEXT_DARK),
    locations = cells_column_labels(columns = everything())
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = TEXT_DARK),
    locations = cells_body(columns = season)
  ) |>
  tab_style(
    style = cell_text(color = TEXT_MID, size = px(13)),
    locations = cells_body(columns = article_read)
  ) |>
  sub_missing(columns = everything(), missing_text = "") |>
  gt_bucks_theme()

save_gt_table(playoff_results_article_table, "playoff_results_article_table")

# -------------------------------------------------------------------------
# 4. Miami shooting table
# -------------------------------------------------------------------------

miami_shooting_table <- databallr_profiles_clean |>
  mutate(
    three_pr_pctile = case_when(
      player == "Tyler Herro" ~ 26,
      player == "Kasparas Jakucionis" ~ 93,
      player == "Kel'el Ware" ~ 88,
      player == "Jaime Jaquez Jr." ~ 3,
      TRUE ~ NA_real_
    ),
    three_pct_pctile = case_when(
      player == "Tyler Herro" ~ 64,
      player == "Kasparas Jakucionis" ~ 98,
      player == "Kel'el Ware" ~ 88,
      player == "Jaime Jaquez Jr." ~ 20,
      TRUE ~ NA_real_
    ),
    player = factor(
      player,
      levels = c(
        "Tyler Herro",
        "Kasparas Jakucionis",
        "Kel'el Ware",
        "Jaime Jaquez Jr."
      )
    ),
    shooting_read = case_when(
      player == "Tyler Herro" ~ "Known volume spacer",
      player == "Kasparas Jakucionis" ~ "Best shooting signal in the return",
      player == "Kel'el Ware" ~ "Rare spacing indicator for a young big",
      player == "Jaime Jaquez Jr." ~ "Main shooting swing question",
      TRUE ~ ""
    )
  ) |>
  arrange(player) |>
  select(
    player,
    three_pr,
    three_pr_pctile,
    three_pct,
    three_pct_pctile,
    shooting_read
  ) |>
  gt() |>
  tab_header(
    title = md("**The Cleanest Skill Signal Is Shooting**"),
    subtitle = "3-point rate and 3-point percentage, with positional percentile ranks"
  ) |>
  cols_label(
    player = "Player",
    three_pr = "3Pr",
    three_pr_pctile = "3Pr %ile",
    three_pct = "3P%",
    three_pct_pctile = "3P% %ile",
    shooting_read = "Read"
  ) |>
  fmt_number(columns = three_pr, decimals = 2) |>
  fmt_number(columns = three_pct, decimals = 1) |>
  fmt_number(columns = c(three_pr_pctile, three_pct_pctile), decimals = 0) |>
  tab_options(
    table.background.color = BG,
    heading.background.color = BG,
    column_labels.background.color = BG,
    table.border.top.color = GRID,
    table.border.bottom.color = GRID,
    column_labels.border.top.color = GRID,
    column_labels.border.bottom.color = GRID,
    table.font.names = c("Arial", "Helvetica", "sans-serif"),
    table.font.size = px(15),
    heading.title.font.size = px(22),
    heading.subtitle.font.size = px(14),
    data_row.padding = px(8)
  ) |>
  data_color(
    columns = c(three_pr_pctile, three_pct_pctile),
    method = "numeric",
    palette = c("#F2DFD8", "#F3E7C9", "#D7E8D2", "#A8D5A2")
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = TEXT_DARK),
    locations = cells_body(columns = player)
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = TEXT_DARK),
    locations = cells_column_labels(columns = everything())
  ) |>
  tab_style(
    style = cell_text(color = TEXT_MID, size = px(13)),
    locations = cells_body(columns = shooting_read)
  ) |>
  tab_source_note(
    source_note = md(
      "Source: DataBallr | Percentile ranks are based on positional-ranked percentiles."
    )
  ) |>
  sub_missing(columns = everything(), missing_text = "") |>
  gt_bucks_theme()

save_gt_table(miami_shooting_table, "miami_shooting_table")

# -------------------------------------------------------------------------
# 4B. Contract context table
# -------------------------------------------------------------------------

format_millions <- function(x) {
  case_when(
    is.na(x) ~ "",
    x >= 1e6 ~ paste0("$", round(x / 1e6, 1), "M"),
    TRUE ~ paste0("$", scales::comma(x))
  )
}

contract_context_table <- current_roster_clean |>
  filter(
    player %in% c(
      "Tyler Herro",
      "Kel'el Ware",
      "Jaime Jaquez Jr.",
      "Kasparas Jakučionis",
      "Brayden Burries",
      "Nate Ament",
      "Ryan Rollins",
      "AJ Green",
      "Andre Jackson Jr.",
      "Kyle Kuzma",
      "Myles Turner",
      "Kevin Porter Jr.",
      "Gary Harris",
      "Jericho Sims"
    )
  ) |>
  mutate(
    player = factor(
      player,
      levels = c(
        "Tyler Herro",
        "Jaime Jaquez Jr.",
        "Kel'el Ware",
        "Kasparas Jakučionis",
        "Brayden Burries",
        "Nate Ament",
        "Ryan Rollins",
        "AJ Green",
        "Andre Jackson Jr.",
        "Myles Turner",
        "Kyle Kuzma",
        "Kevin Porter Jr.",
        "Gary Harris",
        "Jericho Sims"
      )
    ),
    salary_label = format_millions(salary_2026),
    future_money_label = format_millions(future_money_remaining),
    total_commitment_label = format_millions(total_remaining_commitment),
    years_left_label = case_when(
      is.na(years_remaining) ~ "",
      years_remaining == 1 ~ "1 year",
      TRUE ~ paste0(years_remaining, " years")
    )
  ) |>
  arrange(player) |>
  select(
    player,
    age,
    salary_label,
    years_left_label,
    future_money_label,
    total_commitment_label,
    option_type,
  ) |>
  gt() |>
  tab_header(
    title = md("**The Contract Sheet Impacts the Reset**"),
    subtitle = "Data: Spotrac | Turner, Kuzma, and Herro are potential trade pieces."
  ) |>
  cols_label(
    player = "Player",
    age = "Age",
    salary_label = "2026 Salary",
    years_left_label = "Years Left",
    future_money_label = "Future Money",
    total_commitment_label = "Total Left",
    option_type = "Status"
  ) |>
  fmt_number(columns = age, decimals = 1) |>
  cols_width(
    player ~ px(145),
    age ~ px(60),
    salary_label ~ px(95),
    years_left_label ~ px(85),
    future_money_label ~ px(105),
    total_commitment_label ~ px(105),
    option_type ~ px(110)
  ) |>
  tab_options(
    table.background.color = BG,
    heading.background.color = BG,
    column_labels.background.color = BG,
    table.border.top.color = GRID,
    table.border.bottom.color = GRID,
    column_labels.border.top.color = GRID,
    column_labels.border.bottom.color = GRID,
    table.font.names = c("Arial", "Helvetica", "sans-serif"),
    table.font.size = px(14),
    heading.title.font.size = px(22),
    heading.subtitle.font.size = px(14),
    data_row.padding = px(7)
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = TEXT_DARK),
    locations = cells_body(columns = player)
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = TEXT_DARK),
    locations = cells_column_labels(columns = everything())
  ) |>
  sub_missing(columns = everything(), missing_text = "") |>
  gt_bucks_theme()

save_gt_table(contract_context_table, "contract_context_table")

# -------------------------------------------------------------------------
# 5. DataBallr playtype table
# -------------------------------------------------------------------------

databallr_playtype_table <- databallr_playtypes_clean |>
  arrange(player, desc(rts_impact)) |>
  gt(groupname_col = "player") |>
  tab_header(
    title = "DataBallr Playtype Snapshot",
    subtitle = "Frequency and rTS impact by playtype"
  ) |>
  cols_label(
    playtype = "Playtype",
    freq = "Freq%",
    rts_impact = "rTS Impact",
    impact_bucket = "Impact Bucket"
  ) |>
  fmt_number(columns = c(freq, rts_impact), decimals = 1) |>
  gt_bucks_theme()

save_gt_table(databallr_playtype_table, "databallr_playtype_table")

# -------------------------------------------------------------------------
# 6. Draft prospects table
# -------------------------------------------------------------------------

draft_prospects_table <- draft_prospects_clean |>
  select(
    player, prospect_type, ppg, rpg, apg, tov,
    two_fg_pct, three_fg_pct, ft_pct, mock_pos
  ) |>
  gt() |>
  tab_header(
    title = md("**The Lottery Swings**"),
    subtitle = "Burries brings immediate impact and transition scoring; Ament is the higher-variance ceiling bet"
  ) |>
  cols_label(
    player = "Player",
    prospect_type = "Type",
    ppg = "PPG",
    rpg = "RPG",
    apg = "APG",
    tov = "TOV",
    two_fg_pct = "2FG%",
    three_fg_pct = "3FG%",
    ft_pct = "FT%",
    mock_pos = "Mock"
  ) |>
  fmt_number(columns = c(ppg, rpg, apg, tov), decimals = 1) |>
  fmt_percent(columns = c(two_fg_pct, three_fg_pct, ft_pct), decimals = 1) |>
  cols_width(
    player ~ px(150),
    prospect_type ~ px(190),
    ppg ~ px(65),
    rpg ~ px(65),
    apg ~ px(65),
    tov ~ px(65),
    two_fg_pct ~ px(75),
    three_fg_pct ~ px(75),
    ft_pct ~ px(75),
    mock_pos ~ px(170)
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = TEXT_DARK),
    locations = cells_body(columns = player)
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = BUCKS_GREEN),
    locations = cells_body(columns = prospect_type)
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = TEXT_DARK),
    locations = cells_column_labels(columns = everything())
  ) |>
  gt_bucks_theme()

save_gt_table(draft_prospects_table, "draft_prospects_table")

# -------------------------------------------------------------------------
# 7. Future pick control table
# -------------------------------------------------------------------------

appendix_future_pick_control_table <- future_picks_clean |>
  group_by(year, round) |>
  summarise(
    status = paste(unique(status), collapse = " / "),
    bucks_result = paste(unique(bucks_pick_result), collapse = "; "),
    incoming_from = paste(unique(na.omit(incoming_from[incoming_from != ""])), collapse = ", "),
    outgoing_to = paste(unique(na.omit(outgoing_to[outgoing_to != ""])), collapse = ", "),
    read = paste(unique(report_label), collapse = "; "),
    .groups = "drop"
  ) |>
  mutate(
    incoming_from = na_if(incoming_from, ""),
    outgoing_to = na_if(outgoing_to, "")
  ) |>
  arrange(year, round) |>
  gt(groupname_col = "year") |>
  tab_header(
    title = md("**Future Pick Control**"),
    subtitle = "The reset added Miami upside, but 2027-30 are still shaped by old obligations"
  ) |>
  cols_label(
    round = "Round",
    status = "Status",
    bucks_result = "Bucks Pick Result",
    incoming_from = "Incoming From",
    outgoing_to = "Outgoing To",
    read = "Article Read"
  ) |>
  cols_width(
    round ~ px(70),
    status ~ px(150),
    bucks_result ~ px(210),
    incoming_from ~ px(150),
    outgoing_to ~ px(150),
    read ~ px(330)
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = TEXT_DARK),
    locations = cells_column_labels(columns = everything())
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = BUCKS_GREEN),
    locations = cells_body(columns = round)
  ) |>
  sub_missing(columns = everything(), missing_text = "") |>
  gt_bucks_theme()

save_gt_table(appendix_future_pick_control_table, "appendix_future_pick_control_table")

# -------------------------------------------------------------------------
# 7A. Future pick control article table
# -------------------------------------------------------------------------
future_pick_article_table <- future_picks_clean |>
  mutate(
    year = as.integer(year),
    round = as.character(round)
  ) |>
  group_by(year) |>
  summarise(
    first_round_control = paste(
      unique(report_label[round == "1st"]),
      collapse = "; "
    ),
    second_round_control = paste(
      unique(report_label[round == "2nd"]),
      collapse = "; "
    ),
    .groups = "drop"
  ) |>
  mutate(
    first_round_control = na_if(first_round_control, ""),
    second_round_control = na_if(second_round_control, ""),
    article_read = case_when(
      year %in% 2027:2029 ~ "Old obligations still limit the bottom-out path",
      year == 2030 ~ "Miami upside starts to matter, but control is still partial",
      year %in% c(2031, 2033) ~ "Incoming Miami first plus own first creates opportunity",
      year == 2032 ~ "Clean own first, but no extra Miami first",
      TRUE ~ "Limited or unclear control"
    )
  ) |>
  arrange(year) |>
  gt() |>
  tab_header(
    title = md("**The Pick Situation Is Still Bleak**"),
    subtitle = "Miami adds upside, but Milwaukee remains without clear control of its own first until 2031"
  ) |>
  cols_label(
    year = "Year",
    first_round_control = "1st-Round Control",
    second_round_control = "2nd-Round Control",
    article_read = "Read"
  ) |>
  cols_width(
    year ~ px(80),
    first_round_control ~ px(300),
    second_round_control ~ px(220),
    article_read ~ px(360)
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = TEXT_DARK),
    locations = cells_body(columns = year)
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = BUCKS_GREEN),
    locations = cells_body(columns = first_round_control)
  ) |>
  tab_style(
    style = cell_text(color = TEXT_MID, size = px(13)),
    locations = cells_body(columns = article_read)
  ) |>
  sub_missing(columns = everything(), missing_text = "") |>
  gt_bucks_theme()

save_gt_table(future_pick_article_table, "future_pick_article_table")

# -------------------------------------------------------------------------
# 8. Reset paths table
# -------------------------------------------------------------------------

reset_paths_table <- tibble(
  path = c("Best case", "Good case", "Flat case", "Bad case"),
  what_has_to_happen = c(
    "Ware becomes a two-way frontcourt anchor, Ament's flashes become structure, Burries becomes a high-level two-way starter, and Kasparas hits as a connector/playmaker.",
    "Burries and Rollins give Milwaukee a credible two-way backcourt, Ware becomes a useful rim-protecting spacer, Ament is playable in smaller doses, and Kasparas becomes a rotation connector.",
    "Burries is solid, Ware flashes but never stabilizes, Ament remains more projection than production, and Kasparas is useful but capped athletically.",
    "Ament's feel and efficiency concerns win out, Ware's motor and processing habits harden, Burries lacks enough self-creation to scale, and Kasparas cannot separate athletically."
  ),
  article_read = c(
    "A real post-Giannis foundation emerges.",
    "A credible reset, even without a franchise player yet.",
    "Not hopeless, but missing the ceiling piece.",
    "The danger zone: young talent, but no clear engine."
  )
) |>
  gt() |>
  tab_header(
    title = md("**What the Reset Can Become**"),
    subtitle = "Milwaukee has multiple bets, but each comes with a different point of concern"
  ) |>
  cols_label(
    path = "Path",
    what_has_to_happen = "What Has to Happen",
    article_read = "Read"
  ) |>
  cols_width(
    path ~ px(120),
    what_has_to_happen ~ px(590),
    article_read ~ px(270)
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = TEXT_DARK),
    locations = cells_body(columns = path)
  ) |>
  tab_style(
    style = cell_text(weight = "bold", color = BUCKS_GREEN),
    locations = cells_body(columns = article_read)
  ) |>
  gt_bucks_theme()

save_gt_table(reset_paths_table, "reset_paths_table")

# -------------------------------------------------------------------------
# Appendix tables
# -------------------------------------------------------------------------

team_results_table <- bucks_team_results_clean |>
  select(
    season, wins, losses, win_pct, seed, playoff_result,
    ortg, drtg, net, srs, pace
  ) |>
  gt() |>
  tab_header(
    title = "Appendix: Bucks Year-by-Year Team Results",
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

save_gt_table(team_results_table, "appendix_team_results_table")

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
    title = "Appendix: Milwaukee's Window-Shaping Moves",
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

save_gt_table(transaction_summary_table, "appendix_transaction_summary_table")

# -------------------------------------------------------------------------
# Save table object list
# -------------------------------------------------------------------------

report_tables <- list(
  giannis_accomplishments_table = giannis_accomplishments_table,
  playoff_results_article_table = playoff_results_article_table,
  miami_return_profile_table = miami_return_profile_table,
  miami_return_metrics_table = miami_return_metrics_table,
  miami_shooting_table = miami_shooting_table,
  window_moves_table = window_moves_table,
  contract_context_table = contract_context_table,
  databallr_playtype_table = databallr_playtype_table,
  draft_prospects_table = draft_prospects_table,
  future_pick_article_table = future_pick_article_table,
  appendix_future_pick_control_table = appendix_future_pick_control_table,
  reset_paths_table = reset_paths_table,
  appendix_team_results_table = team_results_table,
  appendix_transaction_summary_table = transaction_summary_table
)

saveRDS(report_tables, file.path(dir_processed, "report_tables.rds"))

message("Curated summary tables built and saved to outputs/tables.")
