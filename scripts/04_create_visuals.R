# scripts/04_create_visuals.R

# Purpose:
# Create report-ready visuals and save them to outputs/figures.

source("scripts/02_clean_manual_data.R")

# -------------------------------------------------------------------------
# 1. Giannis production trend
# -------------------------------------------------------------------------

giannis_production_long <- giannis_stats_clean |>
  select(season, season_start, ppg, rpg, apg) |>
  pivot_longer(
    cols = c(ppg, rpg, apg),
    names_to = "stat",
    values_to = "value"
  ) |>
  mutate(
    stat = recode(
      stat,
      ppg = "Points",
      rpg = "Rebounds",
      apg = "Assists"
    )
  )

p_giannis_production <- ggplot(
  giannis_production_long,
  aes(x = season_start, y = value, color = stat)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.4) +
  scale_color_manual(
    values = c(
      "Points" = BUCKS_GREEN,
      "Rebounds" = "#B88746",
      "Assists" = BUCKS_BLUE
    )
  ) +
  scale_x_continuous(
    breaks = giannis_stats_clean$season_start,
    labels = giannis_stats_clean$season
  ) +
  labs(
    title = "Giannis Grew From Prospect to System",
    subtitle = "Regular-season points, rebounds, and assists per game across his Milwaukee years",
    x = NULL,
    y = "Per-game average",
    color = NULL,
    caption = "Source: Basketball Reference manual research | Viz: Caleb Ramsey"
  ) +
  theme_caleb_elevated(grid = "y", legend = "bottom") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text()
  )

save_bucks_plot(
  p_giannis_production,
  "giannis_production_trend.png",
  width = 10,
  height = 6
)

# -------------------------------------------------------------------------
# 2. Bucks win percentage trend
# -------------------------------------------------------------------------

p_win_pct <- ggplot(
  bucks_team_results_clean,
  aes(x = season_start, y = win_pct, fill = era_phase)
) +
  geom_col(width = 0.72) +
  geom_hline(yintercept = 0.5, linetype = "dashed", linewidth = 0.45, color = TEXT_LIGHT) +
  scale_fill_manual(values = ERA_COLORS) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(
    breaks = bucks_team_results_clean$season_start,
    labels = bucks_team_results_clean$season
  ) +
  labs(
    title = "The Giannis Era Raised Milwaukee's Baseline",
    subtitle = "Regular-season win percentage by season",
    x = NULL,
    y = "Win percentage",
    caption = "Source: Basketball Reference manual research | Viz: Caleb Ramsey"
  ) +
  theme_caleb_elevated(grid = "y") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text()
  )

save_bucks_plot(
  p_win_pct,
  "bucks_win_pct_trend.png",
  width = 10,
  height = 6
)

# -------------------------------------------------------------------------
# 3. Team rating trend
# -------------------------------------------------------------------------

team_ratings_long <- bucks_team_results_clean |>
  select(season, season_start, ortg, drtg, net) |>
  pivot_longer(
    cols = c(ortg, drtg, net),
    names_to = "rating_type",
    values_to = "rating"
  ) |>
  mutate(
    rating_type = recode(
      rating_type,
      ortg = "Offensive Rating",
      drtg = "Defensive Rating",
      net = "Net Rating"
    )
  )

p_team_ratings <- ggplot(
  team_ratings_long,
  aes(x = season_start, y = rating, color = rating_type)
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  scale_color_manual(
    values = c(
      "Offensive Rating" = BUCKS_GREEN,
      "Defensive Rating" = "#9B2C2C",
      "Net Rating" = BUCKS_BLUE
    )
  ) +
  scale_x_continuous(
    breaks = bucks_team_results_clean$season_start,
    labels = bucks_team_results_clean$season
  ) +
  labs(
    title = "Milwaukee's Team Profile Shifted Over Time",
    subtitle = "Offensive rating, defensive rating, and net rating by season",
    x = NULL,
    y = "Rating",
    color = NULL,
    caption = "Source: Basketball Reference manual research | Viz: Caleb Ramsey"
  ) +
  theme_caleb_elevated(grid = "y", legend = "bottom") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text()
  )

save_bucks_plot(
  p_team_ratings,
  "bucks_team_ratings_trend.png",
  width = 10,
  height = 6
)

# -------------------------------------------------------------------------
# 4. Playoff results timeline
# -------------------------------------------------------------------------

p_playoff_timeline <- ggplot(
  bucks_playoffs_clean,
  aes(x = season_start, y = playoff_level)
) +
  geom_line(color = BUCKS_GREEN, linewidth = 1.1) +
  geom_point(size = 3.2, color = BUCKS_GREEN) +
  geom_text(
    aes(label = result),
    nudge_y = 0.22,
    size = 3.3,
    color = TEXT_DARK
  ) +
  scale_x_continuous(
    breaks = bucks_playoffs_clean$season_start,
    labels = bucks_playoffs_clean$season
  ) +
  scale_y_continuous(
    breaks = c(1, 2, 3, 4),
    labels = c("Round 1", "Semis", "ECF", "Finals")
  ) +
  labs(
    title = "The Playoff Ceiling Rose, Then Flattened",
    subtitle = "Deepest playoff round reached by season",
    x = NULL,
    y = NULL,
    caption = "Source: Basketball Reference manual research | Viz: Caleb Ramsey"
  ) +
  theme_caleb_elevated(grid = "y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_bucks_plot(
  p_playoff_timeline,
  "playoff_results_timeline.png",
  width = 10,
  height = 6
)

# -------------------------------------------------------------------------
# 5. Current roster age distribution
# -------------------------------------------------------------------------

p_roster_age <- current_roster_clean |>
  mutate(player = fct_reorder(player, age)) |>
  ggplot(aes(x = age, y = player, fill = roster_bucket)) +
  geom_col(width = 0.72) +
  scale_fill_manual(values = ROSTER_COLORS) +
  labs(
    title = "The Post-Giannis Roster Is Younger, But Not Empty",
    subtitle = "Current roster age by player and roster bucket",
    x = "Age",
    y = NULL,
    fill = NULL,
    caption = "Source: ESPN/manual roster research | Viz: Caleb Ramsey"
  ) +
  theme_caleb_elevated(grid = "x", legend = "bottom")

save_bucks_plot(
  p_roster_age,
  "roster_age_distribution.png",
  width = 9,
  height = 7
)

# -------------------------------------------------------------------------
# 6. New core role map
# -------------------------------------------------------------------------

p_new_core <- new_core_clean |>
  filter(!is.na(age), !is.na(ppg)) |>
  ggplot(
    aes(
      x = age,
      y = ppg,
      size = replace_na(mpg, 24),
      color = roster_bucket,
      label = player
    )
  ) +
  geom_point(alpha = 0.85) +
  geom_text(
    nudge_y = 0.7,
    size = 3.1,
    color = TEXT_DARK,
    check_overlap = TRUE
  ) +
  scale_color_manual(values = ROSTER_COLORS) +
  scale_size_continuous(range = c(3, 9), guide = "none") +
  labs(
    title = "Milwaukee's New Core Is More Optional Than Proven",
    subtitle = "Age, scoring production, and roster source for young and bridge pieces",
    x = "Age",
    y = "Points per game",
    color = NULL,
    caption = "Source: Manual research from public data | Viz: Caleb Ramsey"
  ) +
  theme_caleb_elevated(grid = "both", legend = "bottom") +
  theme(axis.title.y = element_text())

save_bucks_plot(
  p_new_core,
  "new_core_role_map.png",
  width = 9,
  height = 6
)

# -------------------------------------------------------------------------
# 7. Transaction timeline
# -------------------------------------------------------------------------

p_transaction_timeline <- transaction_timeline_clean |>
  mutate(
    y = case_when(
      timeline_group == "Draft" ~ 3,
      timeline_group == "Trade" ~ 2,
      timeline_group == "Roster move" ~ 1,
      TRUE ~ 0
    )
  ) |>
  ggplot(aes(x = date, y = y, color = timeline_group)) +
  geom_line(color = GRID, linewidth = 0.6) +
  geom_point(size = 3.4) +
  geom_text(
    aes(label = transaction_type),
    nudge_y = 0.18,
    size = 3,
    color = TEXT_DARK,
    check_overlap = TRUE
  ) +
  scale_color_manual(
    values = c(
      "Draft" = BUCKS_BLUE,
      "Trade" = BUCKS_GREEN,
      "Roster move" = "#B88746",
      "Other" = NEUTRAL
    )
  ) +
  scale_y_continuous(
    breaks = c(1, 2, 3),
    labels = c("Roster move", "Trade", "Draft")
  ) +
  labs(
    title = "The Moves That Built, Extended, and Ended the Window",
    subtitle = "Major roster decisions from the Giannis draft through the post-Giannis reset",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: Manual transaction research | Viz: Caleb Ramsey"
  ) +
  theme_caleb_elevated(grid = "x", legend = "bottom")

save_bucks_plot(
  p_transaction_timeline,
  "transaction_timeline.png",
  width = 11,
  height = 6
)

# -------------------------------------------------------------------------
# 8. Future pick control chart
# -------------------------------------------------------------------------

p_future_picks <- future_picks_clean |>
  count(year, pick_bucket) |>
  ggplot(aes(x = factor(year), y = n, fill = pick_bucket)) +
  geom_col(width = 0.72) +
  scale_fill_manual(values = PICK_COLORS) +
  labs(
    title = "The Reset Added Picks, But Not Full Control",
    subtitle = "Future pick rows by year and control bucket",
    x = NULL,
    y = "Pick rows",
    fill = NULL,
    caption = "Source: RealGM/manual pick research | Viz: Caleb Ramsey"
  ) +
  theme_caleb_elevated(grid = "y", legend = "bottom") +
  theme(axis.title.y = element_text())

save_bucks_plot(
  p_future_picks,
  "future_pick_control.png",
  width = 9,
  height = 6
)

message("Visuals created and saved to outputs/figures.")
