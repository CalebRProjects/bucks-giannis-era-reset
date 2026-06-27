# scripts/04_create_visuals.R

# Purpose:
# Create curated report-ready visuals and save them to outputs/figures.

source("scripts/02_clean_manual_data.R")

# -------------------------------------------------------------------------
# Remove outdated visuals that should not be used in the article
# -------------------------------------------------------------------------

old_visuals <- c(
  "giannis_accomplishments.png",
  "miami_return_skill_snapshot.png",
  "miami_return_shooting_profile.png",
  "miami_return_dpm_scatter.png",
  "new_core_role_map.png",
  "roster_age_summary.png",
  "playoff_results_timeline.png",
  "future_pick_control.png"
)

file.remove(file.path(dir_figures, old_visuals[file.exists(file.path(dir_figures, old_visuals))]))

# -------------------------------------------------------------------------
# 1. Giannis PRA production trend
# -------------------------------------------------------------------------

giannis_pra <- giannis_stats_clean |>
  mutate(
    pra = ppg + rpg + apg,
    label_point = case_when(
      season == "2013-14" ~ TRUE,
      season == "2018-19" ~ TRUE,
      season == "2019-20" ~ TRUE,
      season == "2020-21" ~ TRUE,
      season == "2025-26" ~ TRUE,
      TRUE ~ FALSE
    ),
    point_label = if_else(label_point, as.character(round(pra, 1)), NA_character_)
  )

p_giannis_production <- ggplot(
  giannis_pra,
  aes(x = season_start, y = pra)
) +
  annotate(
    "rect",
    xmin = 2017.5,
    xmax = 2020.5,
    ymin = -Inf,
    ymax = Inf,
    fill = BUCKS_GREEN,
    alpha = 0.06
  ) +
  geom_line(color = BUCKS_GREEN, linewidth = 1.15) +
  geom_point(color = BUCKS_GREEN, size = 3.1) +
  geom_point(
    data = giannis_pra |> filter(label_point),
    color = BUCKS_GREEN,
    size = 4
  ) +
  geom_text(
    aes(label = point_label),
    nudge_y = 2.0,
    size = 4.3,
    fontface = "bold",
    color = TEXT_DARK,
    na.rm = TRUE
  ) +
  annotate(
    "label",
    x = 2018,
    y = 62,
    label = "First MVP\n2018-19",
    size = 4.1,
    color = TEXT_DARK,
    fill = BG,
    label.size = 0,
    lineheight = 0.9
  ) +
  annotate(
    "label",
    x = 2020,
    y = 62,
    label = "Championship\n2020-21",
    size = 4.1,
    color = TEXT_DARK,
    fill = BG,
    label.size = 0,
    lineheight = 0.9
  ) +
  geom_segment(
    aes(x = 2018, xend = 2018, y = 58.8, yend = 51.5),
    linewidth = 0.5,
    linetype = "dashed",
    color = TEXT_LIGHT
  ) +
  geom_segment(
    aes(x = 2020, xend = 2020, y = 58.8, yend = 50.2),
    linewidth = 0.5,
    linetype = "dashed",
    color = TEXT_LIGHT
  ) +
  scale_x_continuous(
    breaks = giannis_pra$season_start[seq(1, nrow(giannis_pra), by = 2)],
    labels = giannis_pra$season[seq(1, nrow(giannis_pra), by = 2)]
  ) +
  scale_y_continuous(
    limits = c(15, 65),
    breaks = seq(20, 60, 10)
  ) +
  labs(
    title = "Giannis Became Milwaukee's Nightly Production Engine",
    subtitle = "Regular-season points + rebounds + assists per game across his Bucks tenure",
    x = NULL,
    y = "PRA per game",
    caption = "Source: Basketball Reference | Bucks career averages: 24.1 PPG, 9.9 RPG, 5.0 APG | @Rambzee_"
  ) +
  theme_caleb_elevated(grid = "y") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(12, 26, 16, 26)
  )

save_bucks_plot(
  p_giannis_production,
  "giannis_production_trend.png",
  width = 8.5,
  height = 5.4
)

# -------------------------------------------------------------------------
# 2. Bucks win percentage trend with record and seed
# -------------------------------------------------------------------------

win_plot_data <- bucks_team_results_clean |>
  mutate(
    article_phase = case_when(
      season_start <= 2017 ~ "Build-up",
      season_start <= 2023 ~ "Title-window",
      TRUE ~ "Window breaks"
    ),
    seed_label = case_when(
      is.na(seed) ~ "Missed",
      TRUE ~ paste0(seed, " seed")
    ),
    bar_label = paste0(wins, "-", losses, "\n", seed_label)
  )

p_win_pct <- ggplot(
  win_plot_data,
  aes(x = season_start, y = win_pct, fill = article_phase)
) +
  geom_col(width = 0.72) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    linewidth = 0.55,
    color = TEXT_LIGHT
  ) +
  geom_text(
    aes(label = bar_label),
    vjust = -0.35,
    size = 4.5,
    color = TEXT_DARK,
    fontface = "bold",
    lineheight = 0.4
  ) +
  scale_fill_manual(
    values = c(
      "Build-up" = "#D8C59A",
      "Title-window" = BUCKS_GREEN,
      "Window breaks" = "#9B2C2C"
    )
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(win_plot_data$win_pct, na.rm = TRUE) + 0.17),
    breaks = seq(0, 0.8, 0.2)
  ) +
  scale_x_continuous(
    breaks = win_plot_data$season_start,
    labels = win_plot_data$season
  ) +
  labs(
    title = "Giannis Turned Milwaukee Into a Yearly Contender",
    subtitle = "Regular-season results by season, with record and seed",
    x = NULL,
    y = "Win%",
    fill = NULL,
    caption = "Source: Basketball Reference manual research | Dashed line marks .500 | @Rambzee_"
  ) +
  theme_caleb_elevated(grid = "y", legend = "bottom") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text()
  )

save_bucks_plot(
  p_win_pct,
  "bucks_win_pct_trend.png",
  width = 9,
  height = 6
)

# -------------------------------------------------------------------------
# 3. Roster age distribution
# -------------------------------------------------------------------------

full_avg_age <- mean(current_roster_clean$age, na.rm = TRUE)

p_roster_age <- current_roster_clean |>
  mutate(
    player = fct_reorder(player, age),
    likely_movable = player %in% c("Kyle Kuzma", "Myles Turner", "Kevin Porter Jr.")
  ) |>
  ggplot(aes(x = age, y = player, fill = roster_bucket, alpha = likely_movable)) +
  geom_col(width = 0.72) +
  geom_vline(
    xintercept = full_avg_age,
    linetype = "dashed",
    linewidth = 0.65,
    color = TEXT_LIGHT
  ) +
  annotate(
    "text",
    x = full_avg_age + 0.25,
    y = 1.2,
    label = paste0("Avg: ", round(full_avg_age, 1)),
    hjust = 0,
    size = 4,
    color = TEXT_DARK
  ) +
  scale_fill_manual(values = ROSTER_COLORS) +
  scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.55), guide = "none") +
  labs(
    title = "Milwaukee Finally Has a Younger Timeline Again",
    subtitle = "Current roster age by player; likely movable veterans are faded",
    x = "Age",
    y = NULL,
    fill = NULL,
    caption = "Source: ESPN | @Rambzee_"
  ) +
  theme_caleb_elevated(grid = "x", legend = "bottom")

save_bucks_plot(
  p_roster_age,
  "roster_age_distribution.png",
  width = 8.5,
  height = 6.5
)

# -------------------------------------------------------------------------
# 4. Miami return playtype profile
# -------------------------------------------------------------------------

library(ggtext)

make_miami_playtype_plot <- function(players, file_name, title, subtitle) {

  player_context_lookup <- tibble::tibble(
    player = c(
      "Tyler Herro",
      "Jaime Jaquez Jr.",
      "Kel'el Ware",
      "Kasparas Jakucionis"
    ),
    player_display = c(
      "Tyler Herro",
      "Jaime Jaquez Jr.",
      "Kel&#39;el Ware",
      "Kasparas Jakucionis"
    ),
    player_context = c(
      "High-usage scoring guard",
      "Connector wing with interior value",
      "Toolsy big with stretch appeal",
      "Spacing-first combo guard prospect"
    )
  )

  facet_levels <- player_context_lookup |>
    filter(player %in% players) |>
    mutate(player = factor(player, levels = players)) |>
    arrange(player) |>
    mutate(
      facet_label = paste0(
        "<span style='font-size:30px; font-weight:800; color:#151515;'>",
        player_display,
        "</span><br>",
        "<span style='font-size:18px; font-weight:500; color:#666666;'>",
        player_context,
        "</span>"
      )
    ) |>
    pull(facet_label)

  plot_data <- databallr_playtypes_clean |>
    filter(player %in% players) |>
    left_join(player_context_lookup, by = "player") |>
    mutate(
      player = factor(player, levels = players),
      playtype = factor(
        playtype,
        levels = rev(c("Creation", "Spacing", "Transition", "Finishing"))
      ),
      freq_pct = freq,
      freq_label = paste0(round(freq_pct), "%"),
      rts_label = case_when(
        rts_impact > 0 ~ paste0("+", round(rts_impact, 1), " rTS"),
        TRUE ~ paste0(round(rts_impact, 1), " rTS")
      ),

      efficiency_bucket = case_when(
        rts_impact >= 7   ~ "Elite",
        rts_impact >= 4   ~ "Strong positive",
        rts_impact >= 1.5 ~ "Positive",
        rts_impact > -1.5 ~ "Neutral",
        rts_impact > -4   ~ "Negative",
        rts_impact > -7   ~ "Strong negative",
        TRUE              ~ "Severe negative"
      ),
      efficiency_bucket = factor(
        efficiency_bucket,
        levels = c(
          "Severe negative",
          "Strong negative",
          "Negative",
          "Neutral",
          "Positive",
          "Strong positive",
          "Elite"
        )
      ),

      facet_label = paste0(
        "<span style='font-size:30px; font-weight:800; color:#151515;'>",
        player_display,
        "</span><br>",
        "<span style='font-size:18px; font-weight:500; color:#666666;'>",
        player_context,
        "</span>"
      ),
      facet_label = factor(facet_label, levels = facet_levels),

      rts_x = freq_pct + 2.0,
      freq_x = pmax(freq_pct * 0.52, 2.7),

      freq_color = case_when(
        freq_pct < 9 ~ "white",
        efficiency_bucket == "Neutral" ~ "white",
        TRUE ~ "white"
      )
    )

  p <- ggplot(
    plot_data,
    aes(x = freq_pct, y = playtype)
  ) +
    geom_col(
      aes(fill = efficiency_bucket),
      width = 0.66,
      show.legend = FALSE
    ) +
    geom_text(
      aes(
        x = freq_x,
        label = freq_label,
        color = freq_color
      ),
      size = 5.4,
      fontface = "bold",
      show.legend = FALSE
    ) +
    geom_text(
      aes(
        x = rts_x,
        label = rts_label,
        color = efficiency_bucket
      ),
      hjust = 0,
      size = 5.2,
      fontface = "bold",
      show.legend = FALSE
    ) +
    facet_wrap(~ facet_label, ncol = 2) +
    scale_fill_manual(
      values = c(
        "Severe negative" = "#6F1111",
        "Strong negative" = "#9B2C2C",
        "Negative" = "#D78989",
        "Neutral" = "#b0a9a0",
        "Positive" = "#8FB98F",
        "Strong positive" = "#3F8F52",
        "Elite" = BUCKS_GREEN
      )
    ) +
    scale_color_manual(
      values = c(
        "Severe negative" = "#6F1111",
        "Strong negative" = "#9B2C2C",
        "Negative" = "#A94E4E",
        "Neutral" = TEXT_MID,
        "Positive" = "#4E8B57",
        "Strong positive" = "#2F7D43",
        "Elite" = BUCKS_GREEN,
        "white" = "white",
        TEXT_DARK = TEXT_DARK
      )
    ) +
    scale_x_continuous(
      limits = c(0, 64),
      breaks = seq(0, 60, 10),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Share of offensive shot volume",
      y = NULL,
      caption = "Source: DataBallr | @Rambzee_"
    ) +
    theme_caleb_elevated(grid = "x") +
    theme(
      legend.position = "none",

      plot.title = element_text(size = 30),
      plot.subtitle = element_text(size = 17),

      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 17, face = "bold"),
      axis.title.x = element_text(size = 15, margin = margin(t = 10)),

      strip.text = ggtext::element_markdown(
        size = 30,
        lineheight = 0.35,
        margin = margin(b = 10)
      ),

      panel.spacing = unit(1.6, "lines"),
      plot.caption = element_text(size = 11),
      plot.margin = margin(18, 40, 18, 30)
    )

  save_bucks_plot(
    p,
    file_name,
    width = 11,
    height = 6.05
  )

  p
}

p_miami_playtypes_guards <- make_miami_playtype_plot(
  players = c("Tyler Herro", "Jaime Jaquez Jr."),
  file_name = "miami_return_playtype_profile_herro_jaquez.png",
  title = "Herro and Jaquez Offer Very Different Offensive Bets",
  subtitle = "Bar length shows each playtype's share of shot volume; labels show playtype rTS"
)

p_miami_playtypes_young <- make_miami_playtype_plot(
  players = c("Kel'el Ware", "Kasparas Jakucionis"),
  file_name = "miami_return_playtype_profile_ware_kasparas.png",
  title = "Ware and Kasparas Show Two Different Development Paths",
  subtitle = "Bar length shows each playtype's share of shot volume; labels show playtype rTS"
)

# -------------------------------------------------------------------------
# 5. Simplified transaction timeline for appendix / article context
# -------------------------------------------------------------------------

library(ggrepel)

transaction_strip_data <- tibble::tribble(
  ~date,          ~label,                         ~phase,                ~label_side, ~label_nudge_days,
  "2013-06-27",  "Giannis draft",                 "Build the core",       1,           -45,
  "2013-07-31",  "Middleton acquired",            "Build the core",      -1,            55,
  "2017-11-07",  "Bledsoe trade",                 "Build the core",      -1,             0,
  "2018-07-17",  "Lopez signing",                 "Build the core",       1,             0,
  "2020-11-21",  "Portis signing",                "Spend for the title",  1,           -45,
  "2020-11-23",  "Jrue trade",                    "Spend for the title", -1,            45,
  "2023-09-27",  "Dame trade",                    "Extend, then reset",  -1,             0,
  "2025-02-07",  "Middleton/Kuzma pivot",         "Extend, then reset",   1,             0,
  "2025-07-01",  "Dame waived\nTurner signed",    "Extend, then reset",  -1,             0,
  "2026-06-22",  "Giannis trade",                 "Extend, then reset",   1,           -65,
  "2026-06-23",  "Burries/Ament",                 "Extend, then reset",  -1,            65
) |>
  mutate(
    date = ymd(date),
    label_date = date + days(label_nudge_days),
    y = 0,
    label_y = label_side * 0.34,
    stem_y = label_side * 0.13,
    phase = factor(
      phase,
      levels = c("Build the core", "Spend for the title", "Extend, then reset")
    )
  )

p_transaction_timeline <- ggplot(
  transaction_strip_data,
  aes(x = date, y = y)
) +
  geom_hline(
    yintercept = 0,
    color = GRID,
    linewidth = 1.8
  ) +
  geom_segment(
    aes(
      x = date,
      xend = label_date,
      y = stem_y,
      yend = label_y - label_side * 0.035,
      color = phase
    ),
    linewidth = 0.65,
    alpha = 0.8,
    show.legend = FALSE
  ) +
  geom_point(
    aes(fill = phase),
    shape = 21,
    size = 5.7,
    color = BG,
    stroke = 1.15,
    show.legend = FALSE
  ) +
  geom_label(
    aes(
      x = label_date,
      y = label_y,
      label = label,
      color = phase
    ),
    fill = BG,
    label.size = 0,
    size = 4.0,
    fontface = "bold",
    lineheight = 0.9,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "Build the core" = BUCKS_BLUE,
      "Spend for the title" = BUCKS_GREEN,
      "Extend, then reset" = "#9B2C2C"
    )
  ) +
  scale_color_manual(
    values = c(
      "Build the core" = BUCKS_BLUE,
      "Spend for the title" = BUCKS_GREEN,
      "Extend, then reset" = "#9B2C2C"
    )
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    limits = c(ymd("2012-11-01"), ymd("2027-02-01")),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    limits = c(-0.58, 0.58),
    breaks = NULL
  ) +
  labs(
    title = "The Moves That Built, Extended, and Ended the Window",
    subtitle = "Milwaukee's Giannis era moved from core-building, to title spending, to a forced reset",
    x = NULL,
    y = NULL,
    caption = "Source: Manual transaction research | Blue = core-building, green = title spending, red = extension/reset moves | Viz: Caleb Ramsey"
  ) +
  theme_caleb_elevated(grid = "x", legend = "none") +
  theme(
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9.5),
    plot.margin = margin(16, 30, 16, 30)
  )

save_bucks_plot(
  p_transaction_timeline,
  "transaction_timeline.png",
  width = 10.5,
  height = 3.8
)

message("Curated visuals created and saved to outputs/figures.")
