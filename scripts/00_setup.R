# scripts/00_setup.R

# Purpose:
# Shared setup for the Milwaukee Bucks post-Giannis reset report.
# Loads packages, defines folders, creates helpers, and sets the visual theme.

# -------------------------------------------------------------------------
# Packages
# -------------------------------------------------------------------------

required_packages <- c(
  "tidyverse",
  "janitor",
  "readr",
  "stringr",
  "lubridate",
  "glue",
  "scales",
  "gt",
  "ggplot2",
  "showtext"
)

installed_packages <- rownames(installed.packages())
missing_packages <- required_packages[!required_packages %in% installed_packages]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))

# -------------------------------------------------------------------------
# Project paths
# -------------------------------------------------------------------------

dir_manual <- "data/manual"
dir_processed <- "data/processed"
dir_figures <- "outputs/figures"
dir_tables <- "outputs/tables"
dir_report <- "report"

dirs <- c(
  dir_manual,
  dir_processed,
  dir_figures,
  dir_tables,
  dir_report
)

walk(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------------
# Fonts
# -------------------------------------------------------------------------
# If Inter is installed locally, ggplot will use it.
# If not, change base_family in theme_caleb_elevated() to "Arial".

showtext_auto()

# -------------------------------------------------------------------------
# Visual identity
# -------------------------------------------------------------------------

BG <- "#F6F4EF"
TEXT_DARK <- "#151515"
TEXT_MID <- "#333333"
TEXT_LIGHT <- "#666666"
GRID <- "#DDD8CF"

BUCKS_GREEN <- "#00471B"
BUCKS_CREAM <- "#EEE1C6"
BUCKS_BLUE <- "#0077C0"
BUCKS_BLACK <- "#000000"

POSITIVE <- BUCKS_GREEN
NEGATIVE <- "#9B2C2C"
NEUTRAL <- "#7A746A"

ERA_COLORS <- c(
  "Development" = BUCKS_CREAM,
  "MVP rise" = BUCKS_BLUE,
  "Peak/title window" = BUCKS_GREEN,
  "Post-title chase" = "#6F6259",
  "Final Milwaukee years" = "#9B2C2C"
)

PICK_COLORS <- c(
  "Incoming" = BUCKS_GREEN,
  "Retained" = BUCKS_BLUE,
  "Swap/limited control" = "#B88746",
  "Outgoing" = "#9B2C2C",
  "Other" = NEUTRAL
)

ROSTER_COLORS <- c(
  "Trade return" = BUCKS_GREEN,
  "2026 draft" = BUCKS_BLUE,
  "Young/bridge piece" = "#B88746",
  "Veteran" = "#6F6259"
)

# -------------------------------------------------------------------------
# ggplot theme
# -------------------------------------------------------------------------

theme_caleb_elevated <- function(
    base_family = "Inter",
    base_size = 12,
    grid = "x",
    legend = "none"
) {
  major_x <- if (grid %in% c("x", "both")) {
    element_line(color = GRID, linewidth = 0.35)
  } else {
    element_blank()
  }

  major_y <- if (grid %in% c("y", "both")) {
    element_line(color = GRID, linewidth = 0.35)
  } else {
    element_blank()
  }

  theme_minimal(base_family = base_family, base_size = base_size) +
    theme(
      plot.background = element_rect(fill = BG, color = NA),
      panel.background = element_rect(fill = BG, color = NA),

      panel.grid.major.x = major_x,
      panel.grid.major.y = major_y,
      panel.grid.minor = element_blank(),

      plot.title = element_text(
        size = 20,
        face = "bold",
        hjust = 0.5,
        color = TEXT_DARK,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        size = 11,
        hjust = 0.5,
        color = TEXT_MID,
        margin = margin(b = 12)
      ),
      plot.caption = element_text(
        size = 8.5,
        color = TEXT_LIGHT,
        hjust = 1,
        margin = margin(t = 8)
      ),

      axis.title.x = element_text(
        size = 12,
        face = "bold",
        color = TEXT_DARK,
        margin = margin(t = 8)
      ),
      axis.title.y = element_text(
        size = 12,
        face = "bold",
        color = TEXT_DARK,
        margin = margin(r = 8)
      ),

      axis.text.x = element_text(size = 10, color = "#4A4A4A"),
      axis.text.y = element_text(size = 11, face = "bold", color = "#4A4A4A"),

      legend.position = legend,
      legend.title = element_text(face = "bold", color = TEXT_DARK),
      legend.text = element_text(color = TEXT_MID),

      plot.margin = margin(12, 32, 14, 14)
    )
}

# -------------------------------------------------------------------------
# General helpers
# -------------------------------------------------------------------------

parse_pct <- function(x) {
  x <- as.character(x)

  out <- x |>
    str_replace_all("%", "") |>
    str_trim() |>
    na_if("") |>
    as.numeric()

  ifelse(!is.na(out) & out > 1, out / 100, out)
}

parse_money_millions <- function(x) {
  x <- as.character(x)

  case_when(
    is.na(x) ~ NA_real_,
    str_detect(str_to_lower(x), "million") ~
      str_extract(x, "[0-9.]+") |> as.numeric(),
    str_detect(x, "\\$") ~
      str_replace_all(x, "[$,]", "") |> as.numeric() / 1e6,
    TRUE ~ suppressWarnings(as.numeric(x))
  )
}

season_start_year <- function(season) {
  str_sub(as.character(season), 1, 4) |> as.integer()
}

save_processed <- function(data, file_name) {
  write_csv(data, file.path(dir_processed, file_name))
}

save_bucks_plot <- function(plot, filename, width = 9, height = 6, dpi = 320) {
  ggsave(
    filename = file.path(dir_figures, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = BG
  )
}

clean_text <- function(x) {
  x |>
    as.character() |>
    str_squish() |>
    na_if("")
}

# -------------------------------------------------------------------------
# Table helper
# -------------------------------------------------------------------------

gt_bucks_theme <- function(gt_tbl) {
  gt_tbl |>
    tab_options(
      table.background.color = BG,
      heading.background.color = BG,
      column_labels.background.color = BG,
      table.font.names = "Inter",
      table.font.size = px(13),
      heading.title.font.size = px(18),
      heading.subtitle.font.size = px(12),
      data_row.padding = px(6),
      table.border.top.color = GRID,
      table.border.bottom.color = GRID,
      column_labels.border.top.color = GRID,
      column_labels.border.bottom.color = GRID
    ) |>
    tab_style(
      style = cell_text(weight = "bold", color = TEXT_DARK),
      locations = cells_column_labels()
    )
}

message("Setup complete.")
