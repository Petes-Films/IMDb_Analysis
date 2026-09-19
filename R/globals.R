# R/globals.R
options(stringsAsFactors = FALSE)

# Core libraries
library(tidyverse)
library(janitor)
library(knitr)
library(kableExtra)
library(plotly)
library(tidytext)
library(gt)
library(gtsummary)
library(reshape2)
library(ggrepel)
library(ggridges)
library(shiny)
library(zoo)
library(here)
library(purrr)

# knitr defaults (used by the report)
knitr::opts_chunk$set(echo = T, fig.align = "center", warning = F, message = F)

# Standard default Variables
Defaults <- list(
  min_films = 5,
  min_genre_films = 100,
  min_votes = 100,
  score_limits = c(1, 10),
  ex_dif = 1,
  no_IMDb_rating = 1
)

# decade converter function
to_decade <- function(year) floor(year / 10) * 10

# House Style for more consistent formatting of tables and charts
# ggplot theme
theme_set(
  theme_classic() +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      #    axis.title = element_blank(),
      legend.title=element_blank()
    )
)

# second theme for blank titles and angled labelling?

# gt table theme
my_gt_theme <- function(gt_tbl) {
  n_rows <- nrow(gt_tbl$`_data`)
  gt_tbl |>
    tab_options(
      data_row.padding = px(6),
      heading.align = 'left',
      column_labels.background.color = '#F3CE13',
      heading.title.font.size = px(26),
      heading.subtitle.font.size = px(14),
      table_body.hlines.width = px(0)
    ) |>
    tab_style(
      style = cell_text(color = '#0c31ec', weight = 'bold'),
      locations = cells_title(groups = 'title')
    ) |>
    tab_style(
      style = cell_fill(color = 'grey90'),
      locations = cells_body(rows = seq(1, n_rows, 2))
    )
}

# Colour schemes for charts
# Main one colour for graphs
base_col <- "darkgreen"

# Colour gradien tstyle for variation from norm
col_change <- list(
  negative = "tomato",
  neutral  = "grey85",
  positive = "steelblue"
)

# Colour scale options for ggplot with additional functionality
scale_change <- function(midpoint = 0, col_limits = NULL) {
  scale_colour_gradient2(
    low = col_change$negative,
    mid = col_change$neutral,
    high = col_change$positive,
    midpoint = midpoint,
    limits = col_limits, # check I haven't messed this up!
    guide = "none"
  )
}

# Reference lines for chart style
ref_col <- "grey50" # or grey 60
ref_line <- "dashed"

# Normal chart transparency level
ref_alpha <- 0.7
