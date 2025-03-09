# dependencies.R

# Install pacman if not already installed
if (!require("pacman")) install.packages("pacman")

# Load and install packages using pacman
pacman::p_load(
  tidyverse,
  tidymodels,
  httr,
  jsonlite,
  tictoc,
  timetk,
  glue,
  logger,
  future,
  future.apply,
  yaml,
  fs,
  lubridate,
  gt,
  pins,
  vetiver,
  plumber,
  conflicted,
  character.only = FALSE,
  install = TRUE,
  update = FALSE
)

tidymodels::tidymodels_prefer()

# Resolve package conflicts
conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::filter)

options(tidymodels.dark = TRUE)
