# R/data/data_collector.R

pacman::p_load(tidyverse, lubridate, yaml, glue, fs, httr, jsonlite)
source(here::here("R", "data", "api_client.R"))

#' Collect data for a specific month
collect_monthly_data <- function(year, month, category, region, params) {
  date_obj <- ymd(paste0(year, "-", month, "-01"))
  next_month_date <- date_obj %m+% months(1)
  
  # Use actual current date for the last month's end date
  current_date <- as_date("2024-02-23")  # Fixed date for consistency
  if (year == year(current_date) && month == month(current_date)) {
    next_month_date <- current_date
  }
  
  # Skip future dates
  if (date_obj > current_date) {
    return(NULL)
  }
  
  message(glue("\nProcessing {month} {year}"))
  data <- fetch_eirgrid_data(
    category = category,
    region = region,
    date_from = date_obj,
    date_to = next_month_date,
    params = params
  )
  
  return(data)
}

#' Collect all data for a specific year
collect_yearly_data <- function(year, category, region, params) {
  message(glue("\nCollecting data for year {year}"))
  
  # Check if file exists and skip if not current year
  filepath <- file.path(
    params$paths$base_dir,
    glue("{region}_{category}_{year}_Eirgrid.csv")
  )
  
  current_year <- 2024  # Fixed for environment
  
  if (file.exists(filepath) && year != current_year) {
    message(glue("File already exists for {year}, skipping..."))
    return(NULL)
  }
  
  # Collect data month by month
  all_data <- list()
  for (month in month.abb) {
    data <- collect_monthly_data(year, month, category, region, params)
    if (!is.null(data)) {
      all_data[[month]] <- data
    }
  }
  
  if (length(all_data) > 0) {
    final_df <- bind_rows(all_data)
    dir_create(dirname(filepath), recurse = TRUE)
    write_csv(final_df, filepath)
    message(glue("Saved {nrow(final_df)} rows to {basename(filepath)}"))
  }
}

#' Main data collection function
collect_eirgrid_data <- function(params = NULL) {
  if (is.null(params)) {
    params <- yaml::read_yaml(here::here("config", "parameters.yaml"))
  }
  
  # Get enabled categories
  categories <- params$data$categories %>%
    keep(~ .$enabled) %>%
    map_chr(~ .$name)
  
  # Process each category
  for (category in categories) {
    message(glue("\nProcessing category: {category}"))
    start_year <- year(as_date(params$data$first_date))
    end_year <- 2024  # Fixed for environment
    
    for (year in start_year:end_year) {
      collect_yearly_data(year, category, params$data$default_region, params)
    }
  }
}