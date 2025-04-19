# INSTALL PACKAGES --------------------------------------------------------

options(scipen = 999)

# INSTALL PACMAN IF NOT ALREADY INSTALLED
if (!require("pacman")) install.packages("pacman")

#remotes::install_github("business-science/modeltime.gluonts")

#modeltime.gluonts::install_gluonts()

# Load and install packages using pacman
pacman::p_load(
  forecast,
  tidyverse,
  tidymodels,
  modeltime.gluonts,
  moments,
  readr,
  vip,
  googleAuthR,
  googleCloudRunner,
  googleCloudStorageR,
  pins,
  tune,
  finetune,
  jsonlite,
  tictoc,
  timetk,
  glue,
  logger,
  future,
  future.apply,
  yaml,
  ranger,
  modeltime,
  pins,
  vetiver,
  fs,
  lubridate,
  gt,
  pins,
  vetiver,
  plumber,
  httr,
  purrr,
  jsonlite,
  conflicted,
  progress,
  character.only = FALSE,
  install = TRUE,
  update = FALSE
)

tidymodels::tidymodels_prefer()

# Resolve package conflicts
conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::filter)
conflicts_prefer(purrr::flatten)

options(tidymodels.dark = TRUE)


# GET DATA ----------------------------------------------------------------

# START TIMER
start_time <- Sys.time()

# SETUP LOGGING
log_info <- function(msg) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- INFO -", msg, "\n")
log_error <- function(msg) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- ERROR -", msg, "\n")

# MAIN FUNCTION TO GET HISTORIC DATA
get_historic_data <- function() {
  region <- "ALL"
  category <- "demandactual"
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  final_year <- current_year - 1999
  
  # CREATE BASE DIRECTORY IF IT DOESN'T EXIST
  csv_dir <- file.path("Downloaded_Data", region)
  dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
  
  # CHECK EXISTING FILES TO DETERMINE WHICH YEARS NEED UPDATING
  existing_files <- list.files(csv_dir, pattern = paste0(region, "_", category, ".*_Eirgrid.csv"))
  
  # EXTRACT YEARS FROM EXISTING FILES
  existing_years <- integer(0)
  if (length(existing_files) > 0) {
    for (file in existing_files) {
      # EXTRACT YEAR FROM FILENAME
      year_match <- regmatches(file, regexpr("_[0-9]{2}_", file))
      if (length(year_match) > 0) {
        year_str <- gsub("_", "", year_match)
        existing_years <- c(existing_years, as.integer(year_str))
      }
    }
  }
  
  # DETERMINE WHICH YEARS TO DOWNLOAD
  years_to_download <- if (length(existing_years) > 0) {
    # IF WE HAVE EXISTING FILES, ONLY DOWNLOAD THE CURRENT YEAR
    final_year
  } else {
    # OTHERWISE, DOWNLOAD ALL YEARS FROM 2014 ONWARDS
    14:final_year
  }
  
  log_info(paste("Will download data for year(s):", paste(years_to_download, collapse = ", ")))
  
  for (year in years_to_download) {
    frames <- list()
    
    # FOR CURRENT YEAR, WE'LL REDOWNLOAD ALL MONTHS TO ENSURE LATEST DATA
    current_year_flag <- (year == final_year)
    
    for (month_idx in 1:length(months)) {
      month <- months[month_idx]
      next_month <- if(month_idx == 12) "Jan" else months[month_idx + 1]
      next_year <- if(month_idx == 12) year + 1 else year
      
      # SKIP FUTURE MONTHS IN CURRENT YEAR
      if (current_year_flag && month_idx > as.integer(format(Sys.Date(), "%m"))) {
        log_info(paste("Skipping future month:", month, year))
        next
      }
      
      api_url <- paste0(
        "https://www.smartgriddashboard.com/DashboardService.svc/data?area=",
        category, "&region=", region,
        "&datefrom=01-", month, "-20", year, "+00%3A00",
        "&dateto=01-", next_month, "-20", next_year, "+21%3A59"
      )
      
      tryCatch({
        response <- httr::GET(api_url, httr::timeout(60))
        result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$Rows
        
        if (!is.null(result) && length(result) > 0) {
          frames[[length(frames) + 1]] <- as.data.frame(result)
          log_info(paste("Download of", region, month, year, "Eirgrid", "demandactual", "Data was successful."))
        } else {
          log_info(paste("No data returned for", region, month, year))
        }
      }, error = function(e) {
        log_error(paste("Failed to download data for", region, month, year, category, ":", e$message))
      })
    }
    
    if (length(frames) > 0) {
      final_df <- do.call(rbind, frames)
      csv_file <- file.path(csv_dir, paste0(region, "_", category, "_", year, "_Eirgrid.csv"))
      write.csv(final_df, csv_file, row.names = FALSE, quote = FALSE)
      log_info(paste("CSV for", region, year, "Eirgrid", category, "Data save was successful."))
    } else {
      log_error(paste("No data frames to combine for", year, "- file not created."))
    }
  }
  
  # END TIMER AND REPORT
  end_time <- Sys.time()
  total_seconds <- as.numeric(difftime(end_time, start_time, units = "secs"))
  log_info(paste("This script took approx.", round(total_seconds, 2), "seconds to complete."))
  log_info("********************************************************")
}

# ENSURE REQUIRED PACKAGES ARE LOADED
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
library(httr)
library(jsonlite)

# RUN THE SCRIPT
get_historic_data()

# FUNCTION TO COMBINE DEMAND ACTUAL CSV FILES INTO A SINGLE FILE
combine_demand_files <- function() {
  # SET DIRECTORY PATH
  data_dir <- "Downloaded_Data/ALL"
  
  # GET CURRENT TIMESTAMP
  current_time <- Sys.time()
  
  # SETUP LOGGING
  log_info <- function(msg) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- INFO -", msg, "\n")
  log_error <- function(msg) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- ERROR -", msg, "\n")

  # LIST ALL DEMANDACTUAL FILES
  demand_files <- list.files(
    path = data_dir,
    pattern = "ALL_demandactual.*\\.csv$",
    full.names = TRUE
  )
  
  log_info(paste("Found", length(demand_files), "demand files to combine"))
  
  # FUNCTION TO READ AND PROCESS A SINGLE FILE
  read_demand_file <- function(file_path) {
    tryCatch({
      log_info(paste("Processing file:", basename(file_path)))
      
      # READ FILE
      df <- read_csv(
        file_path,
        col_names = c("EffectiveTime", "FieldName", "Region", "Value"),
        col_types = cols(),
        show_col_types = FALSE
      )
      
      # PROCESS DATA
      processed_df <- df %>%
        mutate(
          time_stamp = parse_date_time(EffectiveTime, orders = c("dmY HMS", "Ymd HMS")),
          demand = as.numeric(Value),
          file_source = basename(file_path)
        ) %>%
        select(time_stamp, demand, file_source) %>%
        filter(!is.na(time_stamp), time_stamp <= current_time)
      
      log_info(paste("  Found", nrow(processed_df), "valid records"))
      return(processed_df)
      
    }, error = function(e) {
      log_error(paste("Error processing file:", basename(file_path), "Error:", e$message))
      return(NULL)
    })
  }
  
  # COMBINE ALL FILES USING MAP_DFR
  log_info("Combining files...")
  combined_demand <- map_dfr(demand_files, read_demand_file)
  
  # SORT BY TIMESTAMP AND REMOVE DUPLICATES
  log_info("Sorting data and removing duplicates...")
  if (nrow(combined_demand) > 0) {
    combined_demand <- combined_demand %>%
      arrange(time_stamp) %>%
      distinct(time_stamp, .keep_all = TRUE)
    
    # SAVE COMBINED FILE
    output_file <- file.path(data_dir, "combined_demandactual.csv")
    write_csv(combined_demand, output_file)
    log_info(paste("Combined data saved to:", output_file))
    log_info(paste("Total records:", nrow(combined_demand)))
    log_info(paste("Date range:", min(combined_demand$time_stamp), "to", max(combined_demand$time_stamp)))
  } else {
    log_error("No data to combine!")
  }
}

# RUN THE FUNCTION
combine_demand_files()

# READ THE COMBINED DEMAND CSV FILE
combined_file <- "Downloaded_Data/ALL/combined_demandactual.csv"

# CHECK IF FILE EXISTS
if (!file.exists(combined_file)) {
  stop("Combined file not found: ", combined_file)
}

# READ THE COMBINED FILE
combined_demand <- readr::read_csv(
  combined_file,
  col_types = readr::cols(
    time_stamp = readr::col_datetime(),
    demand = readr::col_double(),
    file_source = readr::col_character()
  ),
  show_col_types = FALSE
) %>% 
  select(-file_source) %>% 
  dplyr::arrange(time_stamp) #%>% 
  #timetk::future_frame(.date_var = time_stamp, .length_out = "1 month", .bind_data = TRUE)

# CONVERT 15 MIN DATA TO DAILY AND PERFORM FEATURE ENGINEERING RATHER THAN IN
# A RECIPE SINCE DROP NA IS PROBLEMATIC
daily_demand <- combined_demand %>%
  pad_by_time(time_stamp, .by = "15 min", .pad_value = 0) %>%
  mutate(
    date = as_date(time_stamp),
    demand = as.numeric(demand)
  ) %>%
  group_by(date) %>%
  summarise(
    mean_demand = mean(demand, na.rm = FALSE),
    .groups = "drop"
  ) %>% 
  filter(!is.na(mean_demand))


# EDA ---------------------------------------------------------------------

# Create Daily Plot
daily_demand %>%
  ggplot(aes(x = date, y = mean_demand)) +
  geom_line(color = "#2c3e50", linewidth = 0.7) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Daily Average Electricity Demand in Ireland",
    subtitle = "From 2014 to Present",
    x = "Date",
    y = "Mean Demand (MW)",
    caption = "Data source: EirGrid"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, color = "gray30", margin = margin(b = 10)),
    axis.title = element_text(size = 11, color = "gray30"),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), # Consistent with boxplot example
    panel.border = element_rect(color = "gray90", fill = NA), # Consistent with boxplot example
    plot.caption = element_text(color = "grey40", hjust = 0)
  ) +
  scale_y_continuous(
    labels = scales::comma,
    breaks = pretty_breaks(n = 8), # Using pretty_breaks for consistency and automatic scaling
    expand = expansion(mult = 0.05) # Consistent with boxplot example
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  )

theme_custom <- function() {
  theme_minimal() +
    theme(
      # Text elements
      plot.title = element_text(
        size = 16,
        face = "bold",
        margin = margin(b = 20)
      ),
      plot.subtitle = element_text(
        size = 12,
        color = "gray30",
        margin = margin(b = 15)
      ),
      axis.title = element_text(
        size = 11,
        color = "gray30"
      ),

      # Panel elements
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray90"),
      panel.border = element_rect(color = "gray90", fill = NA),

      # Facet elements
      strip.text = element_text(
        size = 10,
        face = "bold",
        margin = margin(b = 5)
      ),
      strip.placement = "outside",
      strip.background = element_blank(),
      panel.spacing = unit(0.2, "lines"),

      # Remove x-axis elements as specified
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}


# Create Weekly Plot
daily_demand %>%
  mutate(week_num = lubridate::week(date)) %>%
  ggplot(aes(x = factor(week_num), y = mean_demand)) +
  geom_boxplot(
    fill = "#69b3a2",
    alpha = 0.7,
    outlier.shape = 20,
    outlier.size = 1.5,
    outlier.alpha = 0.5
  ) +
  labs(
    title = "Electricity Demand Distribution by Week",
    subtitle = "Showing demand variations across each week",
    x = "Week of Year",
    y = "Mean Demand (MW)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, color = "gray30", margin = margin(b = 10)),
    axis.title = element_text(size = 11, color = "gray30"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(color = "gray90", fill = NA)
  ) +
  scale_y_continuous(
    labels = comma,
    breaks = pretty_breaks(n = 8),
    expand = expansion(mult = 0.05)
  )

# Create Monthly Plot
daily_demand %>%
  mutate(month = lubridate::month(date, label = TRUE, abbr = FALSE)) %>%
  ggplot(aes(x = month, y = mean_demand)) +
  geom_boxplot(
    fill = "#69b3a2",
    alpha = 0.7,
    outlier.shape = 20,
    outlier.size = 1.5,
    outlier.alpha = 0.5
  ) +
  labs(
    title = "Electricity Demand Distribution by Month",
    subtitle = "Showing demand variations across each month",
    x = "Month",
    y = "Mean Demand (MW)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, color = "gray30", margin = margin(b = 10)),
    axis.title = element_text(size = 11, color = "gray30"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(color = "gray90", fill = NA)
  ) +
  scale_y_continuous(
    labels = comma,
    breaks = pretty_breaks(n = 8),
    expand = expansion(mult = 0.05)
  ) +
  scale_x_discrete( # Important: Use scale_x_discrete to preserve month order
    limits = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December") # Explicitly set the order
  )

# Create Quarterly Plot
daily_demand %>%
  mutate(quarter_num = lubridate::quarter(date)) %>%
  mutate(quarter = paste0("Q", quarter_num)) %>%
  ggplot(aes(x = quarter, y = mean_demand)) +
  geom_boxplot(
    fill = "#69b3a2",
    alpha = 0.7,
    outlier.shape = 20,
    outlier.size = 1.5,
    outlier.alpha = 0.5
  ) +
  labs(
    title = "Electricity Demand Distribution by Quarter",
    subtitle = "Showing demand variations across each quarter",
    x = "Quarter",
    y = "Mean Demand (MW)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, color = "gray30", margin = margin(b = 10)),
    axis.title = element_text(size = 11, color = "gray30"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(color = "gray90", fill = NA)
  ) +
  scale_y_continuous(
    labels = comma,
    breaks = pretty_breaks(n = 8),
    expand = expansion(mult = 0.05)
  ) +
  scale_x_discrete( # Important: Use scale_x_discrete to preserve quarter order
    limits = c("Q1", "Q2", "Q3", "Q4") # Explicitly set the order
  )

# Create Yearly Plot
daily_demand %>%
  mutate(year = lubridate::year(date)) %>%
  ggplot(aes(x = factor(year), y = mean_demand)) +
  geom_boxplot(
    fill = "#69b3a2",
    alpha = 0.7,
    outlier.shape = 20,
    outlier.size = 1.5,
    outlier.alpha = 0.5
  ) +
  labs(
    title = "Electricity Demand Distribution by Year",
    subtitle = "Showing demand variations across each year",
    x = "Year",
    y = "Mean Demand (MW)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, color = "gray30", margin = margin(b = 10)),
    axis.title = element_text(size = 11, color = "gray30"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(color = "gray90", fill = NA)
  ) +
  scale_y_continuous(
    labels = comma,
    breaks = pretty_breaks(n = 8),
    expand = expansion(mult = 0.05)
  )

# OVERALL DEMAND DENSITY PLOT BY YEAR
ggplot(daily_demand, aes(x = mean_demand)) +
  geom_density(fill = "#69b3a2", alpha = 0.7) +
  facet_wrap(~ lubridate::year(date)) +
  labs(
    title = "Density of Electricity Demand by Year",
    x = "Mean Demand (MW)",
    y = "Density"
  ) +
  theme_minimal()

# OVERALL ELECTRICITY DEMAND DENSITY PLOT
ggplot(daily_demand, aes(x = mean_demand)) +
  geom_density(fill = "#69b3a2", alpha = 0.7) +
  labs(
    title = "Density of Electricity Demand",
    x = "Mean Demand (MW)",
    y = "Density"
  ) +
  theme_minimal()

# ACF Plot
forecast::ggAcf(
  daily_demand$mean_demand,
  type = "correlation",
  plot = TRUE,
  lag.max = 100
) +
  labs(
    title = "Autocorrelation Plot for Eirgrid Daily Demand",
    x = "Lag (Days)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# PACF Plot
forecast::ggPacf(
  daily_demand$mean_demand,
  lag.max = 100,
  plot = TRUE,
  demean = TRUE
) +
  labs(
    title = "Partial Autocorrelation Plot for Eirgrid Daily Demand",
    x = "Lag (Days)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )


# MODELLING ---------------------------------------------------------------

# THIS ARTICLE SUGGESTS TO CALCULATE ROLLING METRICS BEFORE SPLITTING THE DATA, DO THIS

daily_features <- daily_demand %>%
  mutate(
    #difference = mean_demand - dplyr::lag(mean_demand, n = 1),
    lag_1d  = dplyr::lag(mean_demand, n = 1),
    lag_7d  = dplyr::lag(mean_demand, n = 7),
    lag_14d = dplyr::lag(mean_demand, n = 14),
    lag_21d = dplyr::lag(mean_demand, n = 21),
    lag_30d = dplyr::lag(mean_demand, n = 30),
    lag_60d = dplyr::lag(mean_demand, n = 60),
    lag_90d = dplyr::lag(mean_demand, n = 90),
    lag_365d = dplyr::lag(mean_demand, n = 365),
    diff_1d = mean_demand - dplyr::lag(mean_demand, n = 1),
    deriv2_1d = diff_1d - dplyr::lag(diff_1d, n = 1),
    diff_7d = mean_demand - dplyr::lag(mean_demand, n = 7),
    diff_14d = mean_demand - dplyr::lag(mean_demand, n = 14),
    diff_21d = mean_demand - dplyr::lag(mean_demand, n = 21),
    diff_30d = mean_demand - dplyr::lag(mean_demand, n = 30),
    diff_60d = mean_demand - dplyr::lag(mean_demand, n = 60),
    diff_90d = mean_demand - dplyr::lag(mean_demand, n = 90),
    diff_365d = mean_demand - dplyr::lag(mean_demand, n = 365),
    rolling_mean_7d = slider::slide_dbl(mean_demand, ~ mean(.x, na.rm = TRUE), .before = 7, .after = -1, .complete = FALSE),
    rolling_sd_7d = slider::slide_dbl(mean_demand, ~ sd(.x, na.rm = TRUE), .before = 7, .after = -1, .complete = FALSE),
    rolling_min_7d = slider::slide_dbl(mean_demand, ~ min(.x, na.rm = TRUE), .before = 7, .after = -1, .complete = FALSE),
    rolling_max_7d = slider::slide_dbl(mean_demand, ~ max(.x, na.rm = TRUE), .before = 7, .after = -1, .complete = FALSE),
    rolling_median_7d = slider::slide_dbl(mean_demand, ~ median(.x, na.rm = TRUE), .before = 7, .after = -1, .complete = FALSE),
    rolling_q25_7d = slider::slide_dbl(mean_demand, ~ quantile(.x, probs = 0.25, na.rm = TRUE), .before = 7, .after = -1, .complete = FALSE),
    rolling_q75_7d = slider::slide_dbl(mean_demand, ~ quantile(.x, probs = 0.75, na.rm = TRUE), .before = 7, .after = -1, .complete = FALSE),
    rolling_sum_7d = slider::slide_dbl(mean_demand, ~ sum(.x, na.rm = TRUE), .before = 7, .after = -1, .complete = FALSE),
    rolling_skew_7d = slider::slide_dbl(mean_demand, ~ moments::skewness(.x, na.rm = TRUE), .before = 7, .after = -1, .complete = FALSE),
    rolling_kurt_7d = slider::slide_dbl(mean_demand, ~ moments::kurtosis(.x, na.rm = TRUE), .before = 7, .after = -1, .complete = FALSE),
    rolling_mean_14d = slider::slide_dbl(mean_demand, ~ mean(.x, na.rm = TRUE), .before = 14, .after = -1, .complete = FALSE),
    rolling_sd_14d = slider::slide_dbl(mean_demand, ~ sd(.x, na.rm = TRUE), .before = 14, .after = -1,.complete = FALSE),
    rolling_min_14d = slider::slide_dbl(mean_demand, ~ min(.x, na.rm = TRUE), .before = 14, .after = -1, .complete = FALSE),
    rolling_max_14d = slider::slide_dbl(mean_demand, ~ max(.x, na.rm = TRUE), .before = 14, .after = -1, .complete = FALSE),
    rolling_median_14d = slider::slide_dbl(mean_demand, ~ median(.x, na.rm = TRUE), .before = 14, .after = -1, .complete = FALSE),
    rolling_q25_14d = slider::slide_dbl(mean_demand, ~ quantile(.x, probs = 0.25, na.rm = TRUE), .before = 14, .after = -1, .complete = FALSE),
    rolling_q75_14d = slider::slide_dbl(mean_demand, ~ quantile(.x, probs = 0.75, na.rm = TRUE), .before = 14, .after = -1, .complete = FALSE),
    rolling_sum_14d = slider::slide_dbl(mean_demand, ~ sum(.x, na.rm = TRUE), .before = 14, .after = -1, .complete = FALSE),
    rolling_skew_14d = slider::slide_dbl(mean_demand, ~ moments::skewness(.x, na.rm = TRUE), .before = 14, .after = -1, .complete = FALSE),
    rolling_kurt_14d = slider::slide_dbl(mean_demand, ~ moments::kurtosis(.x, na.rm = TRUE), .before = 14, .after = -1, .complete = FALSE),
    rolling_mean_21d = slider::slide_dbl(mean_demand, ~ mean(.x, na.rm = TRUE), .before = 21, .after = -1, .complete = FALSE),
    rolling_sd_21d = slider::slide_dbl(mean_demand, ~ sd(.x, na.rm = TRUE), .before = 21, .after = -1, .complete = FALSE),
    rolling_min_21d = slider::slide_dbl(mean_demand, ~ min(.x, na.rm = TRUE), .before = 21, .after = -1, .complete = FALSE),
    rolling_max_21d = slider::slide_dbl(mean_demand, ~ max(.x, na.rm = TRUE), .before = 21, .after = -1, .complete = FALSE),
    rolling_median_21d = slider::slide_dbl(mean_demand, ~ median(.x, na.rm = TRUE), .before = 21, .after = -1, .complete = FALSE),
    rolling_q25_21d = slider::slide_dbl(mean_demand, ~ quantile(.x, probs = 0.25, na.rm = TRUE), .before = 21, .after = -1, .complete = FALSE),
    rolling_q75_21d = slider::slide_dbl(mean_demand, ~ quantile(.x, probs = 0.75, na.rm = TRUE), .before = 21, .after = -1, .complete = FALSE),
    rolling_sum_21d = slider::slide_dbl(mean_demand, ~ sum(.x, na.rm = TRUE), .before = 21, .after = -1, .complete = FALSE),
    rolling_skew_21d = slider::slide_dbl(mean_demand, ~ moments::skewness(.x, na.rm = TRUE), .before = 21, .after = -1, .complete = FALSE),
    rolling_kurt_21d = slider::slide_dbl(mean_demand, ~ moments::kurtosis(.x, na.rm = TRUE), .before = 21, .after = -1, .complete = FALSE),
    rolling_mean_30d = slider::slide_dbl(mean_demand, ~ mean(.x, na.rm = TRUE), .before = 30, .after = -1, .complete = FALSE),
    rolling_sd_30d = slider::slide_dbl(mean_demand, ~ sd(.x, na.rm = TRUE), .before = 30, .after = -1, .complete = FALSE),
    rolling_min_30d = slider::slide_dbl(mean_demand, ~ min(.x, na.rm = TRUE), .before = 30, .after = -1, .complete = FALSE),
    rolling_max_30d = slider::slide_dbl(mean_demand, ~ max(.x, na.rm = TRUE), .before = 30, .after = -1, .complete = FALSE),
    rolling_median_30d = slider::slide_dbl(mean_demand, ~ median(.x, na.rm = TRUE), .before = 30, .after = -1, .complete = FALSE),
    rolling_q25_30d = slider::slide_dbl(mean_demand, ~ quantile(.x, probs = 0.25, na.rm = TRUE), .before = 30, .after = -1, .complete = FALSE),
    rolling_q75_30d = slider::slide_dbl(mean_demand, ~ quantile(.x, probs = 0.75, na.rm = TRUE), .before = 30, .after = -1, .complete = FALSE),
    rolling_sum_30d = slider::slide_dbl(mean_demand, ~ sum(.x, na.rm = TRUE), .before = 30, .after = -1, .complete = FALSE),
    rolling_skew_30d = slider::slide_dbl(mean_demand, ~ moments::skewness(.x, na.rm = TRUE), .before = 30, .after = -1, .complete = FALSE),
    rolling_kurt_30d = slider::slide_dbl(mean_demand, ~ moments::kurtosis(.x, na.rm = TRUE), .before = 30, .after = -1, .complete = FALSE),
    rolling_mean_60d = slider::slide_dbl(mean_demand, ~ mean(.x, na.rm = TRUE), .before = 60, .after = -1, .complete = FALSE),
    rolling_sd_60d = slider::slide_dbl(mean_demand, ~ sd(.x, na.rm = TRUE), .before = 60, .after = -1, .complete = FALSE),
    rolling_min_60d = slider::slide_dbl(mean_demand, ~ min(.x, na.rm = TRUE), .before = 60, .after = -1, .complete = FALSE),
    rolling_max_60d = slider::slide_dbl(mean_demand, ~ max(.x, na.rm = TRUE), .before = 60, .after = -1, .complete = FALSE),
    rolling_median_60d = slider::slide_dbl(mean_demand, ~ median(.x, na.rm = TRUE), .before = 60, .after = -1, .complete = FALSE),
    rolling_q25_60d = slider::slide_dbl(mean_demand, ~ quantile(.x, probs = 0.25, na.rm = TRUE), .before = 60, .after = -1, .complete = FALSE),
    rolling_q75_60d = slider::slide_dbl(mean_demand, ~ quantile(.x, probs = 0.75, na.rm = TRUE), .before = 60, .after = -1, .complete = FALSE),
    rolling_sum_60d = slider::slide_dbl(mean_demand, ~ sum(.x, na.rm = TRUE), .before = 60, .after = -1, .complete = FALSE),
    rolling_skew_60d = slider::slide_dbl(mean_demand, ~ moments::skewness(.x, na.rm = TRUE), .before = 60, .after = -1, .complete = FALSE),
    rolling_kurt_60d = slider::slide_dbl(mean_demand, ~ moments::kurtosis(.x, na.rm = TRUE), .before = 60, .after = -1, .complete = FALSE),
    rolling_mean_90d = slider::slide_dbl(mean_demand, ~ mean(.x, na.rm = TRUE), .before = 90, .after = -1, .complete = FALSE),
    rolling_sd_90d = slider::slide_dbl(mean_demand, ~ sd(.x, na.rm = TRUE), .before = 90, .after = -1, .complete = FALSE),
    rolling_min_90d = slider::slide_dbl(mean_demand, ~ min(.x, na.rm = TRUE), .before = 90, .after = -1, .complete = FALSE),
    rolling_max_90d = slider::slide_dbl(mean_demand, ~ max(.x, na.rm = TRUE), .before = 90, .after = -1, .complete = FALSE),
    rolling_median_90d = slider::slide_dbl(mean_demand, ~ median(.x, na.rm = TRUE), .before = 90,  .after = -1, .complete = FALSE),
    rolling_q25_90d = slider::slide_dbl(mean_demand, ~ quantile(.x, probs = 0.25, na.rm = TRUE), .before = 90,  .after = -1, .complete = FALSE),
    rolling_q75_90d = slider::slide_dbl(mean_demand, ~ quantile(.x, probs = 0.75, na.rm = TRUE), .before = 90,  .after = -1, .complete = FALSE),
    rolling_sum_90d = slider::slide_dbl(mean_demand, ~ sum(.x, na.rm = TRUE), .before = 90, .after = -1, .complete = FALSE),
    rolling_skew_90d = slider::slide_dbl(mean_demand, ~ moments::skewness(.x, na.rm = TRUE), .before = 90, .after = -1, .complete = FALSE),
    rolling_kurt_90d = slider::slide_dbl(mean_demand, ~ moments::kurtosis(.x, na.rm = TRUE), .before = 90, .after = -1, .complete = FALSE),
    rolling_mean_365d = slider::slide_dbl(mean_demand, ~ mean(.x, na.rm = TRUE), .before = 365, .after = -1, .complete = FALSE),
    rolling_sd_365d = slider::slide_dbl(mean_demand, ~ sd(.x, na.rm = TRUE), .before = 365, .after = -1, .complete = FALSE),
    rolling_min_365d = slider::slide_dbl(mean_demand, ~ min(.x, na.rm = TRUE), .before = 365, .after = -1, .complete = FALSE),
    rolling_max_365d = slider::slide_dbl(mean_demand, ~ max(.x, na.rm = TRUE), .before = 365, .after = -1, .complete = FALSE),
    rolling_median_365d = slider::slide_dbl(mean_demand, ~ median(.x, na.rm = TRUE), .before = 365, .after = -1, .complete = FALSE),
    rolling_q25_365d = slider::slide_dbl(mean_demand, ~ quantile(.x, probs = 0.25, na.rm = TRUE), .before = 365, .after = -1, .complete = FALSE),
    rolling_q75_365d = slider::slide_dbl(mean_demand, ~ quantile(.x, probs = 0.75, na.rm = TRUE), .before = 365, .after = -1, .complete = FALSE),
    rolling_sum_365d = slider::slide_dbl(mean_demand, ~ sum(.x, na.rm = TRUE), .before = 365, .after = -1, .complete = FALSE),
    rolling_skew_365d = slider::slide_dbl(mean_demand, ~ moments::skewness(.x, na.rm = TRUE), .before = 365, .after = -1, .complete = FALSE),
    rolling_kurt_365d = slider::slide_dbl(mean_demand, ~ moments::kurtosis(.x, na.rm = TRUE), .before = 365, .after = -1, .complete = FALSE),
    pct_change_1d = (mean_demand - dplyr::lag(mean_demand, n = 1)) / dplyr::lag(mean_demand, n = 1),
    pct_change_7d = (mean_demand - dplyr::lag(mean_demand, n = 7)) / dplyr::lag(mean_demand, n = 7),
    pct_change_14d = (mean_demand - dplyr::lag(mean_demand, n = 14)) / dplyr::lag(mean_demand, n = 14), 
    pct_change_21d = (mean_demand - dplyr::lag(mean_demand, n = 21)) / dplyr::lag(mean_demand, n = 21),
    pct_change_30d = (mean_demand - dplyr::lag(mean_demand, n = 30)) / dplyr::lag(mean_demand, n = 30),
    pct_change_60d = (mean_demand - dplyr::lag(mean_demand, n = 60)) / dplyr::lag(mean_demand, n = 60),
    pct_change_90d = (mean_demand - dplyr::lag(mean_demand, n = 90)) / dplyr::lag(mean_demand, n = 90), 
    pct_change_365d = (mean_demand - dplyr::lag(mean_demand, n = 365)) / dplyr::lag(mean_demand, n = 365),
    is_football_final = date %in% as.Date(c(
      "2014-09-21", "2015-09-20", "2016-10-01", "2017-09-17",
      "2018-09-02", "2019-09-14", "2020-12-19", "2021-09-11",
      "2022-07-24", "2023-07-30", "2024-07-28"
    )),
    is_hurling_final = date %in% as.Date(c(
      "2014-09-07", "2015-09-06", "2016-09-04", "2017-09-03",
      "2018-08-19", "2019-08-18", "2020-12-13", "2021-08-22",
      "2022-07-17", "2023-07-23", "2024-07-21"
    )),
    is_irish_holiday = date %in% as.Date(c(
      # 2014 Irish Holidays
      "2014-01-01", "2014-03-17", "2014-04-21", "2014-05-05",
      "2014-06-02", "2014-08-04", "2014-10-27", "2014-12-25",
      "2014-12-26",
      
      # 2015 Irish Holidays
      "2015-01-01", "2015-03-17", "2015-04-06", "2015-05-04",
      "2015-06-01", "2015-08-03", "2015-10-26", "2015-12-25",
      "2015-12-26",
      
      # 2016 Irish Holidays
      "2016-01-01", "2016-03-17", "2016-03-28", "2016-05-02",
      "2016-06-06", "2016-08-01", "2016-10-31", "2016-12-25",
      "2016-12-26",
      
      # 2017 Irish Holidays
      "2017-01-01", "2017-03-17", "2017-04-17", "2017-05-01",
      "2017-06-05", "2017-08-07", "2017-10-30", "2017-12-25",
      "2017-12-26",
      
      # 2018 Irish Holidays
      "2018-01-01", "2018-03-17", "2018-04-02", "2018-05-07",
      "2018-06-04", "2018-08-06", "2018-10-29", "2018-12-25",
      "2018-12-26",
      
      # 2019 Irish Holidays
      "2019-01-01", "2019-03-18", "2019-04-22", "2019-05-06",
      "2019-06-03", "2019-08-05", "2019-10-28", "2019-12-25",
      "2019-12-26",
      
      # 2020 Irish Holidays
      "2020-01-01", "2020-03-17", "2020-04-13", "2020-05-04",
      "2020-06-01", "2020-08-03", "2020-10-26", "2020-12-25",
      "2020-12-28",
      
      # 2021 Irish Holidays
      "2021-01-01", "2021-03-17", "2021-04-05", "2021-05-03",
      "2021-06-07", "2021-08-02", "2021-10-25", "2021-12-25",
      "2021-12-26",
      
      # 2022 Irish Holidays
      "2022-01-03", "2022-03-17", "2022-03-18", "2022-04-18",
      "2022-05-02", "2022-06-06", "2022-08-01", "2022-10-31",
      "2022-12-25", "2022-12-26",
      
      # 2023 Irish Holidays
      "2023-01-02", "2023-02-06", "2023-03-17", "2023-04-10",
      "2023-05-01", "2023-06-05", "2023-08-07", "2023-10-30",
      "2023-12-25", "2023-12-26",
      
      # 2024 Irish Holidays
      "2024-01-01", "2024-02-05", "2024-03-17", "2024-03-18",
      "2024-04-01", "2024-05-06", "2024-06-03", "2024-08-05",
      "2024-10-28", "2024-12-25", "2024-12-26",
      
      # 2025 Irish Holidays
      "2025-01-01", "2025-02-03", "2025-03-17", "2025-04-21",
      "2025-05-05", "2025-06-02", "2025-08-04", "2025-10-27",
      "2025-12-25", "2025-12-26"
    ))
) %>% 
  tk_augment_timeseries_signature(date) %>% 
  dplyr::select(-contains("hour"), -contains("minute"), -contains("second"), -diff, -contains(".iso"), -contains(".xts"), -contains(".lbl"), -am.pm)

#https://www.bryanshalloway.com/2020/10/12/window-functions-for-resampling/


#https://emoriebeck.github.io/behavior-prediction/random-forest.html


# SPLIT THE DATA INTO TRAINING AND TESTING SETS
initial_split <- daily_features %>% 
  filter(date >= max(date) - years(8)) %>%
  rsample::initial_time_split(prop = 0.8)

initial_split

# CREATE A TRAINING DATASET
train <- training(initial_split)

# CREATE A TESTING DATASET
test <- testing(initial_split)

resamples <- rsample::sliding_period(
  train,
  index = date,
  period = "day",
  lookback = 365 * 5,
  assess_stop = 30,
  step = 30
)

resamples


# ============================================================================
# MODEL PREPROCESSING AND TRAINING
# ============================================================================

recipe <- recipe(mean_demand ~ ., data = train) %>%
  step_log(all_outcomes()) %>%
  step_integer(is_hurling_final, is_football_final, is_irish_holiday, zero_based = TRUE) %>% 
  step_zv(all_predictors())

validate_recipe <- recipe %>% 
  prep() %>% 
  bake(new_data = NULL) %>% 
  glimpse()

# ============================================================================
# MODEL TRAINING AND EVALUATION
# ============================================================================

# DEFINE A RANDOM FOREST MODEL WITH TUNABLE PARAMETERS
rf_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

# DEFINE A XGBOOST MODEL WITH TUNABLE PARAMETERS
xgboost_spec <- boost_tree(
  trees = tune(),
  min_n = tune(),
  mtry = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# DEFINE A PROPHET MODEL
prophet_spec <- prophet_reg(
  seasonality_yearly = TRUE,
  seasonality_weekly = TRUE,
  seasonality_daily = FALSE,
  changepoint_num = tune(),
  changepoint_range = tune()
) %>%
  set_engine("prophet") %>%
  set_mode("regression")

# DEFINE A PROPHET BOOST MODEL WITH TUNABLE PARAMETERS
prophet_boost_spec <- prophet_boost(
  seasonality_yearly = FALSE,
  seasonality_weekly = FALSE,
  seasonality_daily = FALSE,
  changepoint_num = tune(),
  mtry = tune(),
  min_n = tune(),
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune()
) %>%
  set_engine("prophet_xgboost") %>%
  set_mode("regression")

# CREATE WORKFLOW SET WITH DATE FOR TIME SERIES MODELS
wflw_set_with_date <- workflow_set(
  preproc = list(
    with_date = recipe
  ),
  models = list(
    prophet = prophet_spec,
    prophet_boost = prophet_boost_spec
  ),
  cross = TRUE
)

# INSPECT WORKFLOW SET WITH DATE
wflw_set_with_date

# CREATE WORKFLOW SET WITHOUT DATE FOR ML MODELS
wflw_set_no_date <- workflow_set(
  preproc = list(
    no_date = recipe %>% step_rm(date)
  ),
  models = list(
    xgb = xgboost_spec,
    rf = rf_spec
  ),
  cross = TRUE
)

# INSPECT WORKFLOW SET WITHOUT DATE
wflw_set_no_date


# COMBINE ALL WORKFLOWS AND CLEAN WORKFLOW IDS
all_workflows <-
  bind_rows(
    wflw_set_with_date, wflw_set_no_date
  ) %>%
  mutate(wflow_id = gsub("(base_)", "", wflow_id))

# INSPECT COMBINED WORKFLOWS
all_workflows

# DEFINE RACE TUNING CONTROL PARAMETERS
grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    save_workflow = TRUE
  )

#cl <- parallel::makeCluster(2, type='PSOCK')

#registerDoParallel(cl)

#https://workshops.tidymodels.org/slides/extras-transit-case-study.html#/make-some-models

# EXECUTE WORKFLOW MAP WITH RACE TUNING
tictoc::tic()
wflw_results <- all_workflows %>%
  workflow_map(
    resamples = resamples,
    grid = 10,
    seed = 123,
    metrics = metric_set(mae,rmse,mase),
    control = grid_ctrl,
    verbose = TRUE
  )
tictoc::toc()

#registerDoSEQ()

saveRDS(wflw_results, "model.rds")

loaded_model <- readRDS("model.rds")

# RANK RESULTS AND FILTER FOR RMSE METRIC
loaded_model %>%
  rank_results() %>%
  dplyr::filter(.metric == "mae") %>%
  select(model, .config, mae = mean, rank)

# PLOT MODEL COMPARISON
autoplot(
  loaded_model,
  rank_metric = "mae",
  metric = "mae"
  #select_best = TRUE
) +
  geom_text(
    aes(
      y = mean,
      label = wflow_id
    ),
    angle = 90,
    vjust = 0.5,
    nudge_x = 0.1,
    size = 3.5
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "Estimated MAE (and approximate confidence intervals)",
    subtitle = "Best model configuration in each workflow",
    x = "Workflow Rank",
    y = "MAE"
  )

loaded_model %>% 
  extract_workflow_set_result("with_date_prophet") %>% 
  autoplot()

# EXTRACT BEST PROPHET MODEL RESULTS
best_result <- loaded_model %>%
  extract_workflow_set_result("with_date_prophet") %>%
  select_best(metric = "rmse")

best_result

# FIT FINAL MODEL ON TEST SET
model_test_results <-
  loaded_model %>%
  extract_workflow("with_date_prophet") %>%
  finalize_workflow(best_result) %>%
  last_fit(split = initial_split)

# COLLECT TEST SET METRICS
collect_metrics(model_test_results)

# PLOT PREDICTED VS OBSERVED VALUES
model_test_results %>%
  collect_predictions() %>%
  ggplot(aes(x = mean_demand, y =.pred)) +
  geom_abline(color = "gray50", lty = 2) +
  geom_point(alpha = 0.5, color = "#69b3a2") +
  coord_obs_pred() +
  labs(
    title = "Predicted vs. Observed Electricity Demand",
    x = "Observed Demand (MW)",
    y = "Predicted Demand (MW)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10))
  )

# Get predictions and actual values
predictions_df <- model_test_results %>%
  collect_predictions() %>%
  bind_cols(
    testing(initial_split) %>% select(date)
  )

# FORECAST PLOT
forecast_plot <- predictions_df %>%
  ggplot() +
  geom_line(aes(x = date, y = mean_demand, color = "Actual"),
    linewidth = 1
  ) +
  geom_line(aes(x = date, y = .pred, color = "Predicted"),
    linewidth = 1, linetype = "dashed"
  ) +
  scale_color_manual(values = c("Actual" = "#2C3E50", "Predicted" = "#E74C3C")) +
  theme_minimal() +
  labs(
    title = "Demand Forecast: Actual vs Predicted Values",
    x = "Date",
    y = "Demand",
    color = "Series"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

# ADD CONFIDENCE INTERVALS TO THE PLOT
if ("conf.low" %in% names(predictions_df) && "conf.high" %in% names(predictions_df)) {
  forecast_plot <- forecast_plot +
    geom_ribbon(
      data = predictions_df,
      aes(
        x = date,
        ymin = conf.low,
        ymax = conf.high
      ),
      fill = "#E74C3C",
      alpha = 0.1
    )
}

# DISPLAY THE PLOT
print(forecast_plot)

# CALCULATE FORECAST ACCURACY METRICS
accuracy_metrics <- predictions_df %>%
  summarise(
    RMSE = sqrt(mean((mean_demand - .pred)^2)),
    MAE = mean(abs(mean_demand - .pred)),
    MAPE = mean(abs((mean_demand - .pred) / mean_demand)) * 100
  )

print(accuracy_metrics)

# CREATE A WORKFLOW FOR THE BEST MODEL
final_workflow <- loaded_model %>%
  extract_workflow("with_date_prophet") %>%
  finalize_workflow(best_result)

# FIT FINAL MODEL WITH ALL THE DATA
final_fit <- final_workflow %>%
  last_fit(initial_split)

# SHOW MODEL PERFORMANCE
final_fit %>%
  collect_metrics() %>%
  gt()

final_fit_to_deploy <- final_fit %>% 
  extract_workflow()

# # FINALIZE AND EXTRACT THE MODEL WORKFLOW
# final_fit_to_deploy <- loaded_model %>%
#   extract_workflow("with_date_prophet") %>%
#   finalize_workflow(best_result) %>%
#   last_fit(split = initial_split) %>% 
#   extract_workflow()

# # DISPLAY VARIABLE IMPORTANCE PLOT
# final_fit_to_deploy %>% 
#   extract_fit_parsnip() %>% 
#   vip(geom = "point", num_features = 15)

# CREATE A PINS BOARD IN GOOGLE STORAGE BUCKET ----------------------------

# CONVERT WORKFLOW TO VETIVER MODEL
v <- vetiver_model(final_fit_to_deploy, 
                   model_name = "eirgrid_model", 
                   description = "mlops-test")

v

names(v)

# AUTHENTICATE WITH GOOGLE CLOUD STORAGE
gcs_auth(json_file = Sys.getenv("GCE_AUTH_FILE"), email = Sys.getenv("CR_BUILD_EMAIL"))

container <- googleCloudStorageR::gcs_list_buckets(projectId = "mlops-r")

bucket <- googleCloudStorageR::gcs_get_bucket(container$name)

# CREATE PINS BOARD AND SAVE MODEL
model_board <- pins::board_gcs(bucket = bucket$name, versioned = TRUE)

model_board %>% 
  vetiver::vetiver_pin_write(v)

model_board %>% 
  vetiver::vetiver_write_plumber(name = "eirgrid_model")

write_board_manifest(board = model_board)

plumber::pr() %>% 
  vetiver::vetiver_api(v) %>% 
  plumber::pr_run()

base_url <- "http://127.0.0.1:3307/"

url = paste0(base_url, "predict")

endpoint = vetiver::vetiver_endpoint(url)

pred_data = train

predict(endpoint, pred_data)

jsonlite::fromJSON(metadata)

# GENERATE PLUMBER API FOR MODEL
model_board %>% 
  vetiver_write_plumber(name = "eirgrid_model",
                        file = "gcp_plumber")

# BUILD DOCKER CONTAINER & DEPLOY ENDPOINT -------------------------------
vetiver::vetiver_prepare_docker(model_board,
                                docker_args = list(port = 8080),
                                name = "eirgrid_model"
)

write_board_manifest(board = model_board)

# vetiver::vetiver_write_plumber(model_board, "eirgrid_model", rsconnect = FALSE)
# 
# vetiver_write_docker(v)

#https://medium.com/@damiencaillet/automate-r-code-in-the-cloud-89266910cc36

docker build -t electricity-prediction .

docker image list

docker run --rm --publish 8080:8080 my-first-model

docker run -p 8000:8000 electricity-prediction

docker tag electricity-prediction europe-west1-docker.pkg.dev/mlops-r/electricity-forecasting/electricity-prediction:latest

gcloud auth configure-docker

docker run --env-file .Renviron --rm -p 8000:8000 electricity-prediction

docker push gcr.io/GCP_PROJECT_ID/update_catalog

