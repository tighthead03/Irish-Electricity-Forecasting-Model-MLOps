# INSTALL PACKAGES --------------------------------------------------------

# Install pacman if not already installed
if (!require("pacman")) install.packages("pacman")

# Load and install packages using pacman
pacman::p_load(
  tidyverse,
  tidymodels,
  vip,
  googleAuthR,
  googleCloudRunner,
  googleCloudStorageR,
  httr,
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

# Timer start
start_time <- Sys.time()

# Logging setup (basic)
log_message <- function(level, message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(paste(timestamp, "-", level, "-", message, "\n"))
}

fetch_data <- function(client, api, timeout = 25, max_tries = 8) {
  tryCatch(
    {
      for (attempt in 1:max_tries) {
        response <- GET(api, timeout = timeout)
        if (http_error(response)) {
          log_message("ERROR", paste("HTTPStatusError for API", api, ":", http_status(response)$message))
          if (attempt == max_tries) {
            stop(paste("HTTPStatusError after", max_tries, "attempts."))
          } else {
            Sys.sleep(2^attempt) # Exponential backoff
          }
        } else {
          return(fromJSON(content(response, "text", encoding = "UTF-8"))$Rows)
        }
      }
    },
    error = function(e) {
      log_message("ERROR", paste("Unexpected error for API", api, ":", e$message))
      stop(e)
    }
  )
}

get_historic_data <- function(region = "ROI", category = "demandactual") {
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  final_year <- as.integer(format(Sys.Date(), "%Y")) - 1999
  year_range <- c(14, final_year)

  walk(year_range[1]:year_range[2], ~ {
    year <- .x
    frames <- list()
    pb_month <- progress_bar$new(
      total = length(months),
      format = paste0("Fetching data for ", category, " - Year: ", year, " [:bar] :percent :elapsed")
    )
    walk(seq_along(months), ~ {
      month_idx <- .x
      pb_month$tick()
      month <- months[month_idx]
      next_month <- if (month_idx == 12) months[1] else months[month_idx + 1]
      next_year <- if (month_idx == 12) year + 1 else year
      api <- paste0(
        "https://www.smartgriddashboard.com/DashboardService.svc/data?",
        "area=", category, "&region=", region, "&",
        "datefrom=01-", month, "-20", year, "+00%3A00&",
        "dateto=01-", next_month, "-20", next_year, "+21%3A59"
      )
      tryCatch(
        {
          data <- fetch_data(GET(api), api)
          frames[[month_idx]] <- as.data.frame(data)
        },
        error = function(e) {
          log_message("WARNING", paste("Failed to fetch or process data for", api, ":", e$message))
        }
      )
    })
    if (length(frames) > 0) {
      final_df <- bind_rows(frames)
      csv_dir <- file.path("Downloaded_Data", region)
      if (!dir.exists(csv_dir)) {
        dir.create(csv_dir, recursive = TRUE)
      }
      csv_file <- file.path(csv_dir, paste0(region, "_", category, "_", year, "_Eirgrid.csv"))
      write.csv(final_df, csv_file, row.names = FALSE, col.names = FALSE)
      log_message("INFO", paste("File saved:", csv_file))
    }
  })
}

main <- function() {
  get_historic_data()
}

main()

end_time <- Sys.time()
log_message("INFO", "********************************************************")
log_message("INFO", paste("Script completed in", round(as.numeric(difftime(end_time, start_time, units = "secs")), 2), "seconds."))
log_message("INFO", "********************************************************")


# DATA PROCESSING -----------------------------------------------

# Set directory path
data_dir <- "C:/Irish-Electricity-Forecasting-Model-MLOps/Downloaded_Data/ALL"

# Get current timestamp for filtering
current_time <- Sys.time()

# List all demandactual files
demand_files <- list.files(
  path = data_dir,
  pattern = "demandactual.*\\.csv$",
  full.names = TRUE
)

# Function to read and process a single file
read_demand_file <- function(file_path) {
  read_csv(
    file_path,
    col_names = c("time_stamp", "col2", "col3", "demand"), # Changed column name to avoid conflict
    show_col_types = FALSE
  ) %>%
    select(time_stamp, demand) %>%
    mutate(
      time_stamp = parse_date_time(time_stamp, orders = c("dmY HMS", "Ymd HMS")),
      file_source = basename(file_path)
    ) %>%
    # Filter out future timestamps
    filter(!is.na(time_stamp), time_stamp <= current_time)
}

# Combine all files
combined_demand <- map_dfr(demand_files, ~ {
  tryCatch(
    {
      message("Processing file: ", basename(.x))
      data <- read_demand_file(.x)
      if (nrow(data) > 0) {
        message("  Found ", nrow(data), " valid records")
      } else {
        message("  No valid records found")
      }
      data
    },
    error = function(e) {
      warning(paste("Error reading file:", .x, "\nError:", e$message))
      return(NULL)
    }
  )
}) %>%
  arrange(time_stamp) # Using new column name

# Remove any duplicates
combined_demand <- combined_demand %>%
  distinct(time_stamp, .keep_all = TRUE)

# CONVERT 15 MIN DATA TO DAILY AND PERFORM FEATURE ENGINEERING RATHER THAN IN
# A RECIPE SINCE DROP NA IS PROBLEMATIC
daily_demand <- combined_demand %>%
  # Filter out NA values and ensure date range is valid first
  filter(
    !is.na(demand),
    time_stamp >= as.Date("2014-01-01"),
    time_stamp < Sys.Date()
  ) %>%
  pad_by_time(time_stamp, .by = "15 min", .pad_value = 0) %>%
  mutate(
    date = as_date(time_stamp),
    demand = as.numeric(demand)
  ) %>%
  group_by(date) %>%
  summarise(
    mean_demand = mean(demand, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    rowid_to_column(.data = ., var = "row_id"),
    event_type = case_when(
      # 2014 Events
      date == as.Date("2014-09-21") ~ "GAA Football Final",
      date == as.Date("2014-09-07") ~ "GAA Hurling Final",
      date %in% as.Date(c(
        "2014-01-01", "2014-03-17", "2014-04-21", "2014-05-05",
        "2014-06-02", "2014-08-04", "2014-10-27", "2014-12-25",
        "2014-12-26"
      )) ~ "Irish Holiday",

      # 2015 Events
      date == as.Date("2015-09-20") ~ "GAA Football Final",
      date == as.Date("2015-09-06") ~ "GAA Hurling Final",
      date %in% as.Date(c(
        "2015-01-01", "2015-03-17", "2015-04-06", "2015-05-04",
        "2015-06-01", "2015-08-03", "2015-10-26", "2015-12-25",
        "2015-12-26"
      )) ~ "Irish Holiday",

      # 2016 Events
      date == as.Date("2016-10-01") ~ "GAA Football Final",
      date == as.Date("2016-09-04") ~ "GAA Hurling Final",
      date %in% as.Date(c(
        "2016-01-01", "2016-03-17", "2016-03-28", "2016-05-02",
        "2016-06-06", "2016-08-01", "2016-10-31", "2016-12-25",
        "2016-12-26"
      )) ~ "Irish Holiday",

      # 2017 Events
      date == as.Date("2017-09-17") ~ "GAA Football Final",
      date == as.Date("2017-09-03") ~ "GAA Hurling Final",
      date %in% as.Date(c(
        "2017-01-01", "2017-03-17", "2017-04-17", "2017-05-01",
        "2017-06-05", "2017-08-07", "2017-10-30", "2017-12-25",
        "2017-12-26"
      )) ~ "Irish Holiday",

      # 2018 Events
      date == as.Date("2018-09-02") ~ "GAA Football Final",
      date == as.Date("2018-08-19") ~ "GAA Hurling Final",
      date %in% as.Date(c(
        "2018-01-01", "2018-03-17", "2018-04-02", "2018-05-07",
        "2018-06-04", "2018-08-06", "2018-10-29", "2018-12-25",
        "2018-12-26"
      )) ~ "Irish Holiday",

      # 2019 Events
      date == as.Date("2019-09-14") ~ "GAA Football Final",
      date == as.Date("2019-08-18") ~ "GAA Hurling Final",
      date %in% as.Date(c(
        "2019-01-01", "2019-03-18", "2019-04-22", "2019-05-06",
        "2019-06-03", "2019-08-05", "2019-10-28", "2019-12-25",
        "2019-12-26"
      )) ~ "Irish Holiday",

      # 2020 Events
      date == as.Date("2020-12-19") ~ "GAA Football Final",
      date == as.Date("2020-12-13") ~ "GAA Hurling Final",
      date %in% as.Date(c(
        "2020-01-01", "2020-03-17", "2020-04-13", "2020-05-04",
        "2020-06-01", "2020-08-03", "2020-10-26", "2020-12-25",
        "2020-12-28"
      )) ~ "Irish Holiday",

      # 2021 Events
      date == as.Date("2021-09-11") ~ "GAA Football Final",
      date == as.Date("2021-08-22") ~ "GAA Hurling Final",
      date %in% as.Date(c(
        "2021-01-01", "2021-03-17", "2021-04-05", "2021-05-03",
        "2021-06-07", "2021-08-02", "2021-10-25", "2021-12-25",
        "2021-12-26"
      )) ~ "Irish Holiday",

      # 2022 Events
      date == as.Date("2022-07-24") ~ "GAA Football Final",
      date == as.Date("2022-07-17") ~ "GAA Hurling Final",
      date %in% as.Date(c(
        "2022-01-03", "2022-03-17", "2022-03-18", "2022-04-18",
        "2022-05-02", "2022-06-06", "2022-08-01", "2022-10-31",
        "2022-12-25", "2022-12-26"
      )) ~ "Irish Holiday",

      # 2023 Events
      date == as.Date("2023-07-30") ~ "GAA Football Final",
      date == as.Date("2023-07-23") ~ "GAA Hurling Final",
      date %in% as.Date(c(
        "2023-01-02", "2023-02-06", "2023-03-17", "2023-04-10",
        "2023-05-01", "2023-06-05", "2023-08-07", "2023-10-30",
        "2023-12-25", "2023-12-26"
      )) ~ "Irish Holiday",

      # 2024 Events
      date == as.Date("2024-07-28") ~ "GAA Football Final",
      date == as.Date("2024-07-21") ~ "GAA Hurling Final",
      date %in% as.Date(c(
        "2024-01-01", "2024-02-05", "2024-03-17", "2024-03-18",
        "2024-04-01", "2024-05-06", "2024-06-03", "2024-08-05",
        "2024-10-28", "2024-12-25", "2024-12-26"
      )) ~ "Irish Holiday",

      # Default case
      TRUE ~ "Not Holiday"
    ),
    is_weekend = lubridate::wday(date, label = TRUE) %in% c("Sun", "Sat")
  ) %>%
  mutate(
    across(
      mean_demand,
      list(
        lag1 = ~ lag(., 1),
        lag2 = ~ lag(., 2),
        lag3 = ~ lag(., 3),
        lag4 = ~ lag(., 4),
        lag5 = ~ lag(., 5),
        lag6 = ~ lag(., 6),
        lag7 = ~ lag(., 7),
        lag8 = ~ lag(., 8)
      )
    ),
    across(
      ends_with("lag"),
      .fns = ~ slide_mean(., before = 3, after = 3, complete = TRUE),
      .names = "{.col}_roll7"
    ),
    # 14-day rolling means
    across(
      ends_with("lag"),
      .fns = ~ slide_mean(., before = 7, after = 6, complete = TRUE),
      .names = "{.col}_roll14"
    ),
    # 21-day rolling means
    across(
      ends_with("lag"),
      .fns = ~ slide_mean(., before = 10, after = 10, complete = TRUE),
      .names = "{.col}_roll21"
    )
  ) %>%
  drop_na()


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

splits <- daily_demand %>%
  initial_time_split(prop = 0.8)

splits

train <- rsample::training(splits)

test <- rsample::testing(splits)

resamples <- rsample::sliding_period(train,
  index = date,
  period = "month",
  # USE 24 MONTHS FOR THE ANALYSIS PERIOD
  lookback = 36,
  # ASSESSMENT SET DEFINED AS 3 MONTHS I.E. MATCHES THE REFRESH PERIOD OF THE MODEL
  assess_stop = 3,
  step = 3
)

devtools::source_gist("https://gist.github.com/brshallo/7d180bde932628a151a4d935ffa586a5")

resamples %>%
  extract_dates_rset() %>%
  print() %>%
  plot_dates_rset()

# ============================================================================
# MODEL PREPROCESSING AND TRAINING
# ============================================================================

# CREATE A MODELLING RECIPE
recipe_with_date <- recipe(mean_demand ~ ., data = train) %>%
  update_role(row_id, new_role = "indicator") %>%
  step_mutate(across(where(is.logical), as.numeric)) %>%
  step_timeseries_signature(date) %>%
  step_rm(
    contains("am.pm"), contains("hour"), contains("minute"),
    contains("second"), contains("xts")
  ) %>%
  step_fourier(date, period = 7, K = 5) %>%
  step_dummy(all_nominal(), one_hot = TRUE, naming = function(var, lvl, is_ordered) paste0(var, "_", lvl)) %>%
  step_naomit()

x <- recipe_with_date %>%
  prep() %>%
  bake(new_data = test)

# CREATE AN ALTERNATIVE RECIPE WITH NO DATE FOR THE RANDOM FOREST MODEL
recipe_no_date <- recipe_with_date %>%
  step_rm(date)

# test <- bake(prep(recipe_with_date), new_data = NULL) %>% glimpse(.)
#
# test_no_date <- bake(prep(recipe_no_date), new_data = NULL) %>% glimpse(.)

# ============================================================================
# MODEL TRAINING AND EVALUATION
# ============================================================================

# DEFINE A RANDOM FOREST MODEL WITH TUNABLE PARAMETERS
rf_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_engine("ranger") %>%
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
# NOTE: XGBOOST HANDLES SEASONALITY SO SEASONALITY PARAMETERS MUST BE FALSE
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
    base = recipe_with_date
  ),
  models = list(
    prophet = prophet_spec,
    prophet_boost = prophet_boost_spec
  )
)

# INSPECT WORKFLOW SET WITH DATE
wflw_set_with_date

# CREATE WORKFLOW SET WITHOUT DATE FOR ML MODELS
wflw_set_no_date <- workflow_set(
  preproc = list(
    base = recipe_no_date
  ),
  models = list(
    xgb = xgboost_spec,
    rf = rf_spec
  )
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
race_ctrl <- control_race(
  save_pred = TRUE,
  parallel_over = "everything",
  save_workflow = TRUE,
  verbose = TRUE,
  verbose_elim = TRUE,
  randomize = TRUE
)

# EXECUTE WORKFLOW MAP WITH RACE TUNING
tictoc::tic()
wflw_results <- all_workflows %>%
  workflow_map(
    seed = 16,
    fn = "tune_race_anova",
    resamples = resamples,
    grid = 25,
    metrics = metric_set(rmse, mae, mape),
    control = race_ctrl,
    verbose = TRUE
  )
tictoc::toc()

saveRDS(wflw_results, "model.rds")

loaded_model <- readRDS("model.rds")

# RANK RESULTS AND FILTER FOR RMSE METRIC
loaded_model %>%
  rank_results() %>%
  dplyr::filter(.metric == "rmse") %>%
  select(model, .config, rmse = mean, rank)

# PLOT MODEL COMPARISON
autoplot(
  loaded_model,
  rank_metric = "rmse",
  metric = "rmse",
  select_best = TRUE
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
    title = "Estimated RMSE (and approximate confidence intervals)",
    subtitle = "Best model configuration in each workflow",
    x = "Workflow Rank",
    y = "RMSE"
  )


# EXTRACT BEST PROPHET MODEL RESULTS
best_result <- loaded_model %>%
  extract_workflow_set_result("xgb") %>%
  select_best(metric = "rmse")

best_result

# FIT FINAL MODEL ON TEST SET
model_test_results <-
  loaded_model %>%
  extract_workflow("xgb") %>%
  finalize_workflow(best_result) %>%
  last_fit(split = splits)

# COLLECT TEST SET METRICS
collect_metrics(model_test_results)

# PLOT PREDICTED VS OBSERVED VALUES
model_test_results %>%
  collect_predictions() %>%
  ggplot(aes(x = mean_demand, y =.pred)) +
  geom_abline(color = "gray50", lty = 2) +
  geom_point(alpha = 0.5, color = "#69b3a2") +  # Added color to points
  coord_obs_pred() +
  labs(
    title = "Predicted vs. Observed Electricity Demand",  # Added title
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
    testing(splits) %>% select(date)
  )

# Create the forecast plot
forecast_plot <- predictions_df %>%
  ggplot() +
  # Actual values
  geom_line(aes(x = date, y = mean_demand, color = "Actual"),
    linewidth = 1
  ) +
  # Predicted values
  geom_line(aes(x = date, y = .pred, color = "Predicted"),
    linewidth = 1, linetype = "dashed"
  ) +
  # Customize colors
  scale_color_manual(values = c("Actual" = "#2C3E50", "Predicted" = "#E74C3C")) +
  # Add themes and labels
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

# Add confidence intervals if available in your prophet model
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

# Display the plot
print(forecast_plot)

# Calculate forecast accuracy metrics
accuracy_metrics <- predictions_df %>%
  summarise(
    RMSE = sqrt(mean((mean_demand - .pred)^2)),
    MAE = mean(abs(mean_demand - .pred)),
    MAPE = mean(abs((mean_demand - .pred) / mean_demand)) * 100
  )

print(accuracy_metrics)

final_fit_to_deploy <- loaded_model %>%
  extract_workflow("xgb") %>%
  finalize_workflow(best_result) %>%
  last_fit(split = splits) %>% 
  extract_workflow()

final_fit_to_deploy %>% 
  extract_fit_parsnip() %>% 
  vip(geom = "point", num_features = 15)


# CREATE A PINS BOARD IN GOOGLE STORAGE BUCKET ----------------------------

# CONVERT WORKFLOW TO VETIVER MODEL
v <- vetiver_model(final_fit_to_deploy, model_name = "eirgrid_model", description = "mlops-test")

v

gcs_auth(json_file = Sys.getenv("GCE_AUTH_FILE"), email = Sys.getenv("CR_BUILD_EMAIL"))

container <- googleCloudStorageR::gcs_list_buckets(projectId = "mlops-r")

bucket <- googleCloudStorageR::gcs_get_bucket(container$name)

model_board <- pins::board_gcs(bucket = bucket$name)

model_board %>% 
  vetiver_pin_write(v, check_renv = TRUE)

model_board %>% 
  vetiver_write_plumber("eirgrid_model")

pin_loc <- pins:::github_raw()

#write_board_manifest(model_board)

# BUILD DOCKER CONTAINER & DEPLOY ENDPOINT -------------------------------

vetiver::vetiver_prepare_docker(model_board,
  docker_args = list(port = 8080),
  name = "eirgrid_model"
)

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
