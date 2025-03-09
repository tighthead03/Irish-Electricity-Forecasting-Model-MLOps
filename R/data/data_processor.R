#' Process raw data files and add features
#' @param params Configuration parameters
process_raw_data <- function(params) {
  # Get list of raw data files
  raw_files <- list.files(
    path = params$paths$base_dir,
    pattern = "ROI_demandactual_.*_Eirgrid\\.csv$",
    full.names = TRUE
  )
  
  message(glue("Found {length(raw_files)} raw data files"))
  
  # Read and combine all files
  current_time <- Sys.time()
  
  # Read and combine all files with better error handling
  combined_demand <- raw_files %>%
    map_dfr(~ {
      message("Processing file: ", basename(.x))
      
      # Read the data
      data <- read_csv(
        .x,
        col_names = c("EffectiveTime", "FieldName", "Region", "Value"),  # Use actual column names
        col_types = cols(
          EffectiveTime = col_character(),
          FieldName = col_skip(),
          Region = col_skip(),
          Value = col_double()
        ),
        show_col_types = FALSE
      )
      
      print(head(data))
      print(str(data))
      
      # Clean and convert the data
      cleaned <- data %>%
        mutate(
          # Try multiple date formats
          EffectiveTime = case_when(
            str_detect(EffectiveTime, "^\\d{2}/\\d{2}/\\d{4}") ~ dmy_hms(EffectiveTime), # Correct format
            TRUE ~ as.POSIXct(NA)  # Handle any other formats as NA
          ),
          EffectiveTime = with_tz(EffectiveTime, tzone = "Europe/Dublin"),  # Assign time zone
          # Ensure demand is numeric
          Value = as.numeric(Value)
        ) %>%
        filter(!is.na(EffectiveTime),!is.na(Value)) %>%
        select(EffectiveTime, Value)
      
      print(head(cleaned))  # Print the first few rows of the cleaned data
      print(sum(is.na(cleaned$EffectiveTime)))  # Print the number of NA EffectiveTimes
      print(sum(is.na(cleaned$Value)))  # Print the number of NA Values
      
      message(glue("  Found {nrow(cleaned)} valid records after cleaning"))
      
      cleaned
    }) %>%
    arrange(EffectiveTime) %>%  # Use the correct column name
    distinct(EffectiveTime,.keep_all = TRUE)  # Use the correct column name
  
  message(glue("Combined data has {nrow(combined_demand)} rows"))
  
  # Process to daily data and add features
  daily_demand <- combined_demand %>%
    rename(time_stamp = EffectiveTime, demand = Value) %>%  # Rename columns for consistency
    filter(
      !is.na(demand),
      time_stamp >= as.Date("2014-01-01"),
      time_stamp < Sys.Date()
    ) %>%
    timetk::pad_by_time(time_stamp,.by = "15 min",.pad_value = 0) %>%
    mutate(date = as_date(time_stamp)) %>%
    group_by(date) %>%
    summarise(
      mean_demand = mean(demand),
      max_demand = max(demand),
      min_demand = min(demand),
      total_observations = n(),
      .groups = "drop"
    ) %>%
    mutate(
      rowid_to_column(.data =., var = "row_id"),
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
        TRUE ~ "Not Holiday"
      ),
      is_weekend = lubridate::wday(date, label = TRUE) %in% c("Sun", "Sat")
    ) %>%
    mutate(
      across(
        mean_demand,
        list(
          lag1 = ~ dplyr::lag(., 1),
          lag2 = ~ dplyr::lag(., 2),
          lag3 = ~ dplyr::lag(., 3),
          lag4 = ~ dplyr::lag(., 4),
          lag5 = ~ dplyr::lag(., 5),
          lag6 = ~ dplyr::lag(., 6),
          lag7 = ~ dplyr::lag(., 7),
          lag8 = ~ dplyr::lag(., 8)
        )
      ),
      across(
        ends_with("lag"),
        .fns = ~ timetk::slidify_vec(., before = 3, after = 3, complete = TRUE),
        .names = "{.col}_roll7"
      ),
      across(
        ends_with("lag"),
        .fns = ~ timetk::slidify_vec(., before = 7, after = 6, complete = TRUE),
        .names = "{.col}_roll14"
      ),
      across(
        ends_with("lag"),
        .fns = ~ timetk::slidify_vec(., before = 10, after = 10, complete = TRUE),
        .names = "{.col}_roll21"
      )
    ) %>%
    drop_na()
  
  #... (Rest of your code for saving the processed data)...
}