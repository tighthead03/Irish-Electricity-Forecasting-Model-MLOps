# R/data/api_client.R

#' API client for EirGrid data
#' @export
fetch_eirgrid_data <- function(category, region, date_from, date_to, params) {
  # Format dates
  date_from_str <- format(date_from, "%d-%b-%Y")
  date_to_str <- format(date_to, "%d-%b-%Y")
  
  # Construct API URL
  api_url <- glue(
    "{params$api$base_url}?",
    "area={category}",
    "&region={region}",
    "&datefrom={date_from_str}+00:00",
    "&dateto={date_to_str}+00:00"
  )
  
  message(glue("Fetching data from {date_from_str} to {date_to_str}"))
  
  # Make request with retries
  for (attempt in 1:params$api$max_retries) {
    tryCatch({
      response <- GET(
        url = api_url,
        add_headers(
          "Accept" = "application/json",
          "User-Agent" = "Mozilla/5.0",
          "Referer" = "https://www.smartgriddashboard.com/"
        ),
        timeout(params$api$timeout)
      )
      
      if (status_code(response) == 200) {
        content <- fromJSON(rawToChar(response$content))
        if (!is.null(content$Rows) && length(content$Rows) > 0) {
          data <- as_tibble(content$Rows)
          message(glue("Retrieved {nrow(data)} rows"))
          return(data)
        }
      }
      
      message(glue("Attempt {attempt} failed. Retrying..."))
      Sys.sleep(2^attempt)
      
    }, error = function(e) {
      message(glue("Error on attempt {attempt}: {e$message}"))
      Sys.sleep(2^attempt)
    })
  }
  
  return(NULL)
}