set_params <- function(type) {
  report_dir <- file.path(".", type)
  project_dir <- "../"
  setwd(project_dir)
  source("R/report-functions.R")
  source("R/report-constants.R")
  date_range <- file.path(".", type) |>
    get_report_start_end(
      type = type,
      months_in_seasons = months_in_seasons
    )
  report_date <- base::max(date_range) |> format("%Y-%m-%d")
  report_name <- base::max(date_range) |> get_report_name(type = type)
  report_name_parsed <- report_name |> parse_report_name(type = type)
  date_ranges_fmtted <- date_range |> make_report_date_ranges()
  setwd(report_dir) # back to report folder

  list(
    date = report_date,
    name = report_name,
    name_parsed = report_name_parsed,
    date_ranges = date_ranges_fmtted
  )
}
