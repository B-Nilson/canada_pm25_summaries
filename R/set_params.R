set_params <- function(
  type,
  default_report_date = "2026-01-02 00",
  months_in_seasons = list("Summer" = 5:10, "Winter" = c(11:12, 1:4))
) {
  default_report_date <- lubridate::ymd_h(default_report_date, tz = "UTC")
  report_dir <- file.path(".", type)
  project_dir <- "../"
  setwd(project_dir)
  source("R/report-functions.R")
  source("R/report-constants.R")
  date_range <- file.path(".", type) |>
    get_report_start_end(
      type = type,
      months_in_seasons = months_in_seasons,
      default_date = default_report_date
    )
  report_date <- base::max(date_range) |> format("%Y-%m-%d")
  report_name <- base::max(date_range) |> get_report_file_names(type = type)
  report_display_name <- report_name |> get_report_display_names(type = type)
  date_ranges_fmtted <- date_range |> make_report_date_ranges()
  setwd(report_dir) # back to report folder

  list(
    date = report_date,
    name = report_name,
    display_name = report_display_name,
    date_ranges = date_ranges_fmtted
  )
}
