# "2026-02-12-day" -> "2026 Feb 12 (day)"
get_report_display_names <- function(
  report_file_names,
  type = c("daily", "monthly", "seasonal")[1]
) {
  date_fmt <- dplyr::case_when(
    type == "daily" ~ "%Y-%m-%d %H",
    type %in% c("monthly", "seasonal") ~ "%Y-%m"
  )
  display_names <- report_file_names |>
    stringr::str_remove("\\.html")

  if (type == "seasonal") {
    display_names <- display_names |> stringr::str_replace("-", " ")
  } else {
    display_fmt <- type |>
      dplyr::recode_values(
        from = c("daily", "monthly"),
        to = c("%Y %b %d (%H)", "%B %Y")
      )
    past_day <- type == "daily" &
      stringr::str_detect(report_file_names, "-night$")
    display_names <- (display_names |>
      stringr::str_replace("-day$", " 23") |>
      stringr::str_replace("-night$", " 11") |>
      lubridate::parse_date_time(date_fmt) -
      lubridate::days(ifelse(past_day, 1, 0))) |>
      format(display_fmt)
    if (type == "daily") {
      display_names <- display_names |>
        stringr::str_replace("\\(11\\)$", "(night)") |>
        stringr::str_replace("\\(23\\)$", "(day)")
    }
  }
  return(display_names)
}

get_report_name <- function(
  max_date,
  type,
  months_in_seasons = list(
    "Summer" = 5:10,
    "Winter" = c(11:12, 1:4)
  )
) {
  date_fmt <- dplyr::case_when(
    type == "daily" ~ "%Y-%m-%d %H",
    type %in% c("monthly", "seasonal") ~ "%Y-%m"
  )

  if (type == "daily") {
    report_name <- (max_date - lubridate::days(1)) |>
      lubridate::ceiling_date("12 hours") |>
      format(date_fmt) |>
      stringr::str_replace(" 00$", "-day") |>
      stringr::str_replace(" 12$", "-night")
  } else if (type == "monthly") {
    report_name <- max_date |> format(date_fmt)
  } else if (type == "seasonal") {
    report_name <- max_date |>
      get_season(months_in_seasons = months_in_seasons)
  }

  return(report_name)
}

get_previous_report_name <- function(
  current_report_date,
  type,
  months_in_seasons = list(
    "Summer" = 5:10,
    "Winter" = c(11:12, 1:4)
  )
) {
  if (type == "daily") {
    period <- lubridate::hours(12)
  } else if (type == "monthly") {
    period <- lubridate::days(32)
  } else if (type == "seasonal") {
    period <- lubridate::days(183)
  } else {
    stop("Other types not supported!")
  }
  (current_report_date - period) |>
    get_report_name(type = type, months_in_seasons = months_in_seasons)
}
