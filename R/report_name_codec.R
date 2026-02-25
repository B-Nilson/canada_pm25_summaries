parse_report_name <- function(report_name, type) {
  date_fmt <- dplyr::case_when(
    type == "daily" ~ "%Y-%m-%d %H",
    type %in% c("monthly", "seasonal") ~ "%Y-%m"
  )
  if (type != "seasonal") {
    drpdwn_date_fmt <- type |>
      dplyr::recode_values(
        from = c("daily", "monthly"),
        to = c("%Y %b %d (%H)", "%B %Y")
      )

    past_day <- type == "daily" & stringr::str_detect(report_name, "-night$")
    reports_parsed <- report_name |>
      stringr::str_remove("\\.html") |>
      stringr::str_replace("-day$", " 23") |>
      stringr::str_replace("-night$", " 11") |>
      lubridate::parse_date_time(date_fmt) -
      lubridate::days(ifelse(past_day, 1, 0))
    reports_parsed <- reports_parsed |>
      format(drpdwn_date_fmt)

    if (type == "daily") {
      reports_parsed <- reports_parsed |>
        stringr::str_replace("\\(11\\)$", "(night)") |>
        stringr::str_replace("\\(23\\)$", "(day)")
    }
  } else {
    reports_parsed <- report_name |>
      stringr::str_replace("-", " ") |>
      stringr::str_remove("\\.html")
  }
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
