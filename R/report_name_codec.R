# 2026-02-11 23:00:00 -> "2026-02-11-day"
get_report_file_names <- function(
  report_end_dates,
  type = c("daily", "monthly", "seasonal")[1],
  months_in_seasons = list(
    "Summer" = 5:10,
    "Winter" = c(11:12, 1:4)
  )
) {
  date_fmt <- type |>
    dplyr::recode_values(
      from = c("daily", "monthly", "seasonal"),
      to = c("%Y-%m-%d %H", "%Y-%m", NA)
    )

  if (type == "daily") {
    (report_end_dates - lubridate::days(1)) |>
      lubridate::ceiling_date("12 hours") |>
      format(date_fmt) |>
      stringr::str_replace(" 00$", "-day") |>
      stringr::str_replace(" 12$", "-night")
  } else if (type == "monthly") {
    report_end_dates |> format(date_fmt)
  } else if (type == "seasonal") {
    report_end_dates |>
      get_season(months_in_seasons = months_in_seasons) |>
      tolower()
  }
}

# "2026-02-11-day" -> "2026 Feb 11 (day)"
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
    display_names <- (display_names |>
      stringr::str_replace("-day$", " 23") |>
      stringr::str_replace("-night$", " 11") |>
      lubridate::parse_date_time(date_fmt)) |>
      format(display_fmt)
    if (type == "daily") {
      display_names <- display_names |>
        stringr::str_replace("\\(11\\)$", "(night)") |>
        stringr::str_replace("\\(23\\)$", "(day)")
    }
  }
  return(display_names)
}

# "2026 Feb 11 (day)" -> 2026-02-11 23:00:00
get_report_end_dates <- function(
  report_display_names
) {
  type <- dplyr::case_when(
    report_display_names |>
      stringr::str_detect("day|night") ~ "daily",
    report_display_names |>
      stringr::str_detect("Summer|Winter") ~ "seasonal",
    .default = "monthly"
  )[1]

  if (type == "seasonal") {
    report_display_names |>
      get_season_end(months_in_seasons = months_in_seasons)
  } else {
    date_fmt <- type |>
      dplyr::recode_values(
        from = c("daily", "monthly"),
        to = c("%Y %b %d (%H)", "%B %Y")
      )
    past_day <- type == "daily" &
      stringr::str_detect(report_display_names, "night")
    report_display_names |>
      stringr::str_replace("\\(day\\)", "23") |>
      stringr::str_replace("\\(night\\)", "11") |>
      lubridate::parse_date_time(date_fmt) +
      lubridate::days(ifelse(past_day, 1, 0))
  }
}

# test to ensure codec is circular
end_dates <- c("2026-02-12 23", "2026-02-13 11") |>
  lubridate::ymd_h(tz = "UTC")
end_dates_2 <- end_dates |>
  get_report_file_names(type = "daily") |>
  get_report_display_names(type = "daily") |>
  get_report_end_dates()
stopifnot(end_dates == end_dates_2)

# get report name from within html file report
extract_file_report_name <- function(file_path) {
  readLines(file_path) |>
    suppressWarnings() |>
    stringr::str_subset("<h1 class=\"title\">") |>
    stringr::str_extract(
      "<mark class=\"bg-info\">(.*)</mark></h1>",
      group = 1
    )
}
