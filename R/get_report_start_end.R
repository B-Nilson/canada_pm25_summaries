get_report_start_end <- function(
  report_dir,
  type = c("daily", "monthly", "seasonal")[1],
  run_future = FALSE,
  months_in_seasons = list(
    "Summer" = 5:10,
    "Winter" = c(11:12, 1:4)
  )
) {
  index_exists <- file.exists(file.path(report_dir, "index.html"))
  now <- lubridate::now(tzone = "UTC")
  last_report_date <- report_dir |>
    get_last_report_date(
      type = type,
      months_in_seasons = months_in_seasons,
      index_exists = index_exists
    )

  if (type == "daily") {
    max_date <- now |> lubridate::floor_date("12 hours") - lubridate::hours(1)
    if (!is.null(last_report_date)) {
      max_date <- pmin(
        last_report_date + lubridate::hours(12),
        max_date
      )
    }
    min_date <- max_date - lubridate::hours(23)
  } else if (type == "monthly") {
    max_date <- now |> lubridate::floor_date("1 month") - lubridate::hours(1)

    if (run_future) {
      max_date <- (max_date + lubridate::hours(2)) |>
        lubridate::ceiling_date("1 months") -
        lubridate::hours(1)
    }
    if (!is.null(last_report_date)) {
      max_date <- pmin(
        (last_report_date + lubridate::hours(2)) |>
          lubridate::ceiling_date("1 months") -
          lubridate::hours(1),
        max_date
      )
    }

    min_date <- max_date |> lubridate::floor_date("1 months")
  } else if (type == "seasonal") {
    max_date <- (now - lubridate::days(ifelse(run_future, 0, 30 * 6))) |>
      get_season(months_in_season = months_in_seasons) |>
      get_season_end(months_in_seasons = months_in_seasons)

    if (!is.null(last_report_date)) {
      max_date <- pmin(
        (last_report_date + lubridate::days(30 * 6)) |>
          get_season(months_in_seasons = months_in_seasons) |>
          get_season_end(months_in_seasons = months_in_seasons),
        max_date
      )
    }

    is_winter <- lubridate::month(max_date) %in% months_in_seasons$Winter
    season_name <- ifelse(is_winter, "Winter", "Summer")
    min_date <- (lubridate::year(max_date) - ifelse(is_winter, 1, 0)) |>
      paste(dplyr::first(months_in_seasons[[season_name]])) |>
      lubridate::ym() |>
      lubridate::as_datetime(tz = "UTC")
  } else {
    stop("Report type not supported")
  }
  return(c(min_date, max_date))
}

get_last_report_date <- function(
  report_dir,
  type,
  months_in_seasons = list(
    "Summer" = 5:10,
    "Winter" = c(11:12, 1:4)
  ),
  index_exists = FALSE
) {
  report_patterns <- list(
    daily = "(day)|(night)\\.html$",
    monthly = "\\d\\.html$",
    seasonal = "Summer\\.html|Winter\\.html"
  )

  season_end_month <- months_in_seasons |>
    lapply(\(months) dplyr::last(months) |> as.character())

  last_report_name <- report_dir |>
    list.files(pattern = report_patterns[[type]], recursive = TRUE) |>
    sort() |>
    dplyr::last() |>
    stringr::str_remove("\\.html")

  if (index_exists) {
    if (is.na(last_report_name)) {
      last_report_name <- lubridate::now(tzone = "UTC") |>
        get_previous_report_name(
          type = type,
          months_in_seasons = months_in_seasons
        )
    }
  }
  if (is.na(last_report_name)) {
    return(NULL)
  }

  is_winter <- last_report_name |>
    stringr::str_detect("Winter")

  dplyr::case_when(
    type == "daily" ~ last_report_name |>
      stringr::str_replace("-day$", " 23") |>
      stringr::str_replace("-night$", " 11") |>
      lubridate::ymd_h(),
    type == "monthly" ~ last_report_name |>
      lubridate::ym() |>
      lubridate::ceiling_date("1 months") -
      lubridate::hours(1),
    type == "seasonal" ~ last_report_name |>
      get_season_end(months_in_seasons = months_in_seasons)
  )
}

get_season_end <- function(
  season,
  months_in_seasons = list(
    "Summer" = 5:10,
    "Winter" = c(11:12, 1:4)
  )
) {
  is_winter <- season |> stringr::str_detect("Winter")
  season_end_months <- months_in_seasons |>
    lapply(dplyr::last) |>
    lapply(as.character)
  season |>
    stringr::str_replace("Winter", season_end_months$Winter) |>
    stringr::str_replace("Summer", season_end_months$Summer) |>
    lubridate::ym() |>
    lubridate::as_datetime(tz = "UTC") |>
    lubridate::ceiling_date("1 months") -
    lubridate::hours(1) -
    lubridate::years(ifelse(is_winter, 1, 0))
}
