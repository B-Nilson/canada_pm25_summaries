handle_old_reports <- function(
  report_dir,
  historic_dir = "historic",
  report_pattern = "\\d\\.html$",
  max_date,
  type = c("daily", "monthly", "seasonal")[1]
) {
  index_exists <- report_dir |>
    file.path("index.html") |>
    file.exists()
  existing_reports <- report_dir |>
    file.path(historic_dir) |>
    list.files(pattern = report_pattern, full.names = TRUE) |>
    sort(decreasing = TRUE)
  if (length(existing_reports) > 0) {
    names(existing_reports) <- existing_reports |>
      basename() |>
      stringr::str_remove("\\.html")
  }

  # Rename index.html if not done so already
  if (index_exists) {
    previous_report <- max_date |>
      get_previous_report_name(
        type = type,
        months_in_seasons = months_in_seasons
      )

    index_path <- report_dir |> file.path("index.html")
    new_path <- report_dir |>
      file.path(historic_dir, paste0(previous_report, ".html"))
    need_to_rename <- (length(existing_reports) &
      !previous_report %in% names(existing_reports)) |
      length(existing_reports) == 0
    if (need_to_rename) {
      index_path |> file.copy(new_path)
      existing_reports <- existing_reports |> c(new_path)
      names(existing_reports)[length(existing_reports)] <- previous_report
    } else {
      "Either no historic reports, or the report for %s already exists - not renaming index.html." |>
        sprintf(previous_report) |>
        warning()
    }
  }

  existing_reports_parsed <- existing_reports |>
    parse_report_name(type = type)

  existing_reports |>
    stringr::str_replace(report_dir, ".") |> # make relative to report
    make_old_reports_dropdown(
      report_names = existing_reports_parsed,
      button_label = "Select previous reports",
      dropdown_label = "Select a Report"
    )
}

make_old_reports_dropdown <- function(
  report_paths,
  report_names,
  button_label = "Select previous reports",
  dropdown_label = "Select a Report"
) {
  if (length(report_paths) == 0) {
    return("<p>No previous reports available at this time.</p>")
  }
  dropdown_entries <- '<li><a class="dropdown-item" href="%s">%s</a></li>' |>
    sprintf(report_paths, report_names) |>
    paste(collapse = "\n    ")

  '<div class="dropdown">
  <button class="btn dropdown-toggle" type="button" data-bs-toggle="dropdown" id="dropdownMenuButton" aria-expanded="false">
    %s
  </button>
  <ul class="dropdown-menu">
    <li><h6 class="dropdown-header">%s</h6></li>
    %s
  </ul>
</div>' |>
    sprintf(
      button_label,
      dropdown_label,
      dropdown_entries
    )
}

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
