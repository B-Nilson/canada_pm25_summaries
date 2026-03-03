build_report_dropdown <- function(
  type = c("daily", "monthly", "seasonal")[1],
  historic_dir = "historic",
  current_report = "index.html"
) {
  # Find existing reports
  report_pattern <- type |>
    dplyr::recode_values(
      from = c("daily", "monthly", "seasonal"),
      to = c("[day,night]\\.html$", "\\d\\.html$", "[summer,winter]\\.html$")
    )
  existing_reports <- type |>
    file.path(historic_dir) |>
    list.files(pattern = report_pattern, full.names = TRUE) |>
    sort(decreasing = TRUE)

  # Remove date of current report
  index_report_name <- type |>
    file.path(current_report) |>
    extract_file_report_name()
  existing_report_names <- basename(existing_reports) |>
    get_report_display_names(type = type)
  existing_reports <- existing_reports[
    existing_report_names != index_report_name
  ]

  if (length(existing_reports) > 0) {
    names(existing_reports) <- existing_reports |>
      basename() |>
      stringr::str_remove("\\.html")
  }

  existing_reports |>
    stringr::str_replace(type, ".") |> # make relative to report
    make_old_reports_dropdown(
      button_label = "Select previous reports",
      dropdown_label = "Select a Report",
      type = type
    )
}

make_old_reports_dropdown <- function(
  report_paths,
  report_names,
  button_label = "Select previous reports",
  dropdown_label = "Select a Report",
  type = c("daily", "monthly", "seasonal")[1]
) {
  report_names <- report_paths |>
    get_report_display_names(type = type)
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
