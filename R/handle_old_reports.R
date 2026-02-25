handle_old_reports <- function(
  report_dir,
  historic_dir = "historic",
  report_pattern = "\\d\\.html$",
  type = c("daily", "monthly", "seasonal")[1]
) {
  index_path <- report_dir |> file.path("index.html")
  index_report_name <- index_path |> extract_file_report_name()
  existing_reports <- report_dir |>
    file.path(historic_dir) |>
    list.files(pattern = report_pattern, full.names = TRUE) |>
    sort(decreasing = TRUE)
  existing_reports <- existing_reports[
    get_report_display_names(basename(existing_reports), type = type) !=
      index_report_name
  ]
  if (length(existing_reports) > 0) {
    names(existing_reports) <- existing_reports |>
      basename() |>
      stringr::str_remove("\\.html")
  }

  existing_report_display_names <- existing_reports |>
    get_report_display_names(type = type)

  existing_reports |>
    stringr::str_replace(report_dir, ".") |> # make relative to report
    make_old_reports_dropdown(
      report_names = existing_report_display_names,
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
