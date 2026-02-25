fix_no_date_citations <- function(report_dirs, index_name = "index.html") {
  reports <- report_dirs |> file.path(index_name)
  reports <- reports[file.exists(reports)]

  for (report in reports) {
    report_lines <- readLines(report) |>
      suppressWarnings() # incomplete file lines

    # Extract report name from title
    report_name <- report_lines |>
      stringr::str_subset("<h1 class=\"title\">") |>
      stringr::str_extract(
        "<mark class=\"bg-info\">(.*)</mark></h1>",
        group = 1
      )

    # Extract modified date from header
    modified_date <- report_lines |>
      stringr::str_subset("<p class=\"date-modified\">") |>
      stringr::str_extract(
        "<p class=\"date-modified\">(.*)</p>",
        group = 1
      )

    # Replace n.d. with modified date, {RNAME} with report name
    date_line_number <- report_lines |> stringr::str_which("<div id=\"ref-") + 1
    report_lines[date_line_number] <- report_lines[date_line_number] |>
      stringr::str_replace(
        pattern = stringr::fixed("n.d."),
        replacement = modified_date |> paste0(".")
      ) |>
      stringr::str_replace(
        pattern = stringr::fixed("{RNAME}"),
        replacement = report_name
      )

    # Replace {RNAME} with report name if on different line
    rname_line_number <- report_lines |>
      stringr::str_which(stringr::fixed("{RNAME}"))
    if (date_line_number != rname_line_number) {
      report_lines[rname_line_number] <- report_lines[rname_line_number] |>
        stringr::str_replace(
          pattern = stringr::fixed("{RNAME}"),
          replacement = report_name
        )
    }

    # Overwrite file
    report_lines |> writeLines(report)
  }
}

# Run for each report type
report_dirs <- c("daily", "monthly", "seasonal")
fix_no_date_citations(report_dirs)
