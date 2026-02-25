source("R/report_name_codec.R")
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
    report_file_name <- report_name |>
      get_report_end_dates() |>
      get_report_file_names(type = dirname(report))

    # Extract modified date from header
    modified_date <- report_lines |>
      stringr::str_subset("<p class=\"date-modified\">") |>
      stringr::str_extract(
        "<p class=\"date-modified\">(.*)</p>",
        group = 1
      )

    # Replace n.d. with modified date, {RNAME} with report name, {RFILE} with report file name
    date_line <- report_lines |> stringr::str_which("<div id=\"ref-") + 1
    if (length(date_line) > 0) {
      report_lines[date_line] <- report_lines[date_line] |>
        stringr::str_replace(
          pattern = stringr::fixed("n.d."),
          replacement = modified_date |> paste0(".")
        )
    }
    rfile_line <- report_lines |>
      stringr::str_which(stringr::fixed("{RFILE}"))
    if (length(rfile_line) > 0) {
      report_lines[rfile_line] <- report_lines[rfile_line] |>
        stringr::str_replace_all(
          pattern = stringr::fixed("{RFILE}"),
          replacement = report_file_name
        )
    }
    rname_line <- report_lines |>
      stringr::str_which(stringr::fixed("{RNAME}"))
    if (length(rname_line) > 0) {
      report_lines[rname_line] <- report_lines[rname_line] |>
        stringr::str_replace_all(
          pattern = stringr::fixed("{RNAME}"),
          replacement = report_name
        )
    }

    # Overwrite file
    report_lines |> writeLines(report)
  }
}

copy_index_to_historic <- function(
  report_dir,
  to_transfer = c("index.html", "index_files", "plots", "data")
) {
  inputs <- report_dir |> file.path(to_transfer)
  if (!file.exists(inputs[1])) {
    warning("No index.html found in ", report_dir, "/")
    return(invisible())
  }

  # Extract report name from title in file and convert to file name
  report_file_name <- readLines(inputs[1]) |>
    suppressWarnings() |>
    stringr::str_subset("<h1 class=\"title\">") |>
    stringr::str_extract(
      "<mark class=\"bg-info\">(.*)</mark></h1>",
      group = 1
    ) |>
    get_report_end_dates() |>
    get_report_file_names(type = report_dir) |>
    paste0(".html")

  outputs <- report_dir |>
    file.path(
      "historic",
      to_transfer |>
        dplyr::recode_values("index.html" ~ report_file_name, default = "/")
    )
  for (i in seq_along(inputs)) {
    is_file <- file.exists(inputs[i]) & !dir.exists(inputs[i])
    if (!(dir.exists(inputs[i]) | is_file)) {
      next
    }
    file.copy(
      from = inputs[i],
      to = outputs[i],
      recursive = !is_file,
      overwrite = TRUE
    )
  }

  # remove historic/ from report selector dropdown
  readLines(outputs[1]) |>
    stringr::str_replace_all(
      pattern = stringr::fixed("/historic/"),
      replacement = "/"
    ) |>
    writeLines(outputs[1])

  invisible()
}

# Fix n.d. and {RNAME} in citations for each report type
report_dirs <- c("daily", "monthly", "seasonal")
fix_no_date_citations(report_dirs)

# Copy index and related files over to historic dir
for (report_dir in report_dirs) {
  copy_index_to_historic(report_dir)
}
