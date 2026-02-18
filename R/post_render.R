input_dirs <- c("css", "js", "icons")

output_dirs <- c("daily", "monthly", "seasonal") |>
  file.path("index_files")

for (output_dir in output_dirs) {
  for (input_dir in input_dirs) {
    out_dir <- output_dir |> file.path(input_dir)
    out_dir |> unlink(recursive = TRUE, force = TRUE)
    out_dir |> dir.create(showWarnings = FALSE, recursive = TRUE)
    file.copy(
      from = input_dir,
      to = output_dir,
      recursive = TRUE,
      overwrite = TRUE
    ) |>
      invisible()
  }
}
