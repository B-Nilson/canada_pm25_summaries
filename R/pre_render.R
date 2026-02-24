# relative to project dir
input_dirs <- c("css", "js", "icons")
output_dirs <- c("daily", "monthly", "seasonal") |>
  file.path("index_files")

for (output_dir in output_dirs) {
  for (input_dir in input_dirs) {
    if (!dir.exists(input_dir)) {
      next
    }
    out_dir <- output_dir |> file.path(input_dir)
    out_dir |> unlink(recursive = TRUE, force = TRUE)
    file.copy(
      from = input_dir,
      to = output_dir,
      recursive = TRUE,
      overwrite = TRUE
    ) |>
      invisible()
  }
}
