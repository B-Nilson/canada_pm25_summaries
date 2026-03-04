message("Running R/pre_render.R...")
# relative to project dir
input_dirs <- c("css", "js", "icons")
output_dirs <- c("daily", "monthly", "seasonal") |>
  file.path("index_files")

# skip if on production server (uses symlinks instead)
if (!is_production){
  for (output_dir in output_dirs) {
    dir.create(output_dir, showWarnings = FALSE)
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
}
