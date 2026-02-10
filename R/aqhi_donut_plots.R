save_aqhi_donuts_plots <- function(
  overall_summary,
  monitor_groups,
  stats = c("mean", "max"),
  avg = "24-hour",
  report_dir,
  figure_dir = "plots",
  fig_dims = list(h = 7, w = 11, u = 'in'),
  plot_timestamp = format(Sys.time(), "%Y%m%d%H%M%S")
) {
  dir.create(file.path(report_dir, figure_dir), showWarnings = FALSE)
  # Prov/Terr monitor counts for centers of donuts
  labels <- overall_summary |>
    dplyr::bind_rows(
      overall_summary |> dplyr::mutate(monitor = "FEM and PA")
    ) |>
    tidyr::complete(prov_terr, monitor) |>
    dplyr::summarise(n = sum(!is.na(pm25_max)), .by = c(prov_terr, monitor)) |>
    dplyr::mutate(label = paste("n =\n", n), x = 0, y = 0)

  # Create paths for donut plots
  monitors_clean <- monitor_groups |>
    stringr::str_to_lower() |>
    stringr::str_replace_all(" ", "_")
  plot_types <- stats |>
    rep(each = length(monitor_groups)) |>
    paste(monitors_clean |> rep(times = length(stats)), sep = "_")
  plot_names <- "monitor_donuts_%s_%s.png" |>
    sprintf(plot_types, plot_timestamp)
  plot_paths <- report_dir |>
    file.path(figure_dir, plot_names) |>
    as.list() |>
    setNames(plot_types)

  lapply(stats, \(stat) {
    plot_data <- overall_summary |>
      make_donut_data(pm25_col = paste0("pm25_", stat))

    # Make and save donut plots
    lapply(monitors_clean, \(monitor_group) {
      plot_type <- paste0(stat, "_", monitor_group)
      group_name <- names(monitors_clean)[monitors_clean == monitor_group]
      plot_data |>
        dplyr::filter(monitor == group_name) |>
        make_donut_plot(
          labels = labels |>
            dplyr::filter(monitor == group_name),
          stat = stringr::str_to_title(stat),
          plot_caption = plot_captions[[group_name]],
          avg = avg
        ) |>
        ggplot2::ggsave(
          filename = plot_paths[[plot_type]],
          dpi = 300,
          units = fig_dims$u,
          height = fig_dims$h,
          width = fig_dims$w
        )
    })
    return(plot_data)
  })
}
