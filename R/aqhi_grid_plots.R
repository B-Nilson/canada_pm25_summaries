make_and_save_prov_terr_grids <- function(
  grid_data,
  report_dir,
  figure_dir,
  monitor_groups,
  plot_timestamp,
  plot_captions,
  fig_dims = list(h = 4, w = 11, u = 'in')
) {
  monitor_groups_cleaned <- monitor_groups |>
    stringr::str_to_lower() |>
    stringr::str_replace_all(" ", "_")
  # Make paths to plot files
  plot_paths <- "%s/prov_median-peak_grid_%s_%s.png" |>
    sprintf(
      file.path(report_dir, figure_dir),
      monitor_groups_cleaned,
      plot_timestamp
    ) |>
    setNames(names(monitor_groups)) |>
    as.list()

  # Make and save plots
  names(monitor_groups) |>
    lapply(\(monitor_group) {
      grid_data |>
        make_prov_terr_grids(
          monitor_group = monitor_group,
          plot_caption = plot_captions[[monitor_group]]
        ) |>
        ggplot2::ggsave(
          filename = plot_paths[[m]],
          height = fig_dims$h,
          width = fig_dims$w,
          units = fig_dims$u,
          dpi = 300
        )
      invisible()
    })

  return(plot_paths)
}

make_and_save_fcst_zone_grids <- function(
  grid_data,
  report_dir,
  figure_dir,
  plot_timestamp,
  plot_caption = "",
  fig_dims = list(h = 7, w = 11, u = 'in')
) {
  prov_order <- canadata::provinces_and_territories$name_en |>
    as.character() |>
    dplyr::replace_values("Quebec" ~ "Québec") |> # TODO: remove once database in cync with canadata
    setNames(canadata::provinces_and_territories$abbreviation) |>
    as.list()
  # Make paths to plot files
  plot_paths <- "%s/zone_median_hourly_grid_%s_%s.png" |>
    sprintf(
      file.path(report_dir, figure_dir),
      names(prov_order),
      plot_timestamp
    ) |>
    setNames(names(prov_order)) |>
    as.list()

  # Make and save plots
  lapply(names(prov_order), \(prov_abbr) {
    prov_name <- prov_order[[prov_abbr]]
    grid_data |>
      subset(as.character(z) == prov_abbr) |>
      make_fcst_zone_grids(
        prov_abbr = prov_abbr,
        plot_caption = plot_caption
      ) |>
      ggplot2::ggsave(
        filename = plot_paths[[prov_abbr]],
        height = fig_dims$h,
        width = fig_dims$w,
        units = fig_dims$u,
        dpi = 300
      )
    invisible()
  })
  return(plot_paths)
}

make_prov_terr_grids <- function(
  grid_data,
  monitor_group,
  plot_caption = ""
) {
  stats <- names(grid_data)
  stats |>
    lapply(\(stat) {
      is_last_stat <- stat == stats[length(stats)]
      caption <- ifelse(is_last_stat, plot_caption, "")
      plot <- grid_data[[stat]] |>
        dplyr::filter(
          monitor == monitor_group | monitor_group == "FEM and PA"
        ) |>
        make_grid_plot(xlab = "Hour of Day", stat = "", caption = caption) +
        ggplot2::labs(title = stat |> stringr::str_to_title())
      if (!is_last_stat) {
        plot <- plot + ggplot2::theme(legend.position = "none")
      }
      return(plot)
    }) |>
    patchwork::wrap_plots(nrow = 1, guides = "collect")
}

make_fcst_zone_grids <- function(
  grid_data,
  prov_abbr = "BC",
  plot_caption = ""
) {
  grid_data |>
    dplyr::arrange(y) |>
    make_grid_plot(
      xlab = "Hour of Day",
      ylab = paste(prov_abbr, "Forecast Zone"),
      stat = "Median",
      small_text = TRUE,
      caption = plot_caption
    )
}

make_grid_summary_text <- function(grid_data) {
  grid_data |>
    subset(y != "Not inside a zone") |>
    dplyr::summarise(n = sum(as.numeric(fill) > 3), .by = c(z, y)) |>
    subset(n >= 3) |>
    dplyr::arrange(factor(z, prov_order), desc(n))

  if (nrow(text) == 0) {
    ""
  } else {
    "- %s: %s" |>
      sprintf(
        unique(text$z),
        unique(text$z) |>
          sapply(\(p) {
            text[text$z == p, "y"] |> join_list_sentence(oxford = TRUE)
          })
      ) |>
      paste(collapse = "\n\n")
  }
}
