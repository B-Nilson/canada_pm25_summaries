make_and_save_prov_terr_grids <- function(
  grid_data,
  report_dir,
  figure_dir,
  monitor_groups,
  plot_timestamp,
  xlab = "Hour of Day",
  plot_captions,
  fig_dims = list(h = 3.5, w = 11, u = 'in')
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
          xlab = xlab,
          plot_caption = plot_captions[[monitor_group]]
        ) |>
        ggplot2::ggsave(
          filename = plot_paths[[monitor_group]],
          height = fig_dims$h,
          width = fig_dims$w,
          units = fig_dims$u,
          dpi = 300
        )
      invisible()
    })

  plot_paths |> setNames(monitor_groups)
}

make_and_save_fcst_zone_grids <- function(
  grid_data,
  report_dir,
  figure_dir,
  plot_timestamp,
  xlab = "Hour of Day",
  plot_caption = "",
  fig_dims = list(h = 7, w = 11, u = 'in'),
  provinces_n_territories
) {
  # Make paths to plot files
  plot_paths <- "%s/zone_median_grid_%s_%s.png" |>
    sprintf(
      file.path(report_dir, figure_dir),
      provinces_n_territories,
      plot_timestamp
    ) |>
    setNames(provinces_n_territories) |>
    as.list()

  # Make and save plots
  lapply(names(provinces_n_territories), \(prov_name) {
    prov_abbr <- provinces_n_territories[[prov_name]]
    grid_data |>
      subset(as.character(z) == prov_abbr) |>
      make_fcst_zone_grids(
        prov_abbr = prov_abbr,
        xlab = xlab,
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
  xlab = "Hour of Day",
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
        make_grid_plot(xlab = xlab, stat = "", caption = caption) +
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
  xlab = "Hour of Day",
  plot_caption = ""
) {
  grid_data |>
    dplyr::arrange(y) |>
    make_grid_plot(
      xlab = xlab,
      ylab = paste(prov_abbr, "Forecast Zone"),
      stat = "Median",
      small_text = TRUE,
      caption = plot_caption
    )
}

make_grid_plot <- function(
  plot_data,
  ylab = "Province / Territory",
  xlab = "Day of Month",
  stat = "Median",
  caption = caption,
  small_text = FALSE,
  x = "discrete"
) {
  aqhi_values <- c(1:10, "+")
  aqhi_colours <- aqhi::get_aqhi_colours(aqhi_values)
  gg <- plot_data |>
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, fill = fill)) +
    ggplot2::geom_tile(colour = "black") +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(0)) +
    ggplot2::scale_fill_manual(
      values = aqhi_colours |> setNames(c(1:10, "+")),
      breaks = aqhi_values,
      drop = FALSE,
      na.value = "white",
      labels = c(paste(0:9 * 10, 1:10 * 10, sep = " - "), ">100")
    ) +
    ggpubr::theme_pubr(border = TRUE) +
    ggplot2::theme(
      legend.position = "right",
      axis.text.y = ggplot2::element_text(
        angle = 0,
        size = ifelse(small_text, 8, 11)
      ),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)
    ) +
    ggplot2::labs(
      y = ylab,
      x = xlab,
      fill = bquote(atop(.(paste(stat, "PM"))[2.5], "(" * mu * g ~ m^-3 * ")")),
      caption = caption
    )
  if (x == "discrete") {
    gg + ggplot2::scale_x_discrete(expand = ggplot2::expansion(0))
  } else {
    gg + ggplot2::scale_x_datetime(expand = ggplot2::expansion(0))
  }
}

make_grid_data <- function(
  obs,
  FUN = median,
  type = c("daily", "monthly", "seasonal")[1],
  fcst_zones = NULL
) {
  if (type == "daily") {
    FUN2 <- lubridate::hour
  } else if (type == "monthly") {
    FUN2 <- lubridate::day
  } else {
    stop("NOT TESTED YET")
  }

  time_order <- obs |>
    dplyr::arrange(date) |>
    dplyr::pull(date) |>
    FUN2() |>
    unique()
  out <- obs |>
    dplyr::bind_rows(obs |> dplyr::mutate(monitor == "FEM and PA")) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      prov_terr = prov_terr |>
        factor(
          levels = levels(prov_terr),
          labels = canadata::provinces_and_territories$abbreviation
        )
    )
  if (is.null(fcst_zones)) {
    out <- out |>
      dplyr::group_by(
        y = prov_terr,
        x = FUN2(date) |> factor(time_order),
        monitor
      ) |>
      dplyr::summarise(
        fill = FUN(pm25, na.rm = TRUE) |> aqhi::AQHI_plus(detailed = FALSE),
        .groups = "drop"
      ) |>
      tidyr::complete(
        y = levels(y) |> factor(levels = levels(y)),
        monitor = monitor,
        x = time_order |> factor(time_order)
      )
  } else {
    f <- \(y) {
      c("Not inside a zone", y[y != "Not inside a zone"])
    }
    fcst_zones_clean <- fcst_zones |>
      handyr::sf_as_df() |>
      seperate_fcst_zone_provs()
    out <- out |>
      dplyr::group_by(
        x = FUN2(date) |> factor(time_order),
        y = fcst_zone,
        z = prov_terr,
        monitor
      ) |>
      dplyr::summarise(
        fill = FUN(pm25, na.rm = TRUE) |> aqhi::AQHI_plus(detailed = FALSE),
        .groups = "drop"
      ) |>
      tidyr::complete(
        monitor,
        x = time_order |> factor(time_order),
        tidyr::nesting(
          y = fcst_zones_clean$fcst_zone,
          z = fcst_zones_clean$prov_terr
        )
      ) |>
      dplyr::mutate(
        all_missing = all(is.na(fill)),
        .by = c(z, y),
      ) |>
      dplyr::mutate(
        y = y |>
          dplyr::replace_values(NA ~ "Not inside a zone") |>
          factor(
            levels = c(sort(y[!all_missing]), sort(y[all_missing])) |>
              unique() |>
              f()
          )
      )
  }
  return(out)
}
