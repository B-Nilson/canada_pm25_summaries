make_and_save_prov_terr_grids <- function(
  grid_data,
  report_dir,
  figure_dir,
  monitor_groups,
  plot_timestamp,
  xlab = "Hour of Day",
  fig_dims = list(h = 4.5, w = 11, u = 'in')
) {
  monitor_groups_cleaned <- monitor_groups |>
    stringr::str_to_lower() |>
    stringr::str_replace_all(" ", "_")
  # Make paths to plot files
  plot_paths <- "%s/prov_median-peak_grid_%s_%s.svg" |>
    sprintf(
      file.path(report_dir, figure_dir),
      monitor_groups_cleaned,
      plot_timestamp
    ) |>
    setNames(monitor_groups) |>
    as.list()

  # Make and save plots
  monitor_groups |>
    lapply(\(monitor_group) {
      grid_data |>
        make_prov_terr_grids(
          monitor_group = monitor_group,
          xlab = xlab
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
  fig_dims = list(w = 11, u = 'in'),
  provinces_n_territories
) {
  # Make paths to plot files
  plot_paths <- "%s/zone_median_max_grid_%s_%s.svg" |>
    sprintf(
      file.path(report_dir, figure_dir),
      provinces_n_territories,
      plot_timestamp
    ) |>
    setNames(provinces_n_territories) |>
    as.list()

  plot_heights <- list(
    BC = 11,
    AB = 10,
    SK = 9.5,
    MB = 9.25,
    ON = 14,
    QC = 17,
    NB = 7,
    NS = 8,
    NL = 9.5,
    PE = 4,
    YT = 5,
    NT = 7,
    NU = 7.5 
  )

  # Make and save plots
  lapply(provinces_n_territories, \(prov_abbr) {
    prov_name <- names(provinces_n_territories)[
      provinces_n_territories == prov_abbr
    ]
    pt_data <- grid_data |>
      lapply(\(x) x |> dplyr::filter(z == prov_abbr))

    if (is.null(fig_dims$h)) {
      h <- plot_heights[[prov_abbr]]
    } else {
      h <- fig_dims$h
    }

    pt_data |>
      make_fcst_zone_grids(
        ylab = paste("Region of", prov_name),
        xlab = xlab,
        monitor_group = "FEM and PA",
        ylab_wrap_width = 32
      ) |>
      ggplot2::ggsave(
        filename = plot_paths[[prov_abbr]],
        height = h,
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
  xlab = "Hour of Day"
) {
  names(grid_data) |>
    lapply(\(stat) {
      plot <- grid_data[[stat]] |>
        dplyr::filter(
          monitor %in% stringr::str_split_1(monitor_group, ", | and ")
        ) |>
        make_grid_plot(xlab = xlab) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 9),
          plot.subtitle = ggplot2::element_text(
            margin = ggplot2::margin_part(b = 2)
          )
        ) +
        ggplot2::labs(
          subtitle = bquote(
            .(stringr::str_to_title(stat)) ~ .(monitor_group) ~ "PM"[
              2.5
            ] ~ "Concentration"
          )
        )
      return(plot)
    }) |>
    patchwork::wrap_plots(
      nrow = 1,
      guides = "collect",
      axis_titles = "collect"
    ) &
    ggplot2::guides(
      fill = ggplot2::guide_legend(nrow = 1, label.position = "bottom")
    ) &
    ggplot2::theme(
      legend.position = "bottom",
      legend.key.width = ggplot2::unit(1.5, "cm"),
      legend.key.spacing.x = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(1, 1, 1, 1, "mm")
    )
}

make_fcst_zone_grids <- function(
  grid_data,
  ylab,
  xlab = "Hour of Day",
  monitor_group,
  ylab_wrap_width = 48
) {
  names(grid_data) |>
    lapply(\(stat) {
      plot <- grid_data[[stat]] |>
        dplyr::arrange(y) |>
        make_grid_plot(
          xlab = xlab,
          ylab = ylab,
          small_text = TRUE,
          ylab_wrap_width = ylab_wrap_width
        ) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 9),
          plot.subtitle = ggplot2::element_text(
            margin = ggplot2::margin_part(b = 2)
          )
        ) +
        ggplot2::labs(
          subtitle = bquote(
            .(stringr::str_to_title(stat)) ~ .(monitor_group) ~
              "PM"[2.5] ~ "Concentration"
          )
        )
      return(plot)
    }) |>
    patchwork::wrap_plots(
      nrow = 1,
      guides = "collect",
      axis_titles = "collect",
      axes = "collect"
    ) &
    ggplot2::guides(
      fill = ggplot2::guide_legend(nrow = 1, label.position = "bottom")
    ) &
    ggplot2::theme(
      legend.position = "bottom",
      legend.key.width = ggplot2::unit(1.5, "cm"),
      legend.key.spacing.x = ggplot2::unit(0, "cm")
    )
}

clean_fcst_zone_names <- function(zone_names) {
  zone_names |>
    stringr::str_remove_all(" sections$") |>
    stringr::str_remove_all("^B\\.C\\. ") |>
    stringr::str_remove_all("P\\.E\\.I\\.$") |>
    stringr::str_remove_all("City of ") |>
    stringr::str_remove_all(" and vicinity$") |>
    stringr::str_remove_all(" and area$") |>
    stringr::str_remove_all(" Region") |>
    stringr::str_remove_all(" Area$") |>
    stringr::str_replace_all(" including", ",")
}

make_grid_plot <- function(
  plot_data,
  ylab = "Province / Territory",
  xlab = "Day of Month",
  small_text = FALSE,
  x = "discrete",
  ylab_wrap_width = 48
) {
  aqhi_values <- c(1:10, "+")
  aqhi_colours <- aqhi::get_aqhi_colours(aqhi_values)
  gg <- plot_data |>
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, fill = fill)) +
    ggplot2::geom_tile(colour = "black", show.legend = TRUE) +
    ggplot2::scale_y_discrete(
      expand = ggplot2::expansion(0),
      labels = \(x) stringr::str_wrap(x, width = ylab_wrap_width)
    ) +
    ggplot2::scale_fill_manual(
      values = aqhi_colours |> setNames(c(1:10, "+")),
      breaks = aqhi_values,
      drop = FALSE,
      na.value = "white",
      labels = c(paste(0:9 * 10, 1:10 * 10, sep = " - "), ">100")
    ) +
    ggpubr::theme_pubr(border = TRUE, base_family = "Inter") +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(
        angle = 0,
        size = ifelse(small_text, 8, 11)
      )
      # axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)
    ) +
    ggplot2::labs(
      y = ylab,
      x = xlab,
      fill = bquote("PM"[2.5] ~ "(" * mu * g ~ m^-3 * ")")
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
    dplyr::bind_rows(
      obs |>
        dplyr::mutate(monitor == monitor |> unique() |> join_list_sentence())
    ) |>
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
      c("Not inside a defined zone", y[y != "Not inside a defined zone"])
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
        y = y |>
          dplyr::replace_values(NA ~ "Not inside a defined zone") |>
          factor(
            levels = c("Not inside a defined zone", sort(unique(y))),
            labels = c("Not inside a defined zone", sort(unique(y))) |>
              clean_fcst_zone_names()
          )
      )
  }
  return(out)
}
