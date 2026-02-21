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

  plot_data <- stats |>
    setNames(stats) |>
    lapply(\(stat) {
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
            networks = group_name,
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
  
  plot_paths <- stats |> 
    setNames(stats) |>
    lapply(\(stat) {
      is_stat <- names(plot_paths) |> startsWith(stat)
      stat_plot_names <- names(plot_paths)[is_stat]
      new_names <- stat_plot_names |>
        stringr::str_replace(stat |> paste0("_"), "") |> 
        stringr::str_replace_all("_", " ") |> 
        stringr::str_replace("only", "Only") |> 
        stringr::str_replace("pa", "PA") |> 
        stringr::str_replace("fem", "FEM")
      plot_paths[is_stat] |>
        setNames(new_names)
    })
  
  return(
    list(
      data = plot_data,
      paths = plot_paths
    )
  )
}

make_donut_plot <- function(
  plot_data,
  labels,
  plot_caption = NULL,
  networks = "FEM and PA",
  stat = "Mean",
  avg = "24-hour"
) {
  legend_position <- c(4 / 5, 1 / 5.5)
  base_outline <- data.frame(xmin = 3, xmax = 4, ymin = 0, ymax = 1)
  outline_colour <- "black"

  base_plot <- plot_data |>
    ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = base_outline,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      colour = outline_colour,
      fill = NA
    ) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::theme_void() +
    ggplot2::xlim(c(2, 4)) +
    ggplot2::facet_wrap(~prov_terr, nrow = 3) +
    ggplot2::labs(
      fill = bquote(.(avg) ~ .(stat) ~ "PM"[2.5] ~ (mu * "g m"^-3)),
      title = bquote(
        .(networks) ~ "Monitor Counts and Site" ~ .(stat) ~ "PM"[2.5] ~
          "Distributions by Province/Territory"
      ),
      caption = plot_caption
    ) +
    ggplot2::theme(
      legend.position = legend_position,
      legend.direction = "horizontal",
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.subtitle = ggplot2::element_text(
        margin = ggplot2::margin(b = 6, unit = "pt")
      )
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        nrow = 2,
        byrow = FALSE,
        title.position = 'top'
      )
    )

  base_plot +
    # Add AQHI coloured bars
    ggplot2::geom_rect(
      xmin = base_outline$xmin,
      xmax = base_outline$xmax,
      ggplot2::aes(ymin = ymin, ymax = ymax, fill = aqhi_p),
      colour = outline_colour
    ) +
    ggplot2::scale_fill_manual(
      values = aqhi::get_aqhi_colours(c(1, 4, 7, 11)),
      breaks = levels(plot_data$aqhi_p)
    ) +
    # Add percentage labels where > 5%
    ggplot2::geom_text(
      x = mean(c(base_outline$xmin, base_outline$xmax)),
      ggplot2::aes(
        color = as.numeric(aqhi_p) == 4,
        label = ifelse(p > 0.05, paste0(round(p * 100), "%"), NA),
        y = label_pos
      ),
      hjust = 0.5,
      vjust = 0.5,
      show.legend = FALSE,
      size = 3
    ) +
    ggplot2::scale_colour_manual(
      values = c("FALSE" = "black", "TRUE" = "white")
    ) +
    # Add monitor count labels
    ggplot2::geom_text(
      data = labels,
      ggplot2::aes(label = n),
      x = 2,
      y = 0,
      hjust = 0.5,
      vjust = 0.5
    )
}

make_donut_data <- function(overall_summary, pm25_col = "pm25_mean") {
  overall_summary |>
    dplyr::bind_rows(
      overall_summary |> dplyr::mutate(monitor = "FEM and PA")
    ) |>
    dplyr::mutate(
      aqhi_p = aqhi::AQHI_plus(get(pm25_col))$risk
    ) |>
    tidyr::complete(
      prov_terr = levels(prov_terr) |> factor(levels = levels(prov_terr)),
      monitor,
      aqhi_p = levels(aqhi_p) |> factor(levels = levels(aqhi_p))
    ) |>
    dplyr::summarise(
      n = sum(!is.na(get(pm25_col))),
      .by = c(prov_terr, monitor, aqhi_p)
    ) |>
    dplyr::arrange(prov_terr, monitor, aqhi_p) |>
    dplyr::mutate(
      p = round(n / sum(n), 3),
      p = ifelse(is.na(p), 0, p),
      ymax = cumsum(p),
      ymin = c(0, head(ymax, n = -1)),
      label_pos = (ymin + ymax) / 2,
      aqhi_p = paste(aqhi_p, "Risk") |>
        factor(levels = levels(aqhi_p) |> paste("Risk")),
      .by = c(prov_terr, monitor)
    )
}
