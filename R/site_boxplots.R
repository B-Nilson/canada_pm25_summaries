make_and_save_site_boxplots <- function(
  overall_summary,
  monitor_groups,
  date_range,
  date_format,
  avg_text,
  plot_timestamp,
  report_dir,
  figure_dir,
  lib_dir
) {
  monitor_groups_cleaned <- monitor_groups |>
    stringr::str_to_lower() |>
    stringr::str_replace_all(" ", "_")
  # Create paths for plots
  plot_paths <- "%s/site_mean_boxplots_%s_%s.html" |>
    sprintf(
      report_dir |> file.path(figure_dir),
      monitor_groups_cleaned,
      plot_timestamp
    ) |>
    setNames(names(monitor_groups)) |>
    as.list()

  # Make and save plots
  boxplot_data <- overall_summary |>
    make_site_boxplot_data()
  summary_values <- lapply(names(monitor_groups), \(monitor_group) {
    plot_data <- boxplot_data |>
      subset(monitor == monitor_group | monitor_group == "FEM and PA")
    plot_data |>
      make_site_boxplots(
        date_range = date_range,
        date_format = date_format,
        avg = avg_text,
        monitor_group = monitor_group
      ) |>
      htmlwidgets::saveWidget(
        plot_paths[[monitor_group]],
        libdir = lib_dir,
        selfcontained = FALSE
      )

    list(
      # Worst site by MAX
      plot_data |>
        dplyr::mutate(name = escape_md(name)) |>
        dplyr::arrange(desc(pm25_max)) |>
        head(1) |>
        dplyr::select(
          M = name,
          T = monitor,
          D = nc_dist_km,
          C = nearest_community,
          P = prov_terr,
          PM = pm25_max
        ),
      # Worst site by MEAN
      plot_data |>
        dplyr::mutate(name = escape_md(name)) |>
        dplyr::arrange(desc(pm25_mean)) |>
        head(1) |>
        dplyr::select(
          M = name,
          T = monitor,
          D = nc_dist_km,
          C = nearest_community,
          P = prov_terr,
          PM = pm25_mean
        )
    )
  }) |>
    setNames(names(monitor_groups))
  list(
    paths = plot_paths |> setNames(monitor_groups),
    summary_values = summary_values
  )
}

make_site_boxplots <- function(
  boxplot_data,
  date_range,
  date_format,
  avg,
  monitor_group
) {
  plot_background_fills <- boxplot_data$pm25_mean |>
    make_plotly_aqhi_background()

  date_range <- date_range |> format(date_format)
  plot_title <- "%s Mean Distributions by Province/Territory for %s to %s" |>
    sprintf(monitor_group, date_range[1], date_range[2])
  axes_titles <- list(
    x = "Province | Territory",
    y = "%s Site Mean PM2.5 (ug/m3)" |> sprintf(avg)
  )

  box <- boxplot(pm25_mean ~ prov_terr, data = boxplot_data, plot = FALSE)
  outliers <- boxplot_data |>
    dplyr::filter(pm25_mean > box$stats[4, as.numeric(prov_terr)])
  boxplot_data |>
    plotly::plot_ly(y = ~ round(pm25_mean, 1), x = ~prov_terr) |>
    plotly::add_boxplot(
      name = "",
      boxpoints = FALSE,
      color = I('black'),
      alpha = 0.2
    ) |>
    plotly::layout(
      title = plot_title,
      shapes = plot_background_fills,
      showlegend = FALSE,
      xaxis = list(title = axes_titles$x, zeroline = TRUE, showgrid = FALSE),
      yaxis = list(title = axes_titles$y, zeroline = TRUE, showgrid = FALSE)
    ) |>
    plotly::add_markers(
      data = outliers,
      text = ~ hours_to_summarise |>
        make_boxplot_hover(
          name = name,
          monitor = monitor,
          pm25_mean = pm25_mean,
          pm25_max = pm25_max,
          n_hours_above_60 = n_hours_above_60,
          n_hours_above_100 = n_hours_above_100,
          nearest_community = nearest_community,
          nc_dist_km = nc_dist_km,
          fcst_zone = fcst_zone
        ),
      hoverinfo = 'text',
      marker = list(
        color = "rgb(107,174,214)",
        line = list(color = "black", width = 1)
      )
    )
}

make_site_boxplot_data <- function(overall_summary) {
  overall_summary |>
    dplyr::mutate(aqhi_p_mean = aqhi::AQHI_plus(pm25_mean, detailed = FALSE)) |>
    tidyr::complete(
      prov_terr = levels(prov_terr) |> factor(levels = levels(prov_terr)),
      monitor
    ) |>
    dplyr::mutate(
      prov_terr = prov_terr |>
        factor(
          levels = levels(prov_terr),
          labels = canadata::provinces_and_territories$abbreviation
        )
    )
}

make_boxplot_hover <- function(
  hours_to_summarise,
  name,
  monitor,
  pm25_mean,
  pm25_max,
  n_hours_above_60,
  n_hours_above_100,
  nearest_community,
  nc_dist_km,
  fcst_zone
) {
  paste(
    sep = "<br>",
    "<b>%s</b>",
    "%s Monitor",
    "24-hour mean: %s ug/m3",
    "24-hour max: %s ug/m3",
    "# hours >60 ug/m3: %s / %s",
    "# hours >100 ug/m3: %s / %s",
    "Closest Community: %s (~%s km)",
    "Forecast Zone: %s"
  ) |>
    sprintf(
      name,
      monitor,
      pm25_mean,
      pm25_max,
      n_hours_above_60,
      length(hours_to_summarise),
      n_hours_above_100,
      length(hours_to_summarise),
      nearest_community,
      nc_dist_km,
      fcst_zone
    )
}

make_plotly_aqhi_background <- function(pm25_values) {
  min_pm25_per_aqhi <- 0:10 * 10
  aqhi_colours <- aqhi::get_aqhi_colours(1:11)
  max_observed_ceiling_10 <- ceiling(max(pm25_values, na.rm = TRUE) / 10) * 10
  1:(max_observed_ceiling_10 / 10) |>
    lapply(\(aqhi_value) {
      list(
        type = "rect",
        fillcolor = aqhi_colours[aqhi_value],
        line = list(width = 0),
        opacity = 1,
        x0 = -0.5,
        x1 = 12.5,
        layer = 'below',
        y0 = min_pm25_per_aqhi[aqhi_value],
        y1 = c(min_pm25_per_aqhi[-1], max_observed_ceiling_10)[aqhi_value]
      )
    })
}
