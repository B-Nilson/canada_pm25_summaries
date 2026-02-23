make_community_table <- function(
  community_summary,
  report_dir,
  data_dir,
  figure_dir,
  plot_timestamp
) {
  display_names <- list(
    nearest_community = "Name",
    prov_terr = "P/T",
    fcst_zone = "Forecast Zone",
    n_fem = "FEM",
    n_pa = "PA",
    nc_dist_km_network_mean_comm_mean = "Mean Dist.",
    nc_dist_km_network_max_comm_max = "Max Dist.",
    n_hours_above_30_network_max_comm_max = gt::md("30 &mu;g/m<sup>3</sup>"),
    n_hours_above_60_network_max_comm_max = gt::md("60 &mu;g/m<sup>3</sup>"),
    n_hours_above_100_network_max_comm_max = gt::md("100 &mu;g/m<sup>3</sup>"),
    pm25_current_network_mean_comm_mean = "Last",
    pm25_mean_network_mean_comm_mean = "Mean",
    pm25_max_network_max_comm_max = "Max"
  )

  table_data <- community_summary |>
    dplyr::arrange(desc(pm25_mean_network_mean_comm_mean)) |>
    dplyr::mutate(
      prov_terr = prov_terr |>
        factor(
          levels = levels(prov_terr),
          labels = canadata::provinces_and_territories$abbreviation
        ),
      n_pa = n_monitors |> stringr::str_extract("PA: (\\d+)", group = 1),
      n_fem = n_monitors |> stringr::str_extract("FEM: (\\d+)", group = 1),
      dplyr::across(c(n_pa, n_fem), \(x) {
        x |> as.numeric() |> dplyr::replace_values(NA ~ 0)
      }),
      aqmap_link = nc_lat |>
        make_aqmap_link(lng = nc_lng, zoom = 12, lang = "EN"),
      nearest_community = "<a href='%s'>%s</a>" |>
        sprintf(aqmap_link, nearest_community),
      dplyr::across(c(fcst_zone, nearest_community), \(x) abbrev_text(x))
    ) |>
    dplyr::select(dplyr::all_of(names(display_names))) |>
    dplyr::arrange(dplyr::desc(pm25_mean_network_mean_comm_mean))

  community_table <- table_data |>
    gt::gt() |>
    gt::opt_interactive() |>
    gt::cols_width(
      nearest_community ~ gt::px(120),
      prov_terr ~ gt::px(60),
      fcst_zone ~ gt::px(130),
      n_pa ~ gt::px(50),
      n_fem ~ gt::px(60),
      dplyr::starts_with("nc_dist") ~ gt::px(104),
      dplyr::starts_with("n_hours") & !dplyr::starts_with("n_hours_above_100") ~ gt::px(97),
      dplyr::starts_with("n_hours_above_100") ~ gt::px(105),
      dplyr::starts_with("pm25") ~ gt::px(78)
    ) |>
    gt::tab_spanner(
      label = "Community Details",
      columns = c("nearest_community", "prov_terr", "fcst_zone")
    ) |>
    gt::tab_spanner(
      label = gt::md("PM<sub>2.5</sub> Monitoring Sites"),
      columns = c(
        n_pa,
        n_fem,
        nc_dist_km_network_max_comm_max,
        nc_dist_km_network_mean_comm_mean
      )
    ) |>
    gt::tab_spanner(
      label = gt::md("PM<sub>2.5</sub> Concentration (&mu;g m<sup>-3</sup>)"),
      columns = dplyr::starts_with("pm25")
    ) |>
    gt::tab_spanner(
      label = gt::md("Hours Above PM<sub>2.5</sub> Threshold"),
      columns = dplyr::starts_with("n_hours"),
      id = "hours_above_spanner"
    ) |>
    gt::cols_label(.list = display_names) |>
    gt::cols_align(dplyr::starts_with("n_"), align = "center") |>
    gt::fmt_number(dplyr::starts_with("pm25"), decimals = 1) |>
    gt::fmt_number(
      dplyr::starts_with("nc_dist_km"),
      decimals = 1,
      pattern = "{x} km"
    ) |>
    gt::data_color(
      alpha = 0.6,
      columns = dplyr::starts_with("nc_dist_km"),
      fn = function(x) {
        leaflet::colorNumeric(
          reverse = TRUE,
          palette = "viridis",
          domain = range(log(x + 1))
        )(log(x + 1))
      }
    ) |>
    gt::data_color(
      alpha = 0.6,
      columns = dplyr::starts_with("n_hours"),
      palette = "plasma",
      domain = c(0, 24)
    ) |>
    gt::data_color(
      alpha = 0.6,
      columns = dplyr::starts_with("n_") & !dplyr::starts_with("n_hours"),
      palette = "viridis",
      domain = c(0, max(c(table_data$n_pa, table_data$n_fem)))
    ) |>
    gt::data_color(
      alpha = 0.6,
      columns = dplyr::starts_with("pm25"),
      fn = \(x) x |> aqhi::get_aqhi_colours(types = "pm25_1hr")
    ) |>
    gt::sub_missing(dplyr::starts_with("n_hours") | dplyr::starts_with("pm25"))

  # Write out data to CSV and make download button linked to it
  file_path <- "%s/%s/community_summary_%s.csv" |>
    sprintf(report_dir, data_dir, plot_timestamp)
  dl_button <- table_data |>
    make_download_button(data_dir = data_dir, file_path = file_path)

  table_path <- "%s/%s/community_summary_%s.html" |>
    sprintf(report_dir, figure_dir, plot_timestamp)
  community_table |> gt::gtsave(filename = table_path)

  list(
    html = community_table,
    path = table_path,
    dl_button = dl_button
  )
}
