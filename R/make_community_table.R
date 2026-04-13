make_community_table <- function(
  community_summary,
  type,
  data_dir,
  figure_dir,
  plot_timestamp,
  table_caption
) {
  key_names <- c(
    name = "nearest_community",
    zone = "fcst_zone",
    fem = "n_fem",
    pa = "n_pa",
    d_mean = "nc_dist_km_network_mean_comm_mean",
    d_max = "nc_dist_km_network_max_comm_max",
    h_30 = "n_hours_above_30_network_max_comm_max",
    h_60 = "n_hours_above_60_network_max_comm_max",
    h_100 = "n_hours_above_100_network_max_comm_max",
    pm_last = "pm25_current_network_mean_comm_mean",
    pm_mean = "pm25_mean_network_mean_comm_mean",
    pm_max = "pm25_max_network_max_comm_max"
  )
  display_names <- list(
    name = "Name",
    zone = "Region",
    fem = "FEM",
    pa = "PA",
    d_mean = "Mean Dist.",
    d_max = "Max Dist.",
    h_30 = gt::md("30 &mu;g/m^3^"),
    h_60 = gt::md("60 &mu;g/m^3^"),
    h_100 = gt::md("100 &mu;g/m^3^"),
    pm_last = "Last",
    pm_mean = "Mean",
    pm_max = "Max"
  )

  table_data <- community_summary |>
    dplyr::filter(nc_dist_km_network_mean_comm_mean <= 20) |>
    dplyr::arrange(desc(pm25_mean_network_mean_comm_mean)) |>
    dplyr::mutate(
      prov_terr = prov_terr |>
        factor(
          levels = levels(prov_terr),
          labels = canadata::provinces_and_territories$abbreviation
        ),
      fcst_zone = prov_terr |> paste0(": ", fcst_zone),
      n_pa = n_monitors |> stringr::str_extract("PA: (\\d+)", group = 1),
      n_fem = n_monitors |> stringr::str_extract("FEM: (\\d+)", group = 1),
      dplyr::across(c(n_pa, n_fem), \(x) {
        x |> as.numeric() |> dplyr::replace_values(NA ~ 0)
      }),
      nearest_community = "<a title=\"%s\" href=\"https:/aqmap.ca/aqmap/#12/%s/%s\">%s</a>" |>
        sprintf(
          nearest_community |> gsub(pattern = '"', replacement = "&quot;"),
          nearest_community,
          nc_lng |> round(digits = 4),
          nc_lat |> round(digits = 4)
        )
    ) |>
    dplyr::arrange(dplyr::desc(pm25_mean_network_mean_comm_mean)) |>
    dplyr::select(dplyr::all_of(key_names)) |>
    dplyr::mutate(dplyr::across("zone", \(x) {
      "<span title=\"%s\">%s</span>" |>
        sprintf(x |> gsub(pattern = '"', replacement = "&quot;"), x)
    }))

  css <- ".rt-text-content {
  overflow-x: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}" |>
    htmltools::tags$style(type = "text/css")

  community_table <- table_data |>
    gt::gt() |>
    gt::opt_interactive(use_filters = TRUE) |>
    gt::cols_width(
      name ~ gt::px(120),
      zone ~ gt::px(130),
      pa ~ gt::px(50),
      fem ~ gt::px(60),
      dplyr::starts_with("d_") ~ gt::px(104),
      dplyr::starts_with("h_") &
        !dplyr::starts_with("h_100") ~ gt::px(97),
      h_100 ~ gt::px(105),
      dplyr::starts_with("pm_") ~ gt::px(78)
    ) |>
    gt::tab_spanner(
      label = "Community Details",
      columns = c("name", "zone")
    ) |>
    gt::tab_spanner(
      label = gt::html("PM<sub>2.5</sub> Monitoring Sites"),
      columns = c(pa, fem, d_max, d_mean)
    ) |>
    gt::tab_spanner(
      label = gt::html("PM<sub>2.5</sub> Concentration (&mu;g m<sup>-3</sup>)"),
      columns = dplyr::starts_with("pm_")
    ) |>
    gt::tab_spanner(
      label = gt::html("Hours Above PM<sub>2.5</sub> Threshold"),
      columns = dplyr::starts_with("h_"),
      id = "hours_above_spanner"
    ) |>
    gt::cols_label(.list = display_names) |>
    gt::cols_align(dplyr::starts_with("h_"), align = "center") |>
    gt::fmt_number(dplyr::starts_with("pm_"), decimals = 1) |>
    gt::fmt_number(
      dplyr::starts_with("d_"),
      decimals = 1,
      pattern = "{x} km"
    ) |>
    gt::data_color(
      alpha = 0.6,
      columns = dplyr::starts_with("d_"),
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
      columns = dplyr::starts_with("h_"),
      palette = "plasma",
      domain = c(0, 24)
    ) |>
    gt::data_color(
      alpha = 0.6,
      columns = c("pa", "fem"),
      palette = "viridis",
      domain = c(0, max(c(table_data$pa, table_data$fem)))
    ) |>
    gt::data_color(
      alpha = 0.6,
      columns = dplyr::starts_with("pm_"),
      fn = \(x) x |> aqhi::get_aqhi_colours(types = "pm25_1hr")
    ) |>
    gt::sub_missing(dplyr::starts_with("h_") | dplyr::starts_with("pm_")) |>
    htmltools::as.tags() |>
    htmltools::tagList(css)

  # Save table to .html and data to .csv, link within a plot_card
  data_path <- "%s/%s/%s/community_summary_%s.csv" |>
    sprintf(type, data_dir, plot_timestamp, plot_timestamp)
  table_name <- "community_summary_%s.html" |>
    sprintf(plot_timestamp)
  table_path_tmp <- type |>
    file.path(figure_dir, table_name)
  table_path <- type |>
    file.path(figure_dir, plot_timestamp, table_name)

  community_table |>
    make_table_card(
      table_data = table_data,
      table_caption = table_caption,
      table_path_tmp = table_path_tmp,
      table_path = table_path,
      data_path = data_path,
      data_rel_dir = data_dir,
      plot_timestamp = plot_timestamp,
      type = type
    )
}
