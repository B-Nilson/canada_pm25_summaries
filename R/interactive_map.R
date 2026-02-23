make_and_save_maps <- function(
  overall_summary,
  zone_summary,
  monitor_groups,
  include_active_fires = FALSE,
  report_dir,
  plot_dir,
  lib_dir = "libs"
) {
  map_data <- overall_summary |>
    make_map_data()

  # Make paths to map files
  plot_names <- "site_mean_map_%s_%s.html" |>
    sprintf(
      monitor_groups |>
        stringr::str_to_lower() |>
        stringr::str_replace_all(" ", "_"),
      plot_timestamp
    )
  plot_paths <- report_dir |>
    file.path(plot_dir, plot_names) |>
    setNames(monitor_groups) |>
    as.list()

  # Make and save maps
  monitor_groups |>
    lapply(\(monitor_group) {
      group_name <- names(monitor_groups)[monitor_groups == monitor_group]
      pd <- map_data
      if (monitor_group != "FEM and PA") {
        pd <- pd |>
          dplyr::filter(
            monitor ==
              ifelse(monitor_group != "FEM and PA", group_name, monitor)
          )
      }
      pd |>
        make_pm25_obs_map(
          zone_summaries = zone_summary,
          include_active_fires = include_active_fires
        ) |>
        aqmapr::save_map(
          save_to = plot_paths[[monitor_group]],
          library_dir = lib_dir,
          self_contained = FALSE
        )
    })

  return(plot_paths)
}

make_pm25_obs_map <- function(
  pd,
  zone_summaries,
  include_active_fires = TRUE,
  mean_pm25_col = 'mean_pm25_24hr_mean'
) {
  layers.all <- c("Observations", "Forecast Zone Mean", "Active Fires")
  if (!include_active_fires) {
    layers.all <- layers.all[-length(layers.all)]
  }

  aqhi_pal <- leaflet::colorBin(
    palette = aqhi::get_aqhi_colours(1:11),
    bins = c(0:10 * 10, Inf),
    right = FALSE,
    na.color = "#bbbbbb"
  )

  pm_legend <- '<div style="margin-bottom:3px"><strong><span>24-hour Mean<br>PM<sub>2.5</sub> (μg m<sup>-3</sup>)</span></strong></div><i style="background:#21C6F5;opacity:1"></i> [0 – 10)<br><i style="background:#189ACA;opacity:1"></i> [10 – 20)<br><i style="background:#0D6797;opacity:1"></i> [20 – 30)<br><i style="background:#FFFD37;opacity:1"></i> [30 – 40)<br><i style="background:#FFCC2E;opacity:1"></i> [40 – 50)<br><i style="background:#FE9A3F;opacity:1"></i> [50 – 60)<br><i style="background:#FD6769;opacity:1"></i> [60 – 70)<br><i style="background:#FF3B3B;opacity:1"></i> [70 – 80)<br><i style="background:#FF0101;opacity:1"></i> [80 – 90)<br><i style="background:#CB0713;opacity:1"></i> [90 – 100)<br><i style="background:#650205;opacity:1"></i> [100 – Inf)<br>'

  map <- pd |>
    leaflet::leaflet(height = 600, width = "100%") |>
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) |>
    leaflet::addMarkers(
      lng = ~lng,
      lat = ~lat,
      options = ~ leaflet::markerOptions(zIndexOffset = pm25_mean * 100),
      label = ~labels,
      group = layers.all[1],
      icon = ~ list(iconUrl = icon_link, iconHeight = 26, iconWidth = 26)
    ) |>
    leaflet::addPolygons(
      data = zone_summaries |> dplyr::filter(!is.na(fcst_zone_fr)),
      fillColor = ~ aqhi_pal(get(mean_pm25_col)),
      color = "black",
      weight = 1,
      fillOpacity = ~ ifelse(is.na(get(mean_pm25_col)), 0, 0.5),
      popupOptions = leaflet::popupOptions(minWidth = '330'),
      group = layers.all[2],
      popup = ~ make_map_popup(
        fcst_zone,
        mean_pm25_24hr_mean_FEM,
        mean_pm25_24hr_mean_PA,
        mean_pm25_24hr_mean,
        n_monitors_FEM,
        n_monitors_PA
      )
    ) |>
    # Add layers control menu
    leaflet::addLayersControl(
      overlayGroups = layers.all,
      position = 'topright'
    ) |>
    leaflet::addControl(html = pm_legend, position = 'bottomleft')

  if (include_active_fires) {
    active_fires <- get_active_fire_data()
    fire_pal <- leaflet::colorFactor(
      c("#ffb24c", "#00a2ff", "#fff300", "#f03b20"),
      levels = fire_states
    )
    map <- map |>
      leaflet::addLegend(
        "bottomright",
        fire_pal,
        factor(fire_states, fire_states),
        opacity = 0.8,
        title = layers.all[3]
      ) |>
      leaflet::addCircleMarkers(
        data = active_fires,
        group = layers.all[3],
        lat = ~lat,
        lng = ~lon,
        fill = TRUE,
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.8,
        opacity = 0.8,
        radius = ~ as.numeric(size) + 2,
        fillColor = ~ fire_pal(state),
        color = "black",
        options = leaflet::markerOptions(
          zIndexOffset = as.numeric(active_fires$state)
        ),
        popup = ~labels,
        popupOptions = leaflet::popupOptions()
      )
  }
  return(map)
}

make_map_data <- function(
  overall_summary,
  icon_dir = "https://aqmap.ca/aqmap/icons"
) {
  overall_summary |>
    dplyr::mutate(
      icon_link = paste0(
        "%s/icon_%s_%s.png" |>
          sprintf(
            icon_dir,
            ifelse(monitor == "FEM", 23, 21),
            ifelse(pm25_mean > 999, "+", round(pm25_mean))
          )
      ),
      labels = "<big><strong>Site: %s (%s)</strong></big>" |>
        c(
          "<b>Nearby Community:</b> %s (~%s km)",
          "<b>24hr mean PM<sub>2.5</sub>:</b> %s &mu;g m<sup>-3</sup>",
          "<b>24hr max PM<sub>2.5</sub>:</b> %s &mu;g m<sup>-3</sup>",
          "<b># Hours with PM<sub>2.5</sub> >= 60 &mu;g m<sup>-3</sup>:</b> %s"
        ) |>
        paste(collapse = "<br>") |>
        sprintf(
          name,
          monitor,
          nearest_community,
          nc_dist_km,
          pm25_mean,
          pm25_max,
          n_hours_above_60
        )
    )
}

make_map_popup <- function(
  name,
  pm25_24_fem,
  pm25_24_pa,
  pm25_24_all,
  n_fem,
  n_pa,
  language = "EN"
) {
  # Handle displayed text language
  text <- list(
    EN = c(
      'Forecast Zone',
      '# of Monitors',
      'Mean 24hr PM<sub>2.5</sub>'
    ),
    FR = c(
      'Zone de prévision',
      '# de moniteurs',
      'Moyenne sur 24 h PM<sub>2,5</sub>'
    )
  )[[language]]
  fem_label <- list(
    EN = 'FEM',
    FR = 'MEF'
  )[[language]]
  all_label <- list(
    EN = 'ALL',
    FR = 'TOUT'
  )[[language]]

  "
    <big><strong>%s: %s</strong></big>
    <table style='margin: auto;'>
      <thead><tr>
          <th style='text-align:center'></th>
          <th style='text-align:center'>%s</th>
          <th style='text-align:center'>PA</th>
          <th style='text-align:center'>%s</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td style='font-weight: bold;text-align:center'>%s</td>
          <td style='text-align:center'>%s</td>
          <td style='text-align:center'>%s</td>
          <td style='text-align:center'>%s</td>
        </tr>
        <tr>
          <td style='font-weight: bold;text-align:center'>%s</td>
          <td style='text-align:center'>%s &mu;g m<sup>-3</sup></td>
          <td style='text-align:center'>%s &mu;g m<sup>-3</sup></td>
          <td style='text-align:center'>%s &mu;g m<sup>-3</sup></td>
        </tr>
      </tbody>
    </table>" |>
    sprintf(
      text[1],
      name,
      fem_label,
      all_label,
      text[2],
      n_fem,
      n_pa,
      n_fem + n_pa,
      text[3],
      pm25_24_fem,
      pm25_24_pa,
      pm25_24_all
    ) |>
    stringr::str_replace_all(">NA &mu;g m<sup>-3</sup><", ">-<")
}

make_map_table_data <- function(obs_summary) {
  obs_summary |>
    dplyr::mutate(
      aqhi_p_cat_mean = aqhi::AQHI_plus(pm25_mean)$risk,
      aqhi_p_cat_max = aqhi::AQHI_plus(pm25_max)$risk,
      aqhi_p_cat_current = aqhi::AQHI_plus(pm25_current)$risk
    ) |>
    dplyr::mutate(
      aqmap_link = paste0(
        "https://aqmap.ca/aqmap/#12/",
        lat,
        "/",
        lng
      )
    ) |>
    dplyr::select(
      "Site Name" = name,
      "Monitor" = monitor,
      "Prov./Terr." = prov_terr,
      "Forecast Zone" = fcst_zone,
      "Closest Community" = nearest_community,
      "Dist. From Community\n(km)" = nc_dist_km,
      "# Hours (/24) >/= 60 ug/m3" = n_hours_above_60,
      "Current PM2.5\n(ug/m3)" = pm25_current,
      "24hr PM2.5 Mean\n(ug/m3)" = pm25_mean,
      "24hr PM2.5 Max\n(ug/m3)" = pm25_max,
      lng,
      lat,
      aqmap_link
    ) |>
    dplyr::arrange(dplyr::desc(`24hr PM2.5 Mean\n(ug/m3)`), `Prov./Terr.`) |>
    dplyr::filter(!duplicated(`Site Name`))
}

make_overall_summary_table <- function(
  table_data,
  monitor_group,
  report_dir,
  data_dir,
  figure_dir,
  plot_timestamp
) {
  dir.create(file.path(report_dir, data_dir), showWarnings = FALSE)
  display_names <- list(
    name = "Name",
    monitor = "Type",
    prov_terr = "P/T",
    fcst_zone = "Forecast Zone",
    nearest_community = "Name",
    nc_dist_km = "Distance",
    n_hours_above_30 = gt::md("30 &mu;g/m<sup>3</sup>"),
    n_hours_above_60 = gt::md("60 &mu;g/m<sup>3</sup>"),
    n_hours_above_100 = gt::md("100 &mu;g/m<sup>3</sup>"),
    pm25_current = "Last",
    pm25_mean = "Mean",
    pm25_max = "Max"
  )
  table_data <- table_data |>
    dplyr::mutate(
      prov_terr = prov_terr |>
        factor(
          levels = levels(prov_terr),
          labels = canadata::provinces_and_territories$abbreviation
        ),
      aqmap_link = make_aqmap_link(lat = lat, lng = lng),
      name = "<a href='%s'>%s</a>" |> sprintf(aqmap_link, name),
      dplyr::across(c(fcst_zone, name, nearest_community), \(x) abbrev_text(x))
    ) |>
    dplyr::select(dplyr::all_of(names(display_names))) |>
    dplyr::arrange(dplyr::desc(pm25_mean), pm25_current)

  table <- table_data |>
    gt::gt() |>
    gt::opt_interactive() |>
    gt::cols_width(
      monitor ~ gt::px(62),
      prov_terr ~ gt::px(60),
      fcst_zone ~ gt::px(130),
      nearest_community ~ gt::px(120),
      nc_dist_km ~ gt::px(90),
      dplyr::starts_with("n_hours") ~ gt::px(94),
      dplyr::starts_with("pm25") ~ gt::px(78)
    ) |>
    gt::tab_spanner(
      label = "Monitoring Site",
      columns = c("name", "monitor", "prov_terr", "fcst_zone")
    ) |>
    gt::tab_spanner(
      label = gt::md("PM<sub>2.5</sub> Concentration (&mu;g m<sup>-3</sup>)"),
      columns = dplyr::starts_with("pm25")
    ) |>
    gt::tab_spanner(
      label = "Nearest Community",
      columns = c("nearest_community", "nc_dist_km")
    ) |>
    gt::tab_spanner(
      label = gt::md("Hours Above PM<sub>2.5</sub> Threshold"),
      columns = dplyr::starts_with("n_hours"),
      id = "hours_above_spanner"
    ) |>
    gt::cols_label(.list = display_names) |>
    gt::cols_align(dplyr::starts_with("n_hours"), align = "center") |>
    gt::cols_align("nearest_community", align = "right") |>
    gt::cols_align("nc_dist_km", align = "left") |>
    gt::fmt_number(dplyr::starts_with("pm25"), decimals = 1) |>
    gt::fmt_number("nc_dist_km", decimals = 1, pattern = "{x} km") |>
    gt::data_color(
      alpha = 0.6,
      columns = nc_dist_km,
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
      columns = dplyr::starts_with("pm25"),
      fn = \(x) x |> aqhi::get_aqhi_colours(types = "pm25_1hr")
    ) |>
    gt::sub_missing(dplyr::starts_with("n_hours") | dplyr::starts_with("pm25"))

  # Save data to csv for download
  m_group_cleaned <- monitor_group |>
    stringr::str_to_lower() |>
    stringr::str_replace_all(" ", "_")
  file_path <- "%s/pm2.5_monitor_sites_%s_%s.csv" |>
    sprintf(
      file.path(report_dir, data_dir),
      m_group_cleaned,
      plot_timestamp
    )
  dl_button <- table_data |>
    make_download_button(data_dir = data_dir, file_path = file_path)

  # Save table to .html
  table_path <- "%s/overall_table_%s_%s.html" |>
    sprintf(
      file.path(report_dir, figure_dir),
      m_group_cleaned,
      plot_timestamp
    )
  table |> gt::gtsave(filename = table_path)

  list(
    html = table,
    path = table_path,
    dl_button = dl_button
  )
}

get_active_fire_data <- function() {
  fire_groups <- c("0 - 100 ha", "101 - 1000 ha", "> 1000ha")
  fire_states <- c("Other", "Under Control", "Being Held", "Out of Control")
  "https://cwfis.cfs.nrcan.gc.ca/downloads/activefires/activefires.csv" |>
    data.table::fread(data.table = FALSE, fill = TRUE, sep = ",") |>
    dplyr::mutate(
      startdate = lubridate::ymd_hms(startdate),
      size = hectares |>
        cut(
          breaks = c(0, 100, 1000, Inf),
          labels = fire_groups,
          right = TRUE,
          include.lowest = TRUE,
          ordered_result = TRUE
        ),
      state = dplyr::case_when(
        stage_of_control == "UC" ~ fire_states[2],
        stage_of_control == "BH" ~ fire_states[3],
        stage_of_control == "OC" ~ fire_states[4],
        TRUE ~ fire_states[1]
      ) |>
        factor(fire_states),
      labels = (paste0(
        "<b>",
        firename,
        "</b><br>",
        "\"",
        state,
        "\" fire<br>Size: ~",
        hectares,
        " ha<br>",
        "Started: ",
        format(startdate, "%Y-%m-%d")
      )),
      labels2 = sapply(labels, htmltools::HTML)
    ) |>
    subset(startdate <= max_date)
}
