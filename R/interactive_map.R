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

make_map_table <- function(
  table_data,
  monitor_group,
  report_dir,
  data_dir,
  plot_timestamp
) {
  dir.create(file.path(report_dir, data_dir), showWarnings = FALSE)
  pd <- table_data
  if (monitor_group != "FEM and PA") {
    pd <- table_data |>
      subset(Monitor == monitor_group)
  }

  column_definitions <- list(
    'Site Name' = reactable::colDef(cell = \(Site, ...) {
      s_name = table_data[table_data$`Site Name` == Site, "aqmap_link"] |>
        unlist() |>
        unname()
      htmltools::tags$a(href = s_name, target = "_blank", Site)
    }),
    aqmap_link = reactable::colDef(show = FALSE),
    lng = reactable::colDef(show = FALSE),
    lat = reactable::colDef(show = FALSE)
  )

  table <- pd |>
    reactable(
      data = _,
      defaultSorted = "24hr PM2.5 Mean\n(ug/m3)",
      defaultSortOrder = "desc",
      columns = column_definitions
    )

  # Save data to csv for download
  file_path <- "%s/pm2.5_monitor_sites_%s_%s.csv" |>
    sprintf(
      file.path(report_dir, data_dir),
      monitor_group |>
        stringr::str_to_lower() |>
        stringr::str_replace_all(" ", "_"),
      plot_timestamp
    )
  dl_button <- pd |>
    make_download_button(data_dir = data_dir, file_path = file_path)

  list(
    html = table,
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
