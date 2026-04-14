make_and_save_overall_summary_cards <- function(
  overall_summary,
  zone_summary,
  date_range,
  all_monitors_group,
  type,
  data_dir,
  figure_dir,
  lib_dir,
  plot_timestamp,
  map_caption,
  table_caption,
  include_active_fires = FALSE
) {
  map <- overall_summary |>
    make_and_save_overall_map(
      zone_summary = zone_summary,
      date_range = date_range,
      monitor_group = all_monitors_group,
      report_dir = type,
      plot_dir = figure_dir,
      lib_dir = lib_dir,
      include_active_fires = include_active_fires,
      map_timestamps = date_range
    ) |>
    stringr::str_replace(stringr::fixed(type), "./") |>
    plot_card(
      text = map_caption,
      iframe = TRUE,
      iframe_height = 617,
      plot_timestamp = plot_timestamp
    ) |>
    knitr::asis_output()

  table <- overall_summary |>
    make_overall_summary_table(
      type = type,
      monitor_group = all_monitors_group,
      data_dir = data_dir,
      figure_dir = figure_dir,
      plot_timestamp = plot_timestamp,
      table_caption = table_caption
    )

  list(
    map = map,
    table = table$card
  )
}

make_and_save_overall_map <- function(
  overall_summary,
  zone_summary,
  date_range,
  monitor_group,
  include_active_fires = FALSE,
  report_dir,
  plot_dir,
  lib_dir,
  map_timestamps
) {
  map_data <- overall_summary |>
    make_map_data()

  # Make paths to map files
  plot_name <- "site_mean_map_%s_%s.html" |>
    sprintf(
      monitor_group |>
        stringr::str_to_lower() |>
        stringr::str_replace_all(" ", "_"),
      plot_timestamp
    )
  plot_path_tmp <- report_dir |>
    file.path(plot_dir, plot_name) # for initial write - otherwise symlinks expanded and libs in /plots/ not referenced correctly by html files in /plots/{date}/
  plot_path <- report_dir |>
    file.path(plot_dir, plot_timestamp, plot_name)

  # Make and save maps
  pd <- map_data |>
    dplyr::filter(
      monitor %in% stringr::str_split_1(monitor_group, ", | and ")
    )
  pd |>
    make_overall_map(
      zone_summary = zone_summary,
      include_active_fires = include_active_fires,
      map_timestamps = map_timestamps
    ) |>
    aqmapr::save_map(
      save_to = plot_path_tmp,
      library_dir = lib_dir,
      self_contained = FALSE
    )

  # Move into date folder now that libs setup correctly
  file.rename(plot_path_tmp, plot_path)

  return(plot_path)
}

make_overall_map <- function(
  pd,
  zone_summary,
  include_active_fires = TRUE,
  mean_pm25_col = 'mean_pm25_24hr_mean',
  map_timestamps
) {
  layers <- list(
    "FEM observations" = pd |> dplyr::filter(monitor == "FEM"),
    "PA observations" = pd |> dplyr::filter(monitor == "PA"),
    "Forecast Zone Mean (FEM + PA)" = zone_summary |>
      dplyr::filter(!is.na(fcst_zone_fr)),
    "Active Fires" = NULL
  )
  if (!include_active_fires) {
    layers <- layers[names(layers) != "Active Fires"]
  }

  aqhi_colours <- aqhi::get_aqhi_colours(1:11)
  aqhi_mins <- 0:10 * 10
  aqhi_maxs <- c(1:10 * 10, Inf)
  aqhi_pal <- leaflet::colorBin(
    palette = aqhi_colours,
    bins = c(0:10 * 10, Inf),
    right = FALSE,
    na.color = "#bbbbbb"
  )

  pm_legend <- paste0(
    '<div style="margin-bottom:3px"><strong><span>',
    "24-hour Mean<br>PM<sub>2.5</sub> (μg m<sup>-3</sup>)",
    "</span></strong></div>",
    paste(
      sep = "<br>",
      '<i style="background: %s; opacity: 1"></i> [%s - %s)' |>
        sprintf(aqhi_colours, aqhi_mins, aqhi_maxs) |>
        paste(collapse = "<br>")
    )
  )

  map <- leaflet::leaflet(height = 600, width = "100%") |>
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) |>
    aqmapr::add_map_timestamps(
      timestamps = map_timestamps,
      prefixes = c("From: ", "Up to: ")
    ) |>
    leaflet::addMarkers(
      data = layers[[1]],
      lng = ~lng,
      lat = ~lat,
      options = ~ leaflet::markerOptions(zIndexOffset = pm25_mean * 100),
      label = ~labels,
      group = names(layers)[1],
      icon = ~ list(iconUrl = icon_link, iconHeight = 26, iconWidth = 26)
    ) |>
    leaflet::addMarkers(
      data = layers[[2]],
      lng = ~lng,
      lat = ~lat,
      options = ~ leaflet::markerOptions(zIndexOffset = pm25_mean * 100),
      label = ~labels,
      group = names(layers)[2],
      icon = ~ list(iconUrl = icon_link, iconHeight = 26, iconWidth = 26)
    ) |>
    leaflet::addPolygons(
      data = zone_summary |>
        dplyr::filter_out(fcst_zone == "Not inside a defined zone"),
      fillColor = ~ aqhi_pal(get(mean_pm25_col)),
      color = "black",
      weight = 1,
      fillOpacity = ~ ifelse(is.na(get(mean_pm25_col)), 0, 0.5),
      popupOptions = leaflet::popupOptions(minWidth = '330'),
      group = names(layers)[3],
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
      overlayGroups = names(layers),
      position = 'topright'
    ) |>
    leaflet::addControl(html = pm_legend, position = 'bottomleft')

  if (include_active_fires) {
    active_fires <- get_active_fire_data(max_date = date_range[2])
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
        title = names(layers)[4]
      ) |>
      leaflet::addCircleMarkers(
        data = active_fires,
        group = names(layers)[4],
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
        popup = ~labels
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
      labels = name |>
        make_map_hover(
          monitors = monitor,
          nc_dists_km = nc_dist_km,
          nearest_communities = nearest_community,
          pm25_means = pm25_mean,
          pm25_maxs = pm25_max,
          n_hours_above_60s = n_hours_above_60
        ) |>
        lapply(htmltools::HTML)
    )
}

make_map_hover <- function(
  names,
  monitors,
  nc_dists_km,
  nearest_communities,
  pm25_means,
  pm25_maxs,
  n_hours_above_60s
) {
  paste(
    "<big><strong>%s</strong></big>",
    "%s ~%s km from %s",
    "<b>24hr mean|max:</b> %s|%s &mu;g m<sup>-3</sup>",
    "<b>Hours &ge; 60 &mu;g m<sup>-3</sup>:</b> %s",
    sep = "<br>"
  ) |>
    sprintf(
      names,
      monitors,
      nc_dists_km,
      nearest_communities,
      pm25_means,
      pm25_maxs,
      n_hours_above_60s
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
      'Count',
      'Mean 24hr PM<sub>2.5</sub> (&mu;g m<sup>-3</sup>)'
    ),
    FR = c(
      'Compte',
      'Moyenne sur 24 h PM<sub>2,5</sub> (&mu;g m<sup>-3</sup>)'
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

  "<big><strong>%s</strong></big>
    <table class='zone-popup-table'>
      <thead><tr><th></th><th>%s</th><th>PA</th><th>%s</th></tr></thead>
      <tbody>
        <tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>
        <tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>
      </tbody>
    </table>" |>
    sprintf(
      # header
      name,
      fem_label,
      all_label,
      # row 1 (counts)
      text[1],
      n_fem,
      n_pa,
      n_fem + n_pa,
      # row 2 (means)
      text[2],
      pm25_24_fem,
      pm25_24_pa,
      pm25_24_all
    ) |>
    stringr::str_replace_all(">NA<", ">-<") |>
    stringr::str_replace_all(">NaN<", ">-<")
}

make_overall_summary_table <- function(
  table_data,
  monitor_group,
  type,
  data_dir,
  figure_dir,
  plot_timestamp,
  table_caption
) {
  key_names <- c(
    name = "name",
    mon = "monitor",
    zone = "fcst_zone",
    comm = "nearest_community",
    d_comm = "nc_dist_km",
    h_30 = "n_hours_above_30",
    h_60 = "n_hours_above_60",
    h_100 = "n_hours_above_100",
    pm_last = "pm25_current",
    pm_mean = "pm25_mean",
    pm_max = "pm25_max"
  )

  display_names <- list(
    name = "Name",
    mon = "Type",
    zone = "Region",
    comm = "Name",
    d_comm = "Distance",
    h_30 = gt::md("30 &mu;g/m^3^"),
    h_60 = gt::md("60 &mu;g/m^3^"),
    h_100 = gt::md("100 &mu;g/m^3^"),
    pm_last = "Last",
    pm_mean = "Mean",
    pm_max = "Max"
  )
  table_data <- table_data |>
    dplyr::mutate(
      prov_terr = prov_terr |>
        factor(
          levels = levels(prov_terr),
          labels = canadata::provinces_and_territories$abbreviation
        ),
      fcst_zone = prov_terr |> paste0(": ", fcst_zone),
      name = "<a title=\"%s\" href=\"https://aqmap.ca/aqmap/#12/%s/%s\">%s</a>" |>
        sprintf(
          name |> gsub(pattern = '"', replacement = "&quot;"),
          lat |> round(digits = 4),
          lng |> round(digits = 4),
          name
        )
    ) |>
    dplyr::arrange(dplyr::desc(pm25_mean), pm25_current) |>
    dplyr::select(dplyr::all_of(key_names)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c("zone", "comm")), \(x) {
      "<span title=\"%s\">%s</span>" |>
        sprintf(x |> gsub(pattern = '"', replacement = "&quot;"), x)
    }))

  css <- ".rt-text-content {
  overflow-x: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}" |>
    htmltools::tags$style(type = "text/css")

  table <- table_data |>
    gt::gt() |>
    gt::opt_interactive(use_filters = TRUE) |>
    gt::cols_width(
      mon ~ gt::px(62),
      zone ~ gt::px(130),
      comm ~ gt::px(120),
      d_comm ~ gt::px(90),
      dplyr::starts_with("h_") &
        !dplyr::starts_with("h_100") ~ gt::px(97),
      h_100 ~ gt::px(105),
      dplyr::starts_with("pm_") ~ gt::px(78)
    ) |>
    gt::tab_spanner(
      label = "Monitoring Site",
      columns = c("name", "mon", "zone")
    ) |>
    gt::tab_spanner(
      label = gt::html("PM<sub>2.5</sub> Concentration (&mu;g m<sup>-3</sup>)"),
      columns = dplyr::starts_with("pm_")
    ) |>
    gt::tab_spanner(
      label = "Nearest Community",
      columns = c("comm", "d_comm")
    ) |>
    gt::tab_spanner(
      label = gt::html("Hours Above PM<sub>2.5</sub> Threshold"),
      columns = dplyr::starts_with("h_"),
      id = "hours_above_spanner"
    ) |>
    gt::cols_label(.list = display_names) |>
    gt::cols_align(dplyr::starts_with("h_"), align = "center") |>
    gt::cols_align("comm", align = "right") |>
    gt::cols_align("d_comm", align = "left") |>
    gt::fmt_number(dplyr::starts_with("pm_"), decimals = 1) |>
    gt::fmt_number("d_comm", decimals = 1, pattern = "{x} km") |>
    gt::data_color(
      alpha = 0.6,
      columns = d_comm,
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
      columns = dplyr::starts_with("pm_"),
      fn = \(x) x |> aqhi::get_aqhi_colours(types = "pm25_1hr")
    ) |>
    gt::sub_missing(
      dplyr::starts_with("h_") | dplyr::starts_with("pm_")
    ) |>
    htmltools::as.tags() |>
    htmltools::tagList(css)

  # Save table to .html and data to .csv, link within a plot_card
  m_group_cleaned <- monitor_group |>
    stringr::str_to_lower() |>
    stringr::str_replace_all(" ", "_")
  data_path <- "%s/%s/%s/pm2.5_monitor_sites_%s_%s.csv" |>
    sprintf(type, data_dir, plot_timestamp, m_group_cleaned, plot_timestamp)
  table_name <- "overall_table_%s_%s.html" |>
    sprintf(m_group_cleaned, plot_timestamp)
  table_path_tmp <- type |>
    file.path(figure_dir, table_name)
  table_path <- type |>
    file.path(figure_dir, plot_timestamp, table_name)

  table |>
    make_table_card(
      table_data = table_data,
      table_caption = table_caption,
      table_path = table_path,
      table_path_tmp = table_path_tmp,
      data_path = data_path,
      data_rel_dir = data_dir,
      plot_timestamp = plot_timestamp,
      type = type
    )
}

get_active_fire_data <- function(max_date) {
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
