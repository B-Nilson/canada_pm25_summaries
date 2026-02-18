source("R/get_report_start_end.R")
source("R/load_report_data.R")
source("R/handle_old_reports.R")
source("R/make_summaries.R")
source("R/aqhi_donut_plots.R")
source("R/interactive_map.R")
source("R/site_boxplots.R")
source("R/aqhi_grid_plots.R")

make_aqmap_link <- function(lat, lng, zoom = 12, lang = "EN") {
  paste0(
    "https://aqmap.ca/aqmap/",
    tolower(lang),
    "/#",
    zoom,
    "/",
    lat,
    "/",
    lng
  )
}

make_hourly_seq <- function(date_range) {
  date_range |>
    handyr::as_interval() |>
    seq(by = "1 hours")
}

make_plot_captions <- function(
  date_range,
  date_format = "%b %d, %Y %H:%M UTC",
  networks = c("FEM", "PA")
) {
  date_range <- sort(date_range) |> format(date_format)
  caption_range <- paste(date_range, collapse = " to ")

  "Obs. from Canadian %s monitors from %s" |>
    sprintf(networks, caption_range) |>
    setNames(networks) |>
    as.list()
}

pop_coverage_barchart <- function(pd, pop_type = "total", leg_rows = 1) {
  prov_order <- list(
    West = c(
      bc = "British Columbia",
      ab = "Alberta",
      sk = "Saskatchewan",
      mb = "Manitoba"
    ),
    Central = c(on = "Ontario", qc = "Québec"),
    East = c(
      ns = "Nova Scotia",
      nb = "New Brunswick",
      nl = "Newfoundland and Labrador",
      pe = "Prince Edward Island"
    ),
    North = c(yk = "Yukon", nt = "Northwest Territories", nu = "Nunavut")
  )

  colours <- c(
    "FEM Only" = "#F0BE19",
    "PA Only" = "#944BE7",
    "FEM and PA" = "#F0BE19",
    "No Monitor" = "#AF1B3F"
  )
  totals <- pd |>
    dplyr::group_by(region, prov_terr) |>
    dplyr::summarise(pop = sum(pop))
  ymax <- 1.05
  if (pop_type %in% c("urban", "rural")) {
    totals$pop <- sapply(totals$pop, \(p) {
      paste0(
        format(round(p / 10000), big.mark = ",", scientific = FALSE),
        "\nx10k"
      )
    })
    ymax <- 1.1
  } else if (pop_type == "total") {
    totals$pop <- sapply(totals$pop, \(p) {
      paste0(format(round(p / 10000), big.mark = ","), "\nx10k")
    })
    ymax <- 1.1
  }

  tmp <- lengths(prov_order) |> c(Canada = 1)
  pat_spacing <- rep(0.05, nlevels(pd$region)) *
    tmp[1] /
    tmp
  pd |>
    # Initiate plot
    ggplot2::ggplot(ggplot2::aes(
      x = prov_terr,
      y = p_pop,
      fill = forcats::fct_rev(network),
      pattern = network == "FEM and PA",
      pattern_spacing = region
    )) +
    # Draw bars, adding stripes to the FEM and PA category
    ggpattern::geom_bar_pattern(
      stat = "identity",
      colour = "black",
      pattern_alpha = 0.8,
      alpha = 0.8,
      pattern_fill = colours[names(colours) == "PA Only"],
      # pattern_angle = 45,
      pattern_density = 0.45,
      # pattern_spacing = 0.025,
      pattern_key_scale_factor = 0.6
    ) +
    # Add total counts to tops of bars
    ggplot2::geom_text(
      data = totals,
      ggplot2::aes(x = prov_terr, label = pop),
      vjust = 0,
      hjust = 0.5,
      family = "Inter",
      y = 1.02,
      inherit.aes = FALSE
    ) +
    # One plot panel per region
    ggplot2::facet_grid(
      ~region,
      scales = "free_x",
      space = "free",
      switch = "x"
    ) +
    # Use percent scales for y, 0-100 on left, 5-95 on right
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(0),
      sec.axis = ggplot2::dup_axis(breaks = seq(0.05, 1, 0.1)),
      labels = scales::percent_format(),
      breaks = seq(0, 1, 0.1),
      limits = c(0, ymax)
    ) +
    # Use specified colours
    ggplot2::scale_fill_manual(values = colours) +
    # Specify pattern drawn for FEM and PA category
    ggpattern::scale_pattern_manual(
      values = c(`TRUE` = "stripe", `FALSE` = "none"),
      guide = "none"
    ) +
    ggpattern::scale_pattern_spacing_manual(
      values = pat_spacing,
      guide = "none"
    ) +
    # Change appearance of plot
    ggpubr::theme_pubr(border = FALSE) +
    ggplot2::theme(
      panel.spacing = ggplot2::unit(0, 'npc'),
      panel.grid.major.y = ggplot2::element_line(
        colour = "grey",
        linewidth = 0.5,
        linetype = "solid"
      ),
      legend.spacing.y = ggplot2::unit(1.0, 'mm'),
      axis.title.y.right = ggplot2::element_blank()
    ) +
    # Add pattern to the legend
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        reverse = TRUE,
        nrow = leg_rows,
        byrow = TRUE,
        override.aes = list(
          pattern = c("none", "stripe", rep("none", length(colours) - 2)),
          linewidth = 2
        )
      )
    ) +
    # Add basic labels
    ggplot2::labs(
      y = ifelse(
        pop_type == "fnic",
        "FN & Inuit Communities",
        paste(stringr::str_to_title(pop_type), "Population (2016 Census)")
      ),
      x = "Province / Territory",
      fill = "Within 25 km of:",
      caption = plot_captions$`FEM and PA`
    )
}

get_season <- function(date = Sys.time(), months_in_seasons) {
  season <- names(months_in_seasons)[
    sapply(months_in_seasons, \(months) lubridate::month(date) %in% months)
  ]
  year <- lubridate::year(date) -
    ifelse(
      season == "Winter" & lubridate::month(date) < 6,
      1,
      0
    )
  paste0(year, "-", season)
}

max <- handyr::max
min <- handyr::min

shorten_number_list <- function(x) {
  if (length(x) == 0) {
    return("")
  }
  paste(
    tapply(x, cumsum(c(1, diff(x) != 1)), \(i) {
      ifelse(
        length(i) > 2,
        paste0(head(i, 1), '-', tail(i, 1)),
        paste(i, collapse = ', ')
      )
    }),
    collapse = ', '
  )
}

join_list_sentence <- function(l, oxford = FALSE) {
  if (length(l) == 0) {
    return("No regions")
  }
  if (length(l) == 1) {
    return(l)
  }
  out <- paste(l[-length(l)], collapse = ", ")
  out <- paste0(out, ifelse(oxford, ", and ", " and "), dplyr::last(l))
  return(out)
}

get_report_name <- function(
  max_date,
  type,
  months_in_seasons = list(
    "Summer" = 5:10,
    "Winter" = c(11:12, 1:4)
  )
) {
  date_fmt <- dplyr::case_when(
    type == "daily" ~ "%Y-%m-%d %H",
    type %in% c("monthly", "seasonal") ~ "%Y-%m"
  )
  report_name <- max_date |> format(date_fmt)

  if (type == "daily") {
    report_name <- report_name |>
      stringr::str_replace(" 11$", "-a") |>
      stringr::str_replace(" 23$", "-b")
  } else if (type == "seasonal") {
    report_name <- max_date |>
      get_season(months_in_seasons = months_in_seasons)
  }

  return(report_name)
}

get_previous_report_name <- function(
  current_report_date,
  type,
  months_in_seasons = list(
    "Summer" = 5:10,
    "Winter" = c(11:12, 1:4)
  )
) {
  if (type == "daily") {
    period <- lubridate::hours(12)
    drpdwn_date_fmt <- "%Y %b %d (%p)"
  } else if (type == "monthly") {
    period <- lubridate::days(32)
    drpdwn_date_fmt <- "%B %Y"
  } else if (type == "seasonal") {
    period <- lubridate::days(183)
  } else {
    stop("Other types not supported!")
  }
  (current_report_date - period) |>
    get_report_name(type = type, months_in_seasons = months_in_seasons)
}

# Data Wrangling ------------

safe_mean <- function(x, digits = 1) {
  mean(x, na.rm = TRUE) |>
    suppressWarnings() |>
    dplyr::replace_values(NaN ~ NA_real_) |>
    round(digits = digits)
}

format_count_summary <- function(
  dat,
  monitor = "FEM",
  range_text = "exceeding 100"
) {
  if (nrow(dat) > 1) {
    x <- paste0("<strong>", dat[[monitor]], "</strong>") |>
      paste(
        unlist(dat[, 'prov_terr']),
        sep = paste0(" ", monitor, " monitor(s) in ")
      )

    x[-length(x)] |>
      paste(collapse = ", ") |>
      paste0(
        ', and ',
        dplyr::last(x),
        " with a mean PM<sub>2.5</sub> concentration over the past 24 hours <strong> ",
        range_text,
        " &mu;g m<sup>-3</sup></strong>"
      )
  } else if (nrow(dat) == 1) {
    paste0("<strong>", dat[[monitor]], "</strong>") |>
      paste(
        unlist(dat[, 'prov_terr']),
        sep = paste0(" ", monitor, " monitor(s) in ")
      ) |>
      paste0(
        " with a mean PM<sub>2.5</sub> concentration over the past 24 hours <strong> ",
        range_text,
        " &mu;g m<sup>-3</sup></strong>"
      )
  } else {
    ""
  }
}

get_pm_events <- function(
  obs,
  groups = c("site_id"),
  clean_air_events = FALSE
) {
  out <- obs |>
    dplyr::mutate(
      aqhip = aqhi_p(pm25, use_risk = TRUE),
      date2 = date,
      date = lubridate::floor_date(date, "days")
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groups, "date")))) |>
    dplyr::mutate(
      mean_pm25 = mean(pm25, na.rm = TRUE),
      max_pm25 = max(pm25, na.rm = TRUE),
      min_pm25 = min(pm25, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groups, "date", "aqhip")))) |>
    dplyr::summarise(
      n = length(unique(date2)),
      n_pa = sum(!duplicated(site_id[monitor == "PA"])),
      n_fem = sum(!duplicated(site_id[monitor == "FEM"])),
      mean_pm25 = mean(mean_pm25, na.rm = TRUE),
      min_pm25 = min(min_pm25, na.rm = TRUE),
      max_pm25 = max(max_pm25, na.rm = TRUE)
    ) |>
    dplyr::mutate(n = ifelse(is.na(aqhip), 0, n), p = n / 24) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groups, "date"))))

  if (clean_air_events) {
    out <- out |>
      dplyr::filter(aqhip == "Low [1-3]" & n == 24)
  } else {
    out <- out |>
      dplyr::filter(aqhip != "Low [1-3]")
  }
  out |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(groups, "date")))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groups)))) |>
    dplyr::filter(!duplicated(date)) |>
    dplyr::mutate(
      not_seq = difftime(date, lag(date, 1), units = "days") != 1,
      not_seq = ifelse(is.na(not_seq), FALSE, not_seq),
      event_id = cumsum(not_seq) + 1
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groups, "event_id")))) |>
    dplyr::summarise(
      min_date = min(date),
      max_date = max(date),
      duration = difftime(max_date, min_date, units = "days") + 1,
      n_pa = max(n_pa, na.rm = TRUE),
      n_fem = max(n_fem, na.rm = TRUE),
      max_pm25 = max(max_pm25, na.rm = TRUE) |> round(1),
      min_pm25 = min(min_pm25, na.rm = TRUE) |> round(1),
      mean_pm25 = mean(mean_pm25, na.rm = TRUE) |> round(1),
      .groups = "drop"
    ) |>
    # dplyr::filter(max_pm25 >= 35) |>
    dplyr::mutate(
      event_magnitude = mean_pm25 * as.numeric(duration) |> round(1)
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
    dplyr::mutate(event_rank = rank(-event_magnitude))
}

# Plotting -------
plot_card <- function(
  plot_src,
  text,
  iframe = FALSE,
  title = NA,
  iframe_height = 615
) {
  img <- ifelse(
    iframe,
    paste0(
      "<iframe loading='lazy' style='height:",
      iframe_height,
      "px' src='$plot_src' class='card-img-top p-2' data-external='1' frameborder='0' scrolling='no' width='100%'></iframe>"
    ),
    "<img loading='lazy' src='$plot_src' class='card-img-top p-2'>"
  ) |>
    stringr::str_replace("\\$plot_src", plot_src)
  if (!is.na(title)) {
    img <- paste0(
      "<center style='padding-top:0.5rem;'><big>",
      title,
      "</big></center>",
      img
    )
  }
    "
::: card
  
$IMG
  
:::: card-body
  
::::: card-text

$text

:::::

::::

:::
" |>
      stringr::str_replace("\\$IMG", img) |>
      stringr::str_replace("\\$text", text)
}

make_sync_map <- function(
  zone_sum_month,
  cols = c("pm25", "pm25_max"),
  legend_titles
) {
  maps <- lapply(cols, \(stat) {
    pm_legend <- paste0(
      '<div style="margin-bottom:3px"><strong><span>',
      legend_titles[[stat]],
      "</span></strong></div>",
      '<i style="background:#ADADAD;opacity:1;"></i> No Data.<br>',
      paste0(
        '<i style="background:',
        leg_ugm3$colours,
        ';opacity:1;"></i> [',
        leg_ugm3$breaks,
        " – ",
        c(leg_ugm3$breaks[-1], "Inf"),
        ")"
      ) |>
        paste(collapse = "<br>")
    )
    fcst.pal <- leaflet::colorBin(
      palette = leg_ugm3$colours,
      bins = c(leg_ugm3$breaks, Inf),
      right = FALSE,
      na.color = "#bbbbbb"
    )
    leaflet::leaflet(
      options = leaflet::leafletOptions(doubleClickZoom = FALSE),
      height = 600
    ) |>
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) |>
      leaflet::addPolygons(
        data = zone_sum_month,
        fillColor = ~ fcst.pal(get(stat)),
        fillOpacity = ~ ifelse(is.na(get(stat)), 0.5, 0.5),
        color = "black",
        weight = 1,
        popupOptions = leaflet::popupOptions(minWidth = '330'),
        label = ~ lapply(label, HTML)
      ) |>
      leaflet::addControl(pm_legend, position = "topright")
  }) |>
    setNames(cols)

  map <- leafsync::sync(maps$pm25, maps$pm25_max)

  # Better formatting for synced maps
  map$dependencies <- list(
    htmltools::htmlDependency(
      head = "<style>body{height: 475px; display: flex; justify-content: space-evenly;}</style>",
      version = '0.0.1',
      name = 'lf-map-sync-style',
      src = ""
    )
  )

  return(map)
}


make_anim_map <- function(zone_sum, legend_title, height = 940, opacity = 0.5) {
  pal <- leaflet::colorBin(
    palette = leg_ugm3$colours,
    bins = c(leg_ugm3$breaks, Inf),
    right = FALSE,
    na.color = "#ADADAD" #"#bbbbbb"
  )
  pm_legend <- paste0(
    '<div style="margin-bottom:3px"><strong><span>',
    legend_title,
    "</span></strong></div>",
    '<i style="background:#ADADAD;opacity:1;"></i> No Data.<br>',
    paste0(
      '<i style="background:',
      leg_ugm3$colours,
      ';opacity:1;"></i> [',
      leg_ugm3$breaks,
      " – ",
      c(leg_ugm3$breaks[-1], "Inf"),
      ")"
    ) |>
      paste(collapse = "<br>")
  )

  leaflet::leaflet(
    options = leaflet::leafletOptions(doubleClickZoom = FALSE),
    height = height
  ) |>
    leaflet::addProviderTiles(providers$OpenStreetMap) |>
    leaflet.extras2::addTimeslider(
      data = dplyr::ungroup(zone_sum) |> dplyr::arrange(fcst_zone),
      fillColor = ~ pal(pm25),
      color = "black",
      weight = 1,
      fillOpacity = opacity,
      label = ~label,
      options = leaflet::timesliderOptions(
        follow = TRUE,
        position = "topright",
        timeAttribute = "date",
        sameDate = TRUE,
        alwaysShowDate = TRUE,
        range = FALSE
      ),
      ordertime = FALSE
    ) |>
    leaflet::addControl(pm_legend, position = "bottomleft")
}

make_community_boxplots <- function(pd, m = "FEM and PA") {
  box <- boxplot(n_hours_above_100 ~ prov_terr, pd, plot = FALSE)
  pd <- pd |>
    dplyr::mutate(
      is_outlier = n_hours_above_100 > box$stats[4, as.numeric(prov_terr)]
    )
  outliers <- subset(pd, is_outlier)

  pd |>
    plotly::plot_ly(y = ~n_hours_above_100, x = ~prov_terr) |>
    plotly::add_boxplot(
      name = "",
      boxpoints = FALSE,
      color = I('black'),
      alpha = 0.2
    ) |>
    plotly::layout(
      title = paste(
        "Number of Community-Level, Very High Exceedances by Province/Territory for",
        format(max_date, "%B %Y")
      ),
      showlegend = FALSE,
      xaxis = list(
        title = "Province | Territory",
        zeroline = TRUE,
        showgrid = FALSE
      ),
      yaxis = list(
        title = paste0(
          "# Hours Community Exceeds PM2.5 of 100 ug/m3 (",
          m,
          ")"
        ),
        zeroline = TRUE,
        showgrid = FALSE
      )
    ) |>
    plotly::add_markers(
      data = outliers,
      text = ~ paste0(
        "<b>",
        nearest_community,
        "</b>",
        # "<br>",community_type,
        "<br># of observation sites: ",
        n_sites,
        "<br>1-month mean: ",
        round(pm25_mean, 1),
        " ug/m3",
        "<br>1-month max: ",
        round(pm25_max, 1),
        " ug/m3",
        "<br># hours >60 ug/m3: ",
        n_hours_above_60,
        " / ",
        length(hours_to_summarise),
        "<br># hours >100 ug/m3: ",
        n_hours_above_100,
        " / ",
        length(hours_to_summarise),
        "<br>Forecast Zone: ",
        fcst_zone
      ),
      hoverinfo = 'text',
      marker = list(
        color = "rgb(107,174,214)",
        line = list(color = "black", width = 1)
      )
    )
}


# Tables ------

reactable <- function(pagination = TRUE, defaultPageSize = 10, ...) {
  reactable::reactable(
    ...,
    filterable = TRUE,
    resizable = TRUE,
    bordered = TRUE,
    striped = TRUE,
    compact = TRUE,
    pagination = pagination,
    # showSortable = TRUE,
    defaultPageSize = defaultPageSize,
    highlight = TRUE,
    defaultColDef = reactable::colDef(align = "center"),
    theme = reactable::reactableTheme(
      # Vertically center cells
      cellStyle = list(
        display = "flex",
        flexDirection = "column",
        justifyContent = "center"
      )
    )
  )
}

dl_button_html <- function(outdir, file) {
  paste0(
    '<button class="btn btn-default" sep="," has_icon="TRUE" type="submit" onclick="window.open(\'',
    outdir,
    file,
    '\')"><i class="fa fa-save"></i> Download data as csv</button>'
  ) |>
    htmltools::HTML()
}

make_download_button <- function(data_for_download, data_dir, file_path) {
  sanitized_header <- data_for_download |>
    names() |>
    stringr::str_replace_all("\n", " ")
  data_for_download |>
    setNames(sanitized_header) |>
    data.table::fwrite(file_path, dateTimeAs = "write.csv")
  dl_button_html(outdir = data_dir |> paste0("/"), file = basename(file_path))
}
