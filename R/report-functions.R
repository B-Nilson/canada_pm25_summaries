source("R/get_report_start_end.R")
source("R/load_report_data.R")
source("R/handle_old_reports.R")
source("R/make_summaries.R")
source("R/aqhi_donut_plots.R")

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

safe_mean <- function(x, digits = 1) {
  mean(x, na.rm = TRUE) |>
    suppressWarnings() |>
    dplyr::replace_values(NaN ~ NA_real_) |>
    round(digits = digits)
}

make_map_table_data <- function(obs_summary, obs_current) {
  obs_summary |>
    dplyr::full_join(
      obs_current |>
        dplyr::select(site_id, name, pm25_current = pm25, monitor),
      by = c('site_id', 'name', 'monitor')
    ) |>
    dplyr::mutate(
      aqhi_p_cat_mean = aqhi_p(pm25_mean, use_risk = TRUE),
      aqhi_p_cat_max = aqhi_p(pm25_max, use_risk = TRUE),
      aqhi_p_cat_current = aqhi_p(pm25_current, use_risk = TRUE),
    ) |>
    dplyr::mutate(pm25_mean = round(pm25_mean, 1)) |>
    dplyr::arrange(desc(pm25_mean)) |>
    dplyr::mutate(
      aqmap_link = paste0(
        "https://aqmap.ca/aqmap/#12/",
        lat,
        "/",
        lng
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      "Site Name" = name,
      "Monitor" = monitor,
      "Prov./Terr." = prov_terr,
      "Forecast Zone" = fcst_zone,
      "Closest Community" = nearest_community,
      "Dist. From Community\n(km)" = nc_dist_km_mean,
      "# Hours (/24) >/= 60 ug/m3" = n_hours_above_60,
      "Current PM2.5\n(ug/m3)" = pm25_current,
      "24hr PM2.5 Mean\n(ug/m3)" = pm25_mean,
      "24hr PM2.5 Max\n(ug/m3)" = pm25_max,
      lng,
      lat,
      aqmap_link
    ) |>
    dplyr::arrange(`Prov./Terr.`) |>
    dplyr::filter(!duplicated(`Site Name`))
}

make_map_data <- function(obs_summary) {
  obs_summary |>
    # subset(n_hours_above_60 >=3) |>
    dplyr::mutate(
      icon_link = paste0(
        "../icons/icon_",
        ifelse(monitor == "FEM", 23, 21),
        "_",
        ifelse(pm25_mean > 999, "+", round(pm25_mean)),
        ".png"
      ),
      labels = htmltools::HTML(paste0(
        "<strong><big>Site: ",
        name,
        " (",
        monitor,
        ")</big></strong><br><b>Nearby Community:</b>",
        nearest_community,
        "(~",
        round(nc_dist_km_mean, 1),
        " km)",
        "<br><b>24hr mean PM<sub>2.5</sub>:</b> ",
        round(pm25_mean, 1),
        " Î¼g m<sup>-3</sup>",
        "<br><b>24hr max PM<sub>2.5</sub>:</b> ",
        pm25_max,
        " Î¼g m<sup>-3</sup>",
        "<br><b># Hours with PM<sub>2.5</sub> >= 60 ug/m3:</b> ",
        n_hours_above_60
      ))
    )
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

  if (is.null(fcst_zones)) {
    out <- obs |>
      dplyr::bind_rows(obs |> dplyr::mutate(monitor == "FEM and PA")) |>
      dplyr::arrange(date) |>
      dplyr::group_by(
        y = prov_terr,
        x = FUN2(date) |> factor(time_order),
        monitor
      ) |>
      dplyr::summarise(
        fill = FUN(pm25, na.rm = TRUE) |> aqhi_p(),
        .groups = "drop"
      ) |>
      dplyr::full_join(
        expand.grid(
          y = names(prov_pretty),
          monitor = names(monitor_groups),
          x = time_order |> factor(time_order)
        ),
        by = c('y', 'x', 'monitor')
      )
  } else {
    f <- \(y) {
      c("Not inside a zone", y[y != "Not inside a zone"])
    }
    out <- obs |>
      dplyr::bind_rows(obs |> dplyr::mutate(monitor == "FEM and PA")) |>
      dplyr::arrange(date) |>
      dplyr::group_by(
        y = fcst_zone,
        x = FUN2(date) |> factor(time_order),
        z = prov_terr,
        monitor
      ) |>
      dplyr::summarise(
        fill = FUN(pm25, na.rm = TRUE) |> aqhi_p(),
        .groups = "drop"
      ) |>
      dplyr::full_join(
        expand.grid(
          y = fcst_zones$fcst_zone |> unique(),
          monitor = names(monitor_groups),
          x = time_order |> factor(time_order)
        ) |>
          dplyr::left_join(
            fcst_zones |>
              dplyr::select(y = fcst_zone, z = prov_terr) |>
              dplyr::mutate(z = ifelse(z == "YT", "YK", z)),
            by = "y"
          ),
        by = c('y', 'x', 'z', 'monitor')
      ) |>
      dplyr::group_by(z, y) |>
      dplyr::mutate(all_missing = all(is.na(fill))) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        y = factor(
          y,
          c(sort(y[!all_missing]), sort(y[all_missing])) |>
            unique() |>
            f()
        )
      )
  }
  return(out)
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
  cat(
    "
<div class='card'>
$IMG
<div class='card-body'>

<div class='card-text'>

$text

</div>

</div>

</div>
" |>
      stringr::str_replace("\\$IMG", img) |>
      stringr::str_replace("\\$text", text) |>
      HTML()
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
  gg <- plot_data |>
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, fill = fill)) +
    ggplot2::geom_tile(colour = "black") +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(0)) +
    ggplot2::scale_fill_manual(
      values = leg_ugm3$colours |> setNames(c(1:10, "+")),
      breaks = c(1:10, "+"),
      drop = FALSE,
      na.value = NA,
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

make_map <- function(
  pd,
  bounds,
  zone_summaries,
  include_active_fires = TRUE,
  mean_pm25_col = 'mean_pm25_24hr_mean'
) {
  layers.all <- c("Observations", "Forecast Zone Mean", "Active Fires")
  if (!include_active_fires) {
    layers.all <- layers.all[-length(layers.all)]
  }

  popup <- function(
    name,
    pm25_24_fem,
    pm25_24_pa,
    pm25_24_all,
    n_fem,
    n_pa,
    FR = FALSE
  ) {
    if (FR) {
      text <- c(
        'Zone de prévision',
        '# de moniteurs',
        'Moyenne sur 24 h PM<sub>2,5</sub>'
      )
      fem <- 'MEF'
      all <- "TOUT"
    } else {
      text <- c('Forecast Zone', '# of Monitors', 'Mean 24hr PM<sub>2.5</sub>')
      fem <- 'FEM'
      all <- 'ALL'
    }
    paste0(
      "<big><strong>",
      text[1],
      ": ",
      name,
      "</strong></big>
    <table style='margin: auto;'>
      <thead>
        <tr>
          <th style='text-align:center'></th>
          <th style='text-align:center'>",
      fem,
      "</th>
          <th style='text-align:center'>PA</th>
          <th style='text-align:center'>",
      all,
      "</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td style='font-weight: bold;text-align:center'>",
      text[2],
      "</td>
          <td style='text-align:center'>",
      n_fem,
      "</td>
          <td style='text-align:center'>",
      n_pa,
      "</td>
          <td style='text-align:center'>",
      n_fem + n_pa,
      "</td>
        </tr>
        <tr>
          <td style='font-weight: bold;text-align:center'>",
      text[3],
      "</td>
          <td style='text-align:center'>",
      pm25_24_fem,
      " μg m<sup>-3</sup></td>
          <td style='text-align:center'>",
      pm25_24_pa,
      " μg m<sup>-3</sup></td>
          <td style='text-align:center'>",
      pm25_24_all,
      " μg m<sup>-3</sup></td>
        </tr>
      </tbody>
      </table>"
    ) |>
      stringr::str_replace_all(">NA μg m<sup>-3</sup><", ">-<")
  }

  fcst.pal <- leaflet::colorBin(
    palette = leg_ugm3$colours,
    bins = c(leg_ugm3$breaks, Inf),
    right = FALSE,
    na.color = "#bbbbbb"
  )

  pm_legend <- '<div style="margin-bottom:3px"><strong><span>24-hour Mean<br>PM<sub>2.5</sub> (μg m<sup>-3</sup>)</span></strong></div><i style="background:#21C6F5;opacity:1"></i> [0 – 10)<br><i style="background:#189ACA;opacity:1"></i> [10 – 20)<br><i style="background:#0D6797;opacity:1"></i> [20 – 30)<br><i style="background:#FFFD37;opacity:1"></i> [30 – 40)<br><i style="background:#FFCC2E;opacity:1"></i> [40 – 50)<br><i style="background:#FE9A3F;opacity:1"></i> [50 – 60)<br><i style="background:#FD6769;opacity:1"></i> [60 – 70)<br><i style="background:#FF3B3B;opacity:1"></i> [70 – 80)<br><i style="background:#FF0101;opacity:1"></i> [80 – 90)<br><i style="background:#CB0713;opacity:1"></i> [90 – 100)<br><i style="background:#650205;opacity:1"></i> [100 – Inf)<br>'

  map <- pd |>
    leaflet::leaflet(height = 600, width = "100%") |>
    # Set zoom to show all markers
    leaflet::fitBounds(bounds[1], bounds[3], bounds[2], bounds[4]) |>
    leaflet::addProviderTiles(providers$OpenStreetMap) |>
    leaflet::addMarkers(
      lng = ~lng,
      lat = ~lat,
      options = ~ leaflet::markerOptions(zIndexOffset = pm25_mean * 100),
      label = ~labels,
      group = layers.all[1],
      icon = ~ list(iconUrl = icon_link, iconHeight = 26, iconWidth = 26)
    ) |>
    leaflet::addPolygons(
      data = zone_summaries,
      fillColor = ~ fcst.pal(get(mean_pm25_col)),
      color = "black",
      weight = 1,
      fillOpacity = ~ ifelse(is.na(get(mean_pm25_col)), 0, 0.5),
      popupOptions = leaflet::popupOptions(minWidth = '330'),
      group = layers.all[2],
      popup = ~ popup(
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

# ONLY TESTED FOR MONTHLY PLOTS
make_site_boxplots <- function(pd, m = "FEM and PA") {
  fills <- 1:length(leg_ugm3[[1]]) |>
    lapply(\(i) {
      max_val <- ceiling(max(pd$pm25_mean, na.rm = TRUE) / 10) * 10

      if (leg_ugm3$breaks[i] > max_val) {
        return(NULL)
      }
      list(
        type = "rect",
        fillcolor = leg_ugm3$colours[i],
        line = list(width = 0),
        opacity = 1,
        x0 = -0.5,
        x1 = 12.5,
        layer = 'below',
        y0 = leg_ugm3$breaks[i],
        y1 = c(leg_ugm3$breaks[-1], max_val)[i]
      )
    })

  box <- boxplot(pm25_mean ~ prov_terr, pd, plot = FALSE)
  pd <- pd |>
    dplyr::mutate(is_outlier = pm25_mean > box$stats[4, as.numeric(prov_terr)])
  outliers <- subset(pd, is_outlier)

  pd |>
    plotly::plot_ly(y = ~ round(pm25_mean, 1), x = ~prov_terr) |>
    plotly::add_boxplot(
      name = "",
      boxpoints = FALSE,
      color = I('black'),
      alpha = 0.2
    ) |>
    plotly::layout(
      title = paste(
        "Site Mean Distributions by Province/Territory for",
        format(max_date, "%B %Y")
      ),
      shapes = fills,
      showlegend = FALSE,
      xaxis = list(
        title = "Province | Territory",
        zeroline = TRUE,
        showgrid = FALSE
      ),
      yaxis = list(
        title = paste0("1-month Site Mean PM2.5 (ug/m3; ", m, ")"),
        zeroline = TRUE,
        showgrid = FALSE
      )
    ) |>
    plotly::add_markers(
      data = outliers,
      text = ~ paste0(
        "<b>",
        name,
        "</b>",
        "<br>",
        monitor,
        " Monitor",
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
        "<br>Closest Community: ",
        nearest_community,
        " (~",
        nc_dist_km_mean,
        " km)",
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
    defaultColDef = colDef(align = "center"),
    theme = reactableTheme(
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
