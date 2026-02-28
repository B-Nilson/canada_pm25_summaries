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
