make_quarto_captions <- function(date_range, monitor_groups) {
  fig_caption_date_range <- date_range |>
    lubridate::with_tz("UTC") |>
    format("%b %d, %Y %H:%M UTC") |>
    paste(collapse = " to ")

  coverage_types <- c(
    total = "total population",
    fnic = "First Nation & Inuit communities",
    urban = "urban population",
    rural = "rural population"
  )

  coverage_sources <- c(
    total = "[the gridded 2016 census](https://open.canada.ca/data/en/dataset/c6c48391-fd2f-4d8a-93c8-eb74f58a859b)",
    fnic = "[First Nation](https://open.canada.ca/data/en/dataset/b6567c5c-8339-4055-99fa-63f92114d9e4) and [Inuit](https://open.canada.ca/data/en/dataset/2bcf34b5-4e9a-431b-9e43-1eace6c873bd) communities",
    urban = "[the gridded 2016 census](https://open.canada.ca/data/en/dataset/c6c48391-fd2f-4d8a-93c8-eb74f58a859b)",
    rural = "[the gridded 2016 census](https://open.canada.ca/data/en/dataset/c6c48391-fd2f-4d8a-93c8-eb74f58a859b)"
  )

  donut_texts <- paste(
    "Monitor counts and frequencies of median AQHI risk levels for %s PM2.5 concentrations for monitoring sites within each p/t for %s.",
    "The number within each circle indicates the total monitors with data in that area during the report period.",
    "Percentages indicate the proportion of those monitors with a median AQHI risk at that level for this report."
  ) |>
    sprintf(names(monitor_groups), fig_caption_date_range) |>
    setNames(monitor_groups)

  table_helper <- "The table is sortable, filterable, and has multiple pages."

  quarto_captions <- list(
    median_donuts = donut_texts,
    max_donuts = donut_texts |> stringr::str_replace_all(" median ", " max "),
    coverage = paste(
      "Estimated percentage of Canadian %s within 25 km of a FEM or PA PM2.5 monitor for %s",
      "The patterned section shows overlapping coverage of the two networks (i.e. %s covered by both networks).",
      "Data for %s from %s"
    ) |>
      sprintf(
        coverage_types,
        fig_caption_date_range,
        coverage_types,
        coverage_types,
        coverage_types,
        coverage_sources
      ) |>
      setNames(names(coverage_types)),
    map = paste(
      "Interactive map of 24-hour mean PM2.5 concentrations from FEM and PA monitoring sites (shown as diamonds and cirles, repsectively) within Canada for %s.",
      "Canadian forecast zones are shown coloured by the mean of the 24-hour mean concentrations from all monitors within the zone.",
      "Hover over a monitor or click on a forecast zone for additional details.",
      "Layers can be controlled in the top right menu."
    ) |>
      sprintf(fig_caption_date_range),
    daily_zones = paste(
      "Interactive map summarizing the daily mean concentrations from both FEM and PA monitors by forecast zone for %s.",
      "You can use the control menu in the top right to step through each day.",
      "If you hover over a forecast zone a summary popup will appear for that zone for the displayed day."
    ) |>
      sprintf(fig_caption_date_range),
    monthly_zones = paste(
      "Interactive map summarizing the monthly mean (left) and maximum (right) concentrations from both FEM and PA monitors by forecast zone for %s.",
      "If you hover over a forecast zone a summary popup will appear for that zone."
    ) |>
      sprintf(fig_caption_date_range),
    overall_table = paste(
      "Canadian FEM and PA monitor PM2.5 observation summary for %s.",
      table_helper,
      "Click the link on a monitor name to view that monitor's location on AQmap."
    ) |>
      sprintf(fig_caption_date_range),
    boxplots = paste(
      "Distributions of site-mean PM2.5 concentrations for %s monitors in each p/t from %s.",
      "Blue points show any sites with a mean concentration higher than that region's 75th percentile, with additional details displayed on hover.",
      "Hover over the boxplot (away from the points) to see the min/median/max and 25th/75th percentiles for that p/t.",
      "See the menu in the top right of the plot for more."
    ) |>
      sprintf(names(monitor_groups), fig_caption_date_range) |>
      setNames(monitor_groups),
    prov_grids = paste(
      "Median and maximum hourly PM2.5 concentrations for all %s monitoring sites in each p/t from %s.",
      "The median is useful for identifying large scale impacts across the p/t,",
      "while the maximum can be used to identify impacts in at least one area."
    ) |>
      sprintf(names(monitor_groups), fig_caption_date_range) |>
      setNames(monitor_groups),
    zone_grids = paste(
      "Hourly median PM2.5 concentrations for all FEM and PA monitors within each %s forecast zone for %s.",
      "A white cell indicates there was no data for that hour from any monitor within that zone.",
      "Zones are grouped into those with and those without monitors and are sorted alphabetically."
    ) |>
      sprintf(provinces_n_territories, fig_caption_date_range) |>
      setNames(provinces_n_territories),
    community_table = paste(
      "Canadian communities with at least one nearby FEM or PA PM2.5 monitor with data from %s.",
      table_helper,
      "Click on a name to view that community on AQmap."
    ) |>
      sprintf(fig_caption_date_range),
    community_boxplots = paste0(
      "Each boxplot summarises the distribution of hours with PM2.5 concentrations above 100 {{< var units.pm >}} for at least one %s monitoring site in each community for each p/t for %s.",
      "Blue points are displayed for any communities with counts higher than that regions 75th percentile.",
      "Hover over the blue points (if present) to see details about that specific community; hover over the boxplot (away from the points) to see the min/median/max and 25th/75th percentiles for that p/t.",
      "See the menu in the top right for more."
    ) |>
      sprintf(names(monitor_groups), fig_caption_date_range) |>
      setNames(monitor_groups),
    monitor_counts = paste(
      "Daily counts of active PA and FEM PM2.5 monitors in each Canadian p/t for %s.",
      "PA monitors are recorded as either owned by Environment and Climate Change Canada\'s Air Quality Science Units (ECCC AQSU) or by anyone else (\"Non-ECCC\").",
      "*Note: Sudden drops in monitors counts may be the result of our automated quality control and/or gaps in our database.*"
    ) |>
      sprintf(fig_caption_date_range),
    mean_map = 'Interactive map of seasonal mean PM2.5 concentrations ({{< var units.pm >}}) by Canadian forecast zone for %s. Hover over a zone for more information.' |>
      sprintf(fig_caption_date_range),
    max_map = 'Interactive map of seasonal max PM2.5 concentrations ({{< var units.pm >}}) by Canadian forecast zone for %s. Hover over a zone for more information.' |>
      sprintf(fig_caption_date_range),
    zone_ts = 'Daily mean PM2.5 concentrations ({{< var units.pm >}}) by Canadian forecast zone for each p/t for %s. The black line indicates the daily mean of the forcast zone averages.' |>
      sprintf(fig_caption_date_range),
    events_max = paste(
      "Daily percentage of forecast zones within each p/t with a daily maximum PM2.5 concentration within each AQHI+ risk group.",
      "White space indicates the percentage of forecast zones without observation data that day.",
      "See `Events Table` for a break down of events within each forecast zone for %s."
    ) |>
      sprintf(fig_caption_date_range),
    events_median = paste(
      "Daily percentage of forecast zones within each p/t with a daily median PM2.5 concentration within each AQHI+ risk group.",
      "White space indicates the percentage of forecast zones without observation data that day.",
      "See `Events Table` for a break down of events within each forecast zone for %s."
    ) |>
      sprintf(fig_caption_date_range),
    worst_events = paste(
      "Hourly PM2.5 concentrations from all FEM and PA monitors in the forecast zone for a selection of the worst events in %s for %s.",
      "Each event panel is labelled with the forecast zone name followed by the date range of the event. Summary text is included in the top left.",
      "*Note: Forecast zones vary in size, larger zones may have greater variation in observations due to physical distances between monitors.*"
    ) |>
      sprintf(names(provinces_n_territories), fig_caption_date_range) |>
      setNames(provinces_n_territories),
    community_exceeds_60 = paste(
      "Number of hours each community with nearby observation data had a mean PM2.5 concentration above 60 {{< var units.pm >}} in each p/t for %s.",
      "A monitor must be within 25 km of a community and not closer to a different community to be considered located in that community."
    ) |>
      sprintf(fig_caption_date_range),
    community_exceeds_100 = paste(
      "Number of hours each community with nearby observation data had a mean PM2.5 concentration above 100 {{< var units.pm >}} for each p/t.",
      "A monitor must be within 25 km of a community and not closer to a different community to be considered located in that community."
    ) |>
      sprintf(fig_caption_date_range)
  )

  # Insert terms defined in _variables.yml
  quarto_captions |>
    lapply(\(caption_vec) {
      caption_vec <- caption_vec |>
        # first occurence only
        stringr::str_replace(" AQmap", " {{< var terms.aqmap >}}") |>
        stringr::str_replace(" PM2.5", " {{< var terms.pm25.long >}}") |>
        stringr::str_replace(" PA", " {{< var terms.PA >}}") |>
        stringr::str_replace(" FEM", " {{< var terms.FEM >}}") |>
        # all other occurences if any
        stringr::str_replace_all(" PM2.5", " {{< var terms.pm25.short >}}") |>
        stringr::str_replace_all(" p/t", " {{< var terms.pt >}}")
      if (length(caption_vec) > 1) {
        as.list(caption_vec)
      } else {
        caption_vec
      }
    })
}
