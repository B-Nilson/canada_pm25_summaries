make_summaries <- function(
  obs,
  type = c("daily", "monthly", "seasonal")[1],
  meta_cols,
  fcst_zones,
  report_dir = ".",
  figure_dir = "plots",
  plot_timestamp = format(Sys.time(), "%Y%m%d%H%M%S")
) {
  summaries <- list(
    overall = obs |> make_overall_summary(meta_cols = meta_cols),
    current = obs |> dplyr::filter(date == max(date))
  )
  summaries$overall <- summaries$overall |>
    dplyr::left_join(
      summaries$current |> dplyr::select(site_id, monitor, pm25_current = pm25),
      by = c('site_id', 'monitor')
    )

  summaries$by_zone <- summaries$overall |>
    make_zone_summary(fcst_zones = fcst_zones)

  if (type != "daily") {
    summaries$daily <- obs |>
      make_period_summary(period = "days")

    summaries$monthly <- obs |>
      make_period_summary(period = "months")

    summaries$daily_by_zone <- summaries$daily |>
      make_period_by_zone_summary(
        fcst_zones = fcst_zones,
        fcst_zones_clean = fcst_zones,
        period_label = "24-hour",
        period_date_fmt = "%F"
      )

    summaries$monthly_by_zone <- summaries$monthly |>
      make_period_by_zone_summary(
        fcst_zones = fcst_zones,
        fcst_zones_clean = fcst_zones,
        period_label = "Monthly",
        period_date_fmt = "%F %HZ" # TODO: why hours?
      )
  }

  if (type != "seasonal") {
    summaries$community <- summaries$overall |>
      make_community_summary()

    summaries$donuts <- summaries$overall |>
      save_aqhi_donuts_plots(
        monitor_groups = monitor_groups,
        stats = c("mean", "max"),
        avg = type |>
          dplyr::recode_values(
            from = c("daily", "monthly"),
            to = c("24-hour", "Monthly")
          ),
        report_dir = report_dir,
        figure_dir = figure_dir,
        plot_timestamp = plot_timestamp
      )
    
    summaries$aqhi_p_counts <- summaries$overall |>
      make_aqhi_p_count_summaries()
  }
  if (type == "monthly") {
    summaries$worst_day <- summaries$daily |>
      make_worst_day_summary()
  }

  return(summaries)
}

make_overall_summary <- function(obs, meta_cols) {
  obs_cols <- c("pm25", "pm25_a", "pm25_b", "rh", "temperature")
  obs |>
    # Mark days where multiple hours exceeded 100 ug/m3
    dplyr::group_by(
      date = lubridate::floor_date(date, "days"),
      dplyr::pick(dplyr::all_of(meta_cols))
    ) |>
    dplyr::mutate(is_over_100 = sum(pm25 >= 100, na.rm = TRUE) >= 12) |>
    dplyr::group_by(monitor, dplyr::pick(dplyr::all_of(meta_cols))) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::any_of(obs_cols),
        list(
          mean = safe_mean,
          max = \(x) handyr::max(x, na.rm = TRUE)
        )
      ),
      n_hours_observed = sum(!is.na(pm25)),
      n_hours_above_30 = sum(pm25 >= 30, na.rm = TRUE),
      n_hours_above_60 = sum(pm25 >= 60, na.rm = TRUE),
      n_hours_above_100 = sum(pm25 >= 100, na.rm = TRUE),
      days_w_over_100 = unique(date[is_over_100]) |>
        format("%m-%d") |>
        paste(collapse = ","),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(pm25_mean))
}

make_zone_summary <- function(overall_summary, fcst_zones) {
  overall_summary |>
    dplyr::mutate(
      fcst_zone = fcst_zone |> dplyr::replace_values(NA ~ "Not inside a zone")
    ) |>
    dplyr::group_by(fcst_zone, monitor) |>
    dplyr::summarise(
      n_monitors = dplyr::n(),
      mean_pm25_24hr_mean = pm25_mean |> safe_mean(),
      dplyr::across(
        dplyr::starts_with("n_hours_above"),
        list(max = \(x) handyr::max(x, na.rm = TRUE))
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = monitor,
      values_from = -c(monitor, fcst_zone)
    ) |>
    dplyr::full_join(
      fcst_zones |> dplyr::select(fcst_zone, fcst_zone_fr),
      by = c('fcst_zone')
    ) |>
    sf::st_as_sf() |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("n_"),
        \(x) x |> dplyr::replace_values(NA ~ 0)
      ),
      mean_pm25_24hr_mean = ((dplyr::replace_values(
        mean_pm25_24hr_mean_PA * n_monitors_PA,
        NA ~ 0
      ) +
        dplyr::replace_values(
          mean_pm25_24hr_mean_FEM * n_monitors_FEM,
          NA ~ 0
        )) /
        (n_monitors_PA + n_monitors_FEM)) |>
        round(digits = 1)
    )
}

make_zone_hover <- function(
  date,
  fcst_zone,
  n,
  n_fem,
  n_pa,
  pm25_mean,
  pm25_max,
  date_fmt = "%Y-%m-%d",
  avg_text = "24-hour"
) {
  date_str <- date |> format(date_fmt)
  paste(
    "<big><strong>%s</strong></big>",
    "Date: <b>%s</b>",
    "<b>%s monitors</b> (FEM: %s, PA: %s)",
    "%s mean PM<sub>2.5</sub>: <b>%s &mu;g m<sup>-3</sup></b>",
    "%s max PM<sub>2.5</sub>: <b>%s &mu;g m<sup>-3</sup></b>",
    sep = "<br>"
  ) |>
    sprintf(
      fcst_zone,
      date_str,
      n,
      n_fem,
      n_pa,
      avg_text,
      pm25_mean,
      avg_text,
      pm25_max
    ) |>
    stringr::str_replace_all("NA &mu;g m<sup>-3</sup>", "No Data.")
}

make_period_summary <- function(obs, period = "days") {
  obs |>
    dplyr::group_by(
      date = lubridate::floor_date(date, period),
      monitor,
      dplyr::pick(dplyr::any_of(meta_cols))
    ) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        list(
          mean = safe_mean,
          max = \(x) handyr::max(x, na.rm = TRUE)
        )
      ),
      .groups = "drop"
    )
}

make_period_by_zone_summary <- function(
  period_summary,
  period_label = "24-hour",
  period_date_fmt = "%F",
  fcst_zones,
  fcst_zones_clean
) {
  period_summary |>
    dplyr::summarise(
      pm25_mean = pm25 |> safe_mean(),
      pm25_max = pm25 |> handyr::max(na.rm = TRUE),
      n = dplyr::n(),
      n_fem = sum(monitor == "FEM"),
      n_pa = sum(monitor == "PA"),
      .by = c("date", "prov_terr", "fcst_zone")
    ) |>
    tidyr::complete(fcst_zone = fcst_zones$fcst_zone, date) |>
    dplyr::select(-prov_terr) |>
    dplyr::left_join(
      fcst_zones_clean |>
        dplyr::left_join(
          fcst_zones |> dplyr::select(fcst_zone, geometry),
          by = "fcst_zone"
        ),
      by = "fcst_zone"
    ) |>
    sf::st_as_sf() |>
    dplyr::mutate(
      dplyr::across(
        c(dplyr::starts_with("n_"), n),
        \(x) x |> dplyr::replace_values(NA ~ 0)
      ),
      label = date |>
        make_zone_hover(
          fcst_zone = fcst_zone,
          n = n,
          n_fem = n_fem,
          n_pa = n_pa,
          pm25_mean = pm25_mean,
          pm25_max = pm25_max,
          date_fmt = period_date_fmt,
          avg_text = period_label
        )
    )
}

make_community_summary <- function(overall_summary) {
  overall_summary |>
    dplyr::group_by(
      prov_terr,
      fcst_zone,
      nearest_community,
      nc_lat,
      nc_lng,
      monitor
    ) |>
    dplyr::summarise(
      n_monitors = dplyr::n(),
      dplyr::across(
        c(dplyr::starts_with(c("nc_dist", "pm25", "n_hours"))),
        list(
          network_mean = safe_mean,
          network_max = \(x) handyr::max(x, na.rm = TRUE)
        )
      ),
      .groups = "drop_last"
    ) |>
    dplyr::summarise(
      n_monitors = paste(paste0(monitor, ": ", n_monitors), collapse = " | "),
      dplyr::across(
        c(dplyr::starts_with(c("nc_dist", "pm25", "n_hours"))),
        list(
          comm_mean = safe_mean,
          comm_max = \(x) handyr::max(x, na.rm = TRUE)
        )
      ),
      .groups = "drop"
    )
}

make_worst_day_summary <- function(daily_summary) {
  daily_summary |>
    dplyr::mutate(
      n_communities_ge_100 = as.numeric(pm25_max >= 100),
      .by = c("nearest_community", "date")
    ) |>
    dplyr::summarise(
      n_sites = dplyr::n(),
      n_communities_ge_100 = sum(n_communities_ge_100, na.rm = TRUE),
      n_sites_ge_100 = sum(pm25_max >= 100, na.rm = TRUE),
      dplyr::across(
        c(dplyr::starts_with(c("pm25"))),
        list(
          mean = safe_mean,
          min = \(x) handyr::min(x, na.rm = TRUE),
          max = \(x) handyr::max(x, na.rm = TRUE)
        )
      ),
      .by = c("prov_terr", "date")
    ) |>
    dplyr::arrange(
      prov_terr,
      dplyr::desc(n_communities_ge_100),
      dplyr::desc(n_sites_ge_100),
      dplyr::desc(pm25_mean_mean),
      dplyr::desc(pm25_max_mean)
    ) |>
    dplyr::filter(!duplicated(prov_terr)) |>
    dplyr::mutate(
      worst_day = date |> format("%B %d (%a)"),
      n_ge_100 = "%s / %s (%s" |>
        sprintf(
          n_sites_ge_100,
          n_sites,
          round(n_sites_ge_100 / n_sites * 100, 1)
        ) |>
        paste0("%)"),
    )
}

make_aqhi_p_count_summaries <- function(overall_summary) {
  placeholders <- unique(overall_summary$monitor) |> 
    lapply(\(x) 0) |> 
    setNames(unique(overall_summary$monitor))
  aqhi_p_counts <- list()
  aqhi_p_counts$by_prov <- overall_summary |>
    dplyr::group_by(
      aqhi_p_24hr = aqhi::AQHI_plus(pm25_mean)$risk,
      prov_terr,
      monitor
    ) |>
    dplyr::summarise(n_monitors = dplyr::n(), .groups = "drop") |>
    tidyr::pivot_wider(names_from = "monitor", values_from = "n_monitors") |>
    tidyr::complete(
      aqhi_p_24hr = levels(aqhi_p_24hr) |> factor(levels = levels(aqhi_p_24hr)),
      prov_terr = levels(prov_terr) |> factor(levels = levels(prov_terr)),
      fill = placeholders
    ) |>
    dplyr::arrange(aqhi_p_24hr, prov_terr)

  aqhi_p_counts$overall <- aqhi_p_counts$by_prov |>
    dplyr::select(-prov_terr) |>
    dplyr::group_by(aqhi_p_24hr) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), sum))

  return(aqhi_p_counts)
}
