make_report_date_ranges <- function(date_range) {
  out_formats <- list(
    utc = "%F %H UTC",
    vancouver = "%F %I %p",
    halifax = "%F %I %p"
  )
  ranges <- list(
    utc = date_range |>
      lubridate::with_tz("UTC"),
    vancouver = date_range |>
      lubridate::with_tz("America/Vancouver"),
    halifax = date_range |>
      lubridate::with_tz("America/Halifax")
  )

  ranges <- names(ranges) |>
    setNames(names(ranges)) |>
    lapply(\(tz) {
      range <- ranges[[tz]]
      range_fmt <- range |> format(out_formats[[tz]])
      if (format(range[1], "%F") == format(range[2], "%F")) {
        range_fmt[2] <- range_fmt[2] |>
          sub(pattern = strptime_to_regex("%F "), replacement = "")
      } else if (format(range[1], "%Y") == format(range[2], "%Y")) {
        range_fmt[2] <- range_fmt[2] |>
          sub(pattern = strptime_to_regex("%Y-"), replacement = "")
      }
      range_fmt |> paste(collapse = " to ")
    })

  ranges$utc <- ranges$utc |>
    sub(pattern = " UTC", replacement = "", fixed = TRUE) # remove first tz

  return(ranges)
}

build_overview_card <- function(
  type = c("daily", "monthly", "seasonal")[1],
  report_dropdown
) {
  texts <- list(
    daily = list(
      data_for_past = "24 hours",
      updated_every = "day at 00:00 UTC and 12:00 UTC"
    ),
    monthly = list(
      data_for_past = "month",
      updated_every = "month at 00:00 UTC"
    ),
    seasonal = list(
      data_for_past = "6 months",
      updated_every = "May and November at 00:00 UTC"
    )
  )[[type]]

  report_overview <- 'This automated report is a summary of the **fine particulate matter (PM<sub>2.5</sub>)** observation data in Canada
for the **past %s** from the regulatory **Federal Equivalent Method (FEM)** monitors
and the network of low-cost monitors from **PurpleAir (PA)**.
This report is automatically updated every %s.'

  report_details <- '<details>
<summary>Click here for more details.</summary>

<p>
  <strong>PM<sub>2.5</sub></strong> is a major constituent of wildfire smoke and has significant health risks associated with acute and chronic exposure.
  <strong>FEM monitors are the gold-standard</strong> for real-time data quality for PM<sub>2.5</sub>; however installations are limited by capital and maintenance costs.
  <strong>PA monitors are less accurate</strong> than their FEM counterparts, but are <strong>much less cost prohibitive</strong> allowing large numbers to be installed. 
  FEM monitors provide <strong>great</strong> data (in <strong>limited areas</strong>), but PA monitors provide <strong>good</strong> data (in <strong>many areas</strong>), and are very useful as "smoke detectors" during wildfire smoke events.
</p>

<p>All PM<sub>2.5</sub> data are sourced from the <a href="https://aqmap.ca/aqmap">UNBC AQmap</a> data repository.</p>

<ul>
  <li>
    Data from the FEM network originate from <a href="https://www.airnow.gov/about-airnow/">AirNow</a>, and are NOT VALIDATED. 
    No QA/QC is applied after retrieval from AirNow, and official values may differ from those presented here.
  </li>
  <li>
    Data from the PA network originate from the <a href="https://api.purpleair.com/">PurpleAir API</a>, and a rigorous automated QA/QC method is applied to ensure the best available data are used. 
    The <a href="https://amt.copernicus.org/articles/15/3315/2022/">UNBC/ECCC bias correction</a> is applied to all PA data to improve comparability with FEM values.
  </li>
</ul>

</details>'

  template <- "
::: overview-card

%s

:::"
  # see R/report-constants.R
  overview_content <- c(
    report_overview |>
      sprintf(texts$data_for_past, texts$updated_every),
    report_dropdown,
    report_details
  ) |>
    paste(collapse = "\n\n")

  template |> sprintf(overview_content) |> knitr::asis_output()
}

tab_explainers <- list(
  primary_tabs = "*Select your question from the following tabs*",
  network_tabs = "*Select which observation monitors to view using the following tabs*",
  scale_tabs = "*Select what scale/resolution to view using the following tabs*",
  prov_terr_tabs = "*Select which province/territory to view using the following tabs*",
  summary_tabs = "*Select which summary to view using the following tabs*"
) |>
  lapply(knitr::asis_output)

build_prov_donut_summary <- function(
  prov_donuts_text,
  type = c("daily", "monthly")[1]
) {
  average_text <- list(daily = "24-hour", monthly = "1-month")[[type]]

  template <- 'There was <strong>%s FEM sites</strong> and <strong>%s PA sites</strong> reporting PM<sub>2.5</sub> in Canada for this report (@fig-monitor_donuts_median_fem_and_pa, @fig-monitor_donuts_max_fem_and_pa). 

- <strong>%s</strong> of the FEM sites in Canada have a %s median exceeding 100 {{< var units.pm >}},
<strong>%s</strong> are between 60 and 100 {{< var units.pm >}},
and <strong>%s</strong> are between 30 and 60 {{< var units.pm >}} (@fig-monitor_donuts_median_fem_only). 
<strong>%s</strong> of the PA sites in Canada have a %s median exceeding 100 {{< var units.pm >}},
<strong>%s</strong> are between 60 and 100 {{< var units.pm >}},
and <strong>%s</strong> are between 30 and 60 {{< var units.pm >}} (@fig-monitor_donuts_median_pa_only).
  
- <strong>%s</strong> of the FEM sites in Canada have a %s maximum exceeding 100 {{< var units.pm >}},
<strong>%s</strong> are between 60 and 100 {{< var units.pm >}},
and <strong>%s</strong> are between 30 and 60 {{< var units.pm >}} (@fig-monitor_donuts_max_fem_only). 
<strong>%s</strong> of the PA sites in Canada have a %s maximum exceeding 100 {{< var units.pm >}},
<strong>%s</strong> are between 60 and 100 {{< var units.pm >}},
and <strong>%s</strong> are between 30 and 60 {{< var units.pm >}} (@fig-monitor_donuts_max_pa_only).'

  template |>
    sprintf(
      sum(prov_donuts_text$fem$median$n),
      sum(prov_donuts_text$pa$median$n),
      prov_donuts_text$fem$median[4] |> paste0("%"),
      average_text,
      prov_donuts_text$fem$median$p[3] |> paste0("%"),
      prov_donuts_text$fem$median$p[2] |> paste0("%"),
      prov_donuts_text$pa$median$p[4] |> paste0("%"),
      average_text,
      prov_donuts_text$pa$median$p[3] |> paste0("%"),
      prov_donuts_text$pa$median$p[2] |> paste0("%"),

      prov_donuts_text$fem$max$p[4] |> paste0("%"),
      average_text,
      prov_donuts_text$fem$max$p[3] |> paste0("%"),
      prov_donuts_text$fem$max$p[2] |> paste0("%"),
      prov_donuts_text$pa$max$p[4] |> paste0("%"),
      average_text,
      prov_donuts_text$pa$max$p[3] |> paste0("%"),
      prov_donuts_text$pa$max$p[2] |> paste0("%")
    ) |>
    make_summary_chunk() |>
    knitr::asis_output()
}

# TODO: cleanup
build_map_summary <- function(
  aqhi_p_counts,
  type = c("daily", "monthly")[1],
  worst_day = NULL
) {
  average_text <- list(daily = "24-hour", monthly = "1-month")[[type]]

  very_high_fem_by_prov <- aqhi_p_counts$by_prov |>
    subset(FEM != 0 & aqhi_p_24hr == "Very High") |>
    dplyr::arrange(dplyr::desc(FEM))
  high_fem_by_prov <- aqhi_p_counts$by_prov |>
    subset(FEM != 0 & aqhi_p_24hr == "High") |>
    dplyr::arrange(dplyr::desc(PA))
  mod_fem_by_prov <- aqhi_p_counts$by_prov |>
    subset(FEM != 0 & aqhi_p_24hr == "Moderate") |>
    dplyr::arrange(dplyr::desc(PA))

  very_high_pa_by_prov <- aqhi_p_counts$by_prov |>
    subset(PA != 0 & aqhi_p_24hr == "Very High") |>
    dplyr::arrange(dplyr::desc(FEM))
  high_pa_by_prov <- aqhi_p_counts$by_prov |>
    subset(PA != 0 & aqhi_p_24hr == "High") |>
    dplyr::arrange(dplyr::desc(PA))
  mod_pa_by_prov <- aqhi_p_counts$by_prov |>
    subset(PA != 0 & aqhi_p_24hr == "Moderate") |>
    dplyr::arrange(dplyr::desc(FEM))

  if (!is.null(worst_day)) {
    wd <- worst_day |>
      dplyr::select(
        p = `Prov./Terr.`,
        w = `Worst Day`,
        m = `Mean of Site Means`
      ) |>
      dplyr::arrange(p) |>
      dplyr::group_by(w) |>
      dplyr::summarise(
        m = join_list_sentence(m) |>
          paste0(ifelse(length(p) > 1, ", respectively", "")),
        p = join_list_sentence(p, type = "provinces/territories")
      ) |>
      dplyr::mutate(
        t = paste0(
          "- ",
          p,
          ": ",
          w,
          " - 24-hour mean PM<sub>2.5</sub>: ",
          m,
          " {{< var units.pm >}}"
        )
      )
  } else {
    wd <- list(t = "")
  }

  template <- 'There was <strong>%s FEM monitors</strong> and <strong>%s PA monitors</strong> in Canada
with a %s mean PM<sub>2.5</sub> concentration <strong>exceeding 100 {{< var units.pm >}}</strong>
*(very high AQHI risk)* (@tbl-overall_table_fem_and_pa, @fig-site_mean_map_fem_and_pa).%s%s

There was <strong>%s FEM monitors</strong> and <strong>%s PA monitors</strong> in Canada
with a %s mean PM<sub>2.5</sub> concentration <strong>between 60 and 100 {{< var units.pm >}}</strong>
*(high AQHI risk)*.%s%s
  
There was <strong>%s FEM monitors</strong> and <strong>%s PA monitors</strong> in Canada
with a %s mean PM<sub>2.5</sub> concentration <strong>between 30 and 60 {{< var units.pm >}}</strong>
*(moderate AQHI risk)*.%s%s
  
%s'

  if (!is.null(worst_day)) {
    wd_text <- "The worst overall day for each province/territory:\n\n%s\n\n</details>" |>
      sprintf(paste(wd$t, collapse = "\n\n"))
  } else {
    wd_text <- ""
  }

  template |>
    sprintf(
      aqhi_p_counts$overall$FEM[4],
      aqhi_p_counts$overall$PA[4],
      average_text,
      format_count_summary(very_high_fem_by_prov, "FEM", "exceeding 100"),
      format_count_summary(very_high_pa_by_prov, "PA", "exceeding 100"),
      aqhi_p_counts$overall$FEM[3],
      aqhi_p_counts$overall$PA[3],
      average_text,
      format_count_summary(high_fem_by_prov, "FEM", "between 60 and 100"),
      format_count_summary(high_pa_by_prov, "PA", "between 60 and 100"),
      aqhi_p_counts$overall$FEM[2],
      aqhi_p_counts$overall$PA[2],
      average_text,
      format_count_summary(mod_fem_by_prov, "FEM", "between 30 and 60"),
      format_count_summary(mod_pa_by_prov, "PA", "between 30 and 60"),
      wd_text
    ) |>
    make_summary_chunk() |>
    knitr::asis_output()
}

format_count_summary <- function(
  dat,
  monitor = "FEM",
  range_text = "exceeding 100"
) {
  if (nrow(dat) > 1) {
    x <- paste0(" <strong>", dat[[monitor]], "</strong>") |>
      paste(
        unlist(dat[, 'prov_terr']),
        sep = paste0(" ", monitor, " monitor(s) in ")
      )

    x[-length(x)] |>
      paste(collapse = ", ") |>
      paste0(
        ', and ',
        dplyr::last(x),
        " had a mean PM<sub>2.5</sub> concentration ",
        range_text,
        " {{< var units.pm >}}."
      )
  } else if (nrow(dat) == 1) {
    paste0(" <strong>", dat[[monitor]], "</strong>") |>
      paste(
        unlist(dat[, 'prov_terr']),
        sep = paste0(" ", monitor, " monitor(s) in ")
      ) |>
      paste0(
        " had a mean PM<sub>2.5</sub> concentration ",
        range_text,
        " {{< var units.pm >}}."
      )
  } else {
    ""
  }
}

# TODO: cleanup
build_boxplot_summary <- function(
  boxplot_vals,
  type = c("daily", "monthly")[1]
) {
  average_text <- list(daily = "24-hour", monthly = "1-month")[[type]]
  template <- '
*%s* (a %s monitor located %s km from %s, %s) had the highest observed hourly maximum PM<sub>2.5</sub>
concentration in Canada from the FEM network for this report (%s {{< var units.pm >}}) (@fig-site_mean_boxplots_fem_and_pa, @fig-site_mean_boxplots_fem_only).
*%s* (a %s monitor located %s km from %s, %s) had the highest observed %s mean PM<sub>2.5</sub>
concentration in Canada from the FEM network (%s {{< var units.pm >}}).

*%s* (a %s monitor located %s km from %s, %s) had the highest observed hourly maximum PM<sub>2.5</sub>
concentration in Canada from the PA network for this report (%s {{< var units.pm >}})  (@fig-site_mean_boxplots_fem_and_pa, @fig-site_mean_boxplots_pa_only).
*%s* (a %s monitor located %s km from %s, %s) had the highest observed %s mean PM<sub>2.5</sub>
concentration in Canada from the PA network (%s {{< var units.pm >}}).'

  template |>
    sprintf(
      boxplot_vals$FEM[[1]]$M,
      boxplot_vals$FEM[[1]]$T,
      boxplot_vals$FEM[[1]]$D,
      boxplot_vals$FEM[[1]]$C,
      boxplot_vals$FEM[[1]]$P,
      boxplot_vals$FEM[[1]]$PM,
      boxplot_vals$FEM[[2]]$M,
      boxplot_vals$FEM[[2]]$T,
      boxplot_vals$FEM[[2]]$D,
      boxplot_vals$FEM[[2]]$C,
      boxplot_vals$FEM[[2]]$P,
      average_text,
      boxplot_vals$FEM[[2]]$PM,
      boxplot_vals$PA[[1]]$M,
      boxplot_vals$PA[[1]]$T,
      boxplot_vals$PA[[1]]$D,
      boxplot_vals$PA[[1]]$C,
      boxplot_vals$PA[[1]]$P,
      boxplot_vals$PA[[1]]$PM,
      boxplot_vals$PA[[2]]$M,
      boxplot_vals$PA[[2]]$T,
      boxplot_vals$PA[[2]]$D,
      boxplot_vals$PA[[2]]$C,
      boxplot_vals$PA[[2]]$P,
      average_text,
      boxplot_vals$PA[[2]]$PM
    ) |>
    make_summary_chunk() |>
    knitr::asis_output()
}

build_prov_grid_summary <- function(
  grid_data,
  type = c("daily", "monthly")[1]
) {
  average_text <- list(daily = "hours", monthly = "days")[[type]]
  p <- grid_data |>
    lapply(\(stat_data) {
      stat_data |>
        dplyr::group_by(y) |>
        dplyr::summarise(n = sum(as.numeric(fill) > 3, na.rm = TRUE)) |>
        dplyr::filter(n >= 3) |>
        dplyr::arrange(dplyr::desc(n)) |>
        dplyr::summarise(
          text = join_list_sentence(y, type = "provinces/territories")
        ) |>
        dplyr::pull(text)
    })

  template <- "%s had a significant amount of locations with elevated
PM<sub>2.5</sub> concentrations  (> 30 {{< var units.pm >}})
for at least 3 %s (See @fig-prov_medianpeak_grid_fem_and_pa \"Median\"). 
%s had at least one location with elevated 
PM<sub>2.5</sub> concentrations (> 30 {{< var units.pm >}})
for at least 3 %s (See @fig-prov_medianpeak_grid_fem_and_pa \"Maximum\")."

  template |>
    sprintf(p$median, average_text, p$maximum, average_text) |>
    make_summary_chunk() |>
    knitr::asis_output()
}

build_fcst_grid_summary <- function(
  grid_data,
  type = c("daily", "monthly")[1]
) {
  text <- grid_data |>
    dplyr::filter(y != "Not inside a zone") |>
    dplyr::summarise(n = sum(as.numeric(fill) > 3), .by = c(z, y)) |>
    dplyr::filter(n >= 3) |>
    dplyr::arrange(z, desc(n))

  if (nrow(text) == 0) {
    text <- ""
  } else {
    text <- "- %s (@fig-zone_median_grid_%s): %s" |>
      sprintf(
        unique(text$z),
        unique(text$z),
        unique(text$z) |>
          sapply(\(p) {
            text$y[text$z == p] |>
              join_list_sentence(oxford = TRUE)
          })
      ) |>
      paste(collapse = "\n\n")
  }

  average_text <- list(daily = "hours", monthly = "days")[[type]]
  template <- "The following forecast regions experienced elevated PM<sub>2.5</sub> concentrations 
(> 30 {{< var units.pm >}}) for at least 3 %s):

%s"

  template |>
    sprintf(average_text, text) |>
    make_summary_chunk() |>
    knitr::asis_output()
}

build_community_summary <- function(
  community_summary,
  type = c("daily", "monthly")[1]
) {
  average_text <- list(daily = "24-hour", monthly = "1-month")[[type]]
  pd <- community_summary |>
    dplyr::select(
      P = prov_terr,
      C = nearest_community,
      N = n_monitors,
      D = nc_dist_km_network_max_comm_max,
      PM1 = pm25_mean_network_mean_comm_mean,
      PM2 = pm25_max_network_max_comm_max,
      H1 = n_hours_above_100_network_max_comm_max,
      H2 = n_hours_above_60_network_max_comm_max,
      H3 = n_hours_above_30_network_max_comm_max
    ) |>
    dplyr::mutate(
      C = C |> escape_md(),
      NFEM = N |>
        stringr::str_extract("FEM: \\d*") |>
        stringr::str_remove("FEM: ") |>
        handyr::swap(NA, with = "0"),
      NPA = N |>
        stringr::str_extract("PA: \\d*") |>
        stringr::str_remove("PA: ") |>
        handyr::swap(NA, with = "0")
    )
  community_table_vals <- list(
    pd |>
      dplyr::arrange(desc(PM2)) |>
      head(1),
    pd |>
      dplyr::arrange(desc(PM1)) |>
      head(1),
    pd |>
      dplyr::arrange(desc(H1), desc(H2), desc(H3)) |>
      head(1)
  )

  template <- "*%s* (a community in %s with %s FEM and %s PA monitors within at least %s km)
had the highest observed hourly PM<sub>2.5</sub> concentration in Canada for this report (%s {{< var units.pm >}}) (@tbl-community_summary). 
*%s* (a community in %s with %s FEM and %s PA monitors within at least %s km) 
had the highest %s mean in Canada (%s {{< var units.pm >}}).

*%s* (a community in %s with %s FEM and %s PA monitors within at least %s km)
had %s hours where at least one monitor exceeded 100 {{< var units.pm >}}, 
%s hours where at least one monitor exceeded 60 {{< var units.pm >}}, 
and %s hours where at least one monitor exceeded 30 {{< var units.pm >}}."

  template |>
    sprintf(
      community_table_vals[[1]]$C,
      community_table_vals[[1]]$P,
      community_table_vals[[1]]$NFEM,
      community_table_vals[[1]]$NPA,
      community_table_vals[[1]]$D,
      community_table_vals[[1]]$PM2,
      community_table_vals[[2]]$C,
      community_table_vals[[2]]$P,
      community_table_vals[[2]]$NFEM,
      community_table_vals[[2]]$NPA,
      community_table_vals[[2]]$D,
      average_text,
      community_table_vals[[2]]$PM1,
      community_table_vals[[3]]$C,
      community_table_vals[[3]]$P,
      community_table_vals[[3]]$NFEM,
      community_table_vals[[3]]$NPA,
      community_table_vals[[3]]$D,
      community_table_vals[[3]]$H1,
      community_table_vals[[3]]$H2,
      community_table_vals[[3]]$H3
    ) |>
    make_summary_chunk() |>
    knitr::asis_output()
}

build_coverage_summary <- function(coverage_data) {
  p_summ = coverage_data |>
    dplyr::group_by(pt = `Prov./Terr.`) |>
    dplyr::mutate(
      pop_fnic = as.numeric(stringr::str_remove_all(pop_fnic, ",")),
      pop_rural = as.numeric(stringr::str_remove_all(pop_rural, ",")),
      pop_urban = as.numeric(stringr::str_remove_all(pop_urban, ",")),
      pop_total = as.numeric(stringr::str_remove_all(pop_total, ",")),
      total_fnic = sum(pop_fnic, na.rm = TRUE),
      total_rural = sum(pop_rural, na.rm = TRUE),
      total_urban = sum(pop_urban, na.rm = TRUE),
      total_total = sum(pop_total, na.rm = TRUE)
    ) |>
    dplyr::filter(`Within 25km of` != "No Monitor") |>
    dplyr::summarise(
      p_fnic = round(sum(pop_fnic / total_fnic, na.rm = TRUE) * 100, 1),
      p_urban = round(sum(pop_rural / total_rural, na.rm = TRUE) * 100, 1),
      p_rural = round(sum(pop_urban / total_urban, na.rm = TRUE) * 100, 1),
      p_total = round(sum(pop_total / total_total, na.rm = TRUE) * 100, 1)
    )
  reg_summ = coverage_data |>
    dplyr::group_by(Region) |>
    dplyr::mutate(
      pop_fnic = as.numeric(stringr::str_remove_all(pop_fnic, ",")),
      pop_rural = as.numeric(stringr::str_remove_all(pop_rural, ",")),
      pop_urban = as.numeric(stringr::str_remove_all(pop_urban, ",")),
      pop_total = as.numeric(stringr::str_remove_all(pop_total, ",")),
      total_fnic = sum(pop_fnic, na.rm = TRUE),
      total_rural = sum(pop_rural, na.rm = TRUE),
      total_urban = sum(pop_urban, na.rm = TRUE),
      total_total = sum(pop_total, na.rm = TRUE)
    ) |>
    dplyr::filter(`Within 25km of` != "No Monitor") |>
    dplyr::summarise(
      p_fnic = round(sum(pop_fnic / total_fnic, na.rm = TRUE) * 100, 1),
      p_urban = round(sum(pop_rural / total_rural, na.rm = TRUE) * 100, 1),
      p_rural = round(sum(pop_urban / total_urban, na.rm = TRUE) * 100, 1),
      p_total = round(sum(pop_total / total_total, na.rm = TRUE) * 100, 1)
    )

  template <- "Across Canada, %s of the population (%s of rural and %s of urban)
and %s of Indigenous communities were within 25 km of a regulatory
or low-cost PurpleAir PM<sub>2.5</sub> monitor during this report.

In the west, %s of the *total population* was within 25 km of a PA or FEM PM<sub>2.5</sub> monitor
(%s for BC, %s for AB, %s for SK and %s for MB). 
For the central provinces, %s of the population (%s for ON and %s for QC) was within 25 km of a PM<sub>2.5</sub> monitor. 
In the east, %s of the population (%s for NS, %s for NB, %s for NL and %s for PE) was within 25 km of a PM<sub>2.5</sub> monitor. 
For the territories, %s of the population (%s for YT, %s for NT and %s for NU) was within 25 km of a PM<sub>2.5</sub> monitor.

In the west, %s of the *Indigenous communities* were within 25 km of a PA or FEM PM<sub>2.5</sub> monitor
(%s for BC, %s for AB, %s for SK and %s for MB). 
For the central provinces, %s of the Indigenous communities (%s for ON and %s for QC) was within 25 km of a PM<sub>2.5</sub> monitor. 
In the east, %s of the Indigenous communities (%s for NS, %s for NB, %s for NL and %s for PE) was within 25 km of a PM<sub>2.5</sub> monitor. 
For the territories, %s of the Indigenous communities (%s for YT, %s for NT and %s for NU) was within 25 km of a PM<sub>2.5</sub> monitor.
  
In the west, %s of the *urban population* was within 25 km of a PA or FEM PM<sub>2.5</sub> monitor
(%s for BC, %s for AB, %s for SK and %s for MB). 
For the central provinces, %s of the urban population (%s for ON and %s for QC) was within 25 km of a PM<sub>2.5</sub> monitor. 
In the east, %s of the urban population (%s for NS, %s for NB, %s for NL and %s for PE) was within 25 km of a PM<sub>2.5</sub> monitor. 
For the territories, %s of the urban population (%s for YT, %s for NT and %s for NU) was within 25 km of a PM<sub>2.5</sub> monitor.

In the west, %s of the *rural population* was within 25 km of a PA or FEM PM<sub>2.5</sub> monitor
(%s for BC, %s for AB, %s for SK and %s for MB). 
For the central provinces, %s of the rural population (%s for ON and %s for QC) was within 25 km of a PM<sub>2.5</sub> monitor. 
In the east, %s of the rural population (%s for NS, %s for NB, %s for NL and %s for PE) was within 25 km of a PM<sub>2.5</sub> monitor. 
For the territories, %s of the rural population (%s for YT, %s for NT and %s for NU) was within 25 km of a PM<sub>2.5</sub> monitor.
  "

  template |>
    sprintf(
      dplyr::filter(reg_summ, Region == "Canada")$p_total[1] |> paste0("%"),
      dplyr::filter(reg_summ, Region == "Canada")$p_rural[1] |> paste0("%"),
      dplyr::filter(reg_summ, Region == "Canada")$p_urban |> paste0("%"),
      dplyr::filter(reg_summ, Region == "Canada")$p_fnic[1] |> paste0("%"),
      dplyr::filter(reg_summ, Region == "West")$p_total[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "BC")$p_total[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "AB")$p_total[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "SK")$p_total[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "MB")$p_total[1] |> paste0("%"),
      dplyr::filter(reg_summ, Region == "Central")$p_total[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "ON")$p_total[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "QC")$p_total[1] |> paste0("%"),
      dplyr::filter(reg_summ, Region == "East")$p_total[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "NS")$p_total[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "NB")$p_total[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "NL")$p_total[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "PE")$p_total[1] |> paste0("%"),
      dplyr::filter(reg_summ, Region == "North")$p_total[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "YK")$p_total[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "NT")$p_total[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "NU")$p_total[1] |> paste0("%"),
      dplyr::filter(reg_summ, Region == "West")$p_fnic[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "BC")$p_fnic[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "AB")$p_fnic[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "SK")$p_fnic[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "MB")$p_fnic[1] |> paste0("%"),
      dplyr::filter(reg_summ, Region == "Central")$p_fnic[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "ON")$p_fnic[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "QC")$p_fnic[1] |> paste0("%"),
      dplyr::filter(reg_summ, Region == "East")$p_fnic[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "NS")$p_fnic[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "NB")$p_fnic[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "NL")$p_fnic[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "PE")$p_fnic[1] |> paste0("%"),
      dplyr::filter(reg_summ, Region == "North")$p_fnic[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "YK")$p_fnic[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "NT")$p_fnic[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "NU")$p_fnic[1] |> paste0("%"),
      dplyr::filter(reg_summ, Region == "West")$p_urban[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "BC")$p_urban[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "AB")$p_urban[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "SK")$p_urban[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "MB")$p_urban[1] |> paste0("%"),
      dplyr::filter(reg_summ, Region == "Central")$p_urban[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "ON")$p_urban[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "QC")$p_urban[1] |> paste0("%"),
      dplyr::filter(reg_summ, Region == "East")$p_urban[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "NS")$p_urban[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "NB")$p_urban[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "NL")$p_urban[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "PE")$p_urban[1] |> paste0("%"),
      dplyr::filter(reg_summ, Region == "North")$p_urban[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "YK")$p_urban[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "NT")$p_urban[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "NU")$p_urban[1] |> paste0("%"),
      dplyr::filter(reg_summ, Region == "West")$p_rural[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "BC")$p_rural[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "AB")$p_rural[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "SK")$p_rural[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "MB")$p_rural[1] |> paste0("%"),
      dplyr::filter(reg_summ, Region == "Central")$p_rural[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "ON")$p_rural[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "QC")$p_rural[1] |> paste0("%"),
      dplyr::filter(reg_summ, Region == "East")$p_rural[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "NS")$p_rural[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "NB")$p_rural[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "NL")$p_rural[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "PE")$p_rural[1] |> paste0("%"),
      dplyr::filter(reg_summ, Region == "North")$p_rural[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "YK")$p_rural[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "NT")$p_rural[1] |> paste0("%"),
      dplyr::filter(p_summ, pt == "NU")$p_rural[1] |> paste0("%")
    ) |>
    make_summary_chunk() |>
    knitr::asis_output()
}
