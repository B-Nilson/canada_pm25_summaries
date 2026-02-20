build_header <- function(
  type = c("daily", "monthly", "seasonal")[1],
  date_range,
  months_in_seasons = list(
    "Summer" = 5:10,
    "Winter" = c(11:12, 1:4)
  )
) {
  template <- '<center><h1>Canadian PM<sub>2.5</sub> Observations %s Summary</h1></center>
<center><h3>Non-validated Data for <mark class="bg-info">%s</mark></center>
<center><h3>Representing %s</h3></center>
<center><h4>%s in Vancouver | %s in Halifax</h4></center>'

  report_name <- date_range[2] |>
    get_report_name(type = type, months_in_seasons = months_in_seasons) |>
    parse_report_name(type = type)

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

  template |>
    sprintf(
      stringr::str_to_title(type),
      report_name,
      ranges$utc,
      ranges$vancouver,
      ranges$halifax
    ) |>
    knitr::asis_output()
}

build_overview_card <- function(
  type = c("daily", "monthly", "seasonal")[1],
  report_dropdown,
  contact_email
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

  contact_details <- "For any concerns, questions, or feedback regarding this report or the data within it, please contact %s" |>
    sprintf(contact_email)

  template <- "
::: card

:::: card-body

%s

::::

:::"
  # see R/report-constants.R
  overview_content <- c(
    report_overview |>
      sprintf(texts$data_for_past, texts$updated_every),
    report_dropdown,
    contact_details,
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

  template <- 'There was <strong>%s FEM sites</strong> and <strong>%s PA sites</strong> reporting PM<sub>2.5</sub> in Canada for this report. 

- <strong>%s</strong> of the FEM sites in Canada have a %s mean exceeding 100 {{< pm_units >}},
<strong>%s</strong> are between 60 and 100 {{< pm_units >}},
and <strong>%s</strong> are between 30 and 60 {{< pm_units >}}. 

- <strong>%s</strong> of the PA sites in Canada have a %s mean exceeding 100 {{< pm_units >}},
<strong>%s</strong> are between 60 and 100 {{< pm_units >}},
and <strong>%s</strong> are between 30 and 60 {{< pm_units >}}.'

  template |>
    sprintf(
      sum(prov_donuts_text$p$fem$n),
      sum(prov_donuts_text$p$pa$n),
      prov_donuts_text$p$fem$p[4] |> paste0("%"),
      average_text,
      prov_donuts_text$p$fem$p[3] |> paste0("%"),
      prov_donuts_text$p$fem$p[2] |> paste0("%"),
      prov_donuts_text$p$pa$p[4] |> paste0("%"),
      average_text,
      prov_donuts_text$p$pa$p[3] |> paste0("%"),
      prov_donuts_text$p$pa$p[2] |> paste0("%")
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
  very_high_pa_by_prov <- aqhi_p_counts$by_prov |>
    subset(PA != 0 & aqhi_p_24hr == "Very High") |>
    dplyr::arrange(dplyr::desc(FEM))
  high_pa_by_prov <- aqhi_p_counts$by_prov |>
    subset(PA != 0 & aqhi_p_24hr == "High") |>
    dplyr::arrange(dplyr::desc(PA))

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
        p = join_list_sentence(p)
      ) |>
      dplyr::mutate(
        t = paste0(
          "- ",
          p,
          ": ",
          w,
          " - 24-hour mean PM<sub>2.5</sub>: ",
          m,
          " {{< pm_units >}}"
        )
      )
  } else {
    wd <- list(t = "")
  }

  template <- 'There was <strong>%s FEM monitors</strong> and <strong>%s PA monitors</strong> in Canada
with a %s mean PM<sub>2.5</sub> concentration <strong>exceeding 100 {{< pm_units >}}</strong>

%s

%s

There was <strong>%s FEM monitors</strong> and <strong>%s PA monitors</strong> in Canada
with a %s mean PM<sub>2.5</sub> concentration <strong>between 60 and 100 {{< pm_units >}}</strong>
  
%s

%s
  
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
      paste0(
        ifelse(nrow(very_high_fem_by_prov), "* ", ""),
        format_count_summary(very_high_fem_by_prov, "FEM", "exceeding 100")
      ),
      paste0(
        ifelse(nrow(very_high_pa_by_prov), "* ", ""),
        format_count_summary(very_high_pa_by_prov, "PA", "exceeding 100")
      ),
      aqhi_p_counts$overall$FEM[3],
      aqhi_p_counts$overall$PA[3],
      average_text,
      paste0(
        ifelse(nrow(high_fem_by_prov), "* ", ""),
        format_count_summary(high_fem_by_prov, "FEM", "between 60 and 100")
      ),
      paste0(
        ifelse(nrow(high_pa_by_prov), "* ", ""),
        format_count_summary(high_pa_by_prov, "PA", "between 60 and 100")
      ),
      wd_text
    ) |>
    make_summary_chunk() |>
    knitr::asis_output()
}

# TODO: cleanup
build_boxplot_summary <- function(
  boxplot_vals,
  type = c("daily", "monthly")[1]
) {
  average_text <- list(daily = "24-hour", monthly = "1-month")[[type]]
  template <- '
%s (a %s monitor located %s km from %s, %s) had the highest observed hourly mean PM<sub>2.5</sub>
concentration in Canada from the FEM network for this report (%s {{< pm_units >}}).
%s (a %s monitor located %s km from %s, %s) had the highest observed %s mean PM<sub>2.5</sub>
concentration in Canada from the FEM network (%s {{< pm_units >}}).

%s (a %s monitor located %s km from %s, %s) had the highest observed hourly mean PM<sub>2.5</sub>
concentration in Canada from the PA network for this report (%s {{< pm_units >}}).
%s (a %s monitor located %s km from %s, %s) had the highest observed %s mean PM<sub>2.5</sub>
concentration in Canada from the PA network (%s {{< pm_units >}}).'

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
        dplyr::summarise(text = join_list_sentence(y)) |>
        dplyr::pull(text)
    })

  template <- "%s had a significant amount of locations with elevated
PM<sub>2.5</sub> concentrations  (> 30 {{< pm_units >}})
for at least 3 %s [see median plot]. 

%s had at least one location with elevated 
PM<sub>2.5</sub> concentrations (> 30 {{< pm_units >}})
for at least 3 %s [see maximum plot]."

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
    text <- "- %s: %s" |>
      sprintf(
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
(> 30 {{< pm_units >}}) for at least 3 %s:

%s"

  template |>
    sprintf(average_text, text) |>
    make_summary_chunk() |>
    knitr::asis_output()
}
