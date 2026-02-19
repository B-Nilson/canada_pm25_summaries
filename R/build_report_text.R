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
