
# Canada PM25 Summaries

<!-- badges: start -->
<!-- badges: end -->

Automated periodic summary reports of hourly airborne fine particulate matter (PM~2.5~) concentrations from Canadian monitoring stations.

Reports are generated using Quarto (see [daily/index.qmd], [monthly/index.qmd], [seasonal/index.qmd]), and saved as .html files (both `index.html` and `historic/[report-date].html`).
Currently only the daily report is functional, work is needed to restore functionality to the monthly/seasonal reports.
Report data comes from the [AQmap](https://aqmap.ca) database, and are available here: https://aqmap.ca/aqmap/reports/.
Currently reports are available from the start of 2026 onwards, however, older reports can be generated upon request.

Monitoring networks included:

- FEM: Federal Equivelant Method regulatory monitors; gold-standard measurement for real-time data like this.
- PA: PurpleAir low-cost monitors; less accurate but higher spatial density of observations.

## Daily Reports

The daily reports are produced every 12 hours (at 00 UTC and 12 UTC) to summarise the past 24-hours of observations (effectivly a "day" and "night" report for every day).

View reports at:

- Current report: https://aqmap.ca/aqmap/reports/daily/
- Historic reports: https://aqmap.ca/aqmap/reports/daily/historic/ (or via the dropdown near the top of the current report)

## Citing Reports

All reports have a [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/) license.

At the bottom of each report there is citation information for attribution. For example:

```
Nilson, Brayden. March 31, 2026. “Daily Canada PM2.5 Summary: 2026 Jan 02 (day).” University of Northern British Columbia, Environment and Climate Change Canada. https://aqmap.ca/aqmap/reports/daily/historic/2026-01-02-day.html.
```
