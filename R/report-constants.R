# Run Control ------

#Change local->server when running on AQmap server
run_location <- "server"

run_current_day <- FALSE
run_current_month <- FALSE
run_current_season <- FALSE

cache <- run_location == "local"

# File Locations -----

base_loc <- list(
  local = 'https://aqmap.ca/aqmap/',
  server = '../'
)[[run_location]]

aqcsv_dir <- paste0(base_loc, 'data/AQCSV/')
aqcsv_folders <- c(raw = "unvalidated", fem = "fem")

# Output Locations ----

report_dir = "./" # appended to with report type for each report (i.e ./daily)
report_dir_esc = "\\./" # appended to with report type for each report (i.e ./daily)
figure_dir <- "plots" # relative to report_dir for each type
data_dir <- "data" # relative to report_dir for each type
lib_dir <- "libs" # relative to report_dir for each type

# Font(s) ----------

## Uncomment for INIT run only
# if(cache){
#   extrafont::font_import(pattern = "Inter", prompt = FALSE)
# }else{
#   extrafont::font_import("~/.fonts", pattern = "Inter", prompt = FALSE)
# }

extrafont::loadfonts(device = "all")

# File meta ------

meta_cols <- c(
  "site_id",
  "name",
  "lat",
  "lng",
  "prov_terr",
  "is_aqsu",
  "fcst_zone",
  "nearest_community",
  "nc_dist_km",
  "nc_lat",
  "nc_lng"
)

# AQCSV Header
header <- c(
  "site",
  "data_status",
  "action_code",
  "datetime",
  "parameter",
  "duration",
  "frequency",
  "value",
  "unit",
  "qc",
  "poc",
  "lat",
  "lon",
  "GISDatum",
  "elev",
  "method_code",
  "mpc",
  "mpc_value",
  "uncertainty",
  "qualifiers",
  "A",
  "B",
  "FEM",
  "FEMdistance",
  "FEMsite",
  "T",
  "RH",
  "P"
)

monitor_groups <- c(
  FEM = "FEM Only",
  PA = "PA Only",
  "FEM and PA" = "FEM and PA"
)
monitor_groups_cleaned <- monitor_groups |>
  stringr::str_to_lower() |>
  stringr::str_replace_all(" ", "_")

municipal_classes <- c(
  "District Municipality",
  "Community",
  "Settlement",
  "Hamlet",
  "Village",
  "Urban Community",
  "City",
  "Village Municipality",
  "Town",
  "Compact Rural Community",
  "Municipality",
  "Northern Community", #'Post Office',#"Borough",
  "Northern Village",
  "Locality",
  "Community Government",
  "First Nation Village",
  "Northern Village Municipality",
  "Resort Village",
  "Organized Hamlet",
  "Township Municipality",
  "Recreational Community",
  "Rural Village",
  "Summer Village",
  "Northern Settlement",
  "Rural Community",
  "Metis Settlement",
  "Cree Village",
  "Cree Village Municipality",
  "Northern Hamlet",
  "Metropolitan Area",
  "Charter Community",
  "Fort",
  "Amerindian Settlement",
  "Naskapi Village Municipality",
  "Naskapi Village",
  "Forest Village",
  "Townsite",
  "Resort Municipality",
  "Mountain Resourt Municipality"
)

# Misc -----

loading_div <- '
<div id="loading" style="text-align:center;display: flex; align-items: center; justify-content: center; height: 250px;">
  <div>
    <p>Loading, please wait...</p>
    <p><img id="loading-image" src="index_files/icons/loading.gif" alt="Loading..." /></p>
  </div>
</div>
' |>
  htmltools::HTML()

report_details <- '
<details>
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

</details>
' |>
  htmltools::HTML()

contact_email <- "brayden.nilson@ec.gc.ca"
contact_details <- "For any concerns, questions, or feedback regarding this report or the data within it, please contact %s" |> 
  sprintf(contact_email)

# Define legend breaks and colours from AQHI+ scale
leg_ugm3 <- data.frame(
  breaks = 0:10 * 10,
  colours = c(
    "#21C6F5",
    '#189ACA',
    "#0D6797", #Low [1 - 3]
    "#FFFD37",
    '#FFCC2E',
    "#FE9A3F", # Moderate [4 - 6]
    "#FD6769",
    "#FF3B3B",
    "#FF0101",
    "#CB0713", # High [7 - 10]
    "#650205"
  )
)

###!Don't need all these different prov vars....
prov_order <- list(
  West = c("BC", "AB", "SK"),
  Central = c("MB", "ON", "QC"),
  East = c("NS", "NB", "NL", "PE"),
  Territories = c("YK", "NT", "NU")
)

provinces_n_territories <- canadata::provinces_and_territories$abbreviation |>
  as.character() |>
  dplyr::replace_values("Quebec" ~ "Québec") |> # TODO: remove once database in cync with canadata
  setNames(canadata::provinces_and_territories$name_en) |>
  as.list()

months_in_seasons = list(
  "Summer" = 5:10,
  "Winter" = c(11:12, 1:4)
)

# Forecast Zones ---

fcst_zones <- canadata::forecast_zones |>
  dplyr::select(
    prov_terrs,
    fcst_zone = name_en,
    fcst_zone_fr = name_fr,
    geometry
  )

seperate_fcst_zone_provs <- function(fcst_zones) {
  fcst_zones |>
    dplyr::rename(prov_terr = prov_terrs) |>
    tidyr::separate_longer_delim(cols = prov_terr, delim = ",") |>
    dplyr::mutate(
      prov_terr = prov_terr |>
        factor(levels(canadata::provinces_and_territories$abbreviation))
    )
}

fcst_zones_clean <- fcst_zones |>
  handyr::sf_as_df() |>
  seperate_fcst_zone_provs()

prov_terr_zone_counts <- fcst_zones_clean |>
  dplyr::summarise(n_zones = dplyr::n(), .by = prov_terr)
