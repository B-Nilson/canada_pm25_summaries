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

report_dir = "./"
report_dir_esc = "\\./"
figure_dir <- "plots" # relative to report_dir

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

prov_pretty <- list(
  BC = "British Columbia",
  AB = "Alberta",
  SK = "Saskatchewan",
  MB = "Manitoba",
  ON = "Ontario",
  QC = "Québec",
  NS = "Nova Scotia",
  NB = "New Brunswick",
  NL = "Newfoundland and Labrador",
  PE = "Prince Edward Island",
  YK = "Yukon",
  NT = "Northwest Territories",
  NU = "Nunavut"
)

months_in_seasons = list(
  "Summer" = 5:10,
  "Winter" = c(11:12, 1:4)
)

# Forecast Zones ---

fcst_zones <- canadata::forecast_zones |>
  dplyr::select(prov_terrs, fcst_zone = name_en, fcst_zone_fr = name_fr)

fcst_zones_clean <- fcst_zones |>
  handyr::sf_as_df() |>
  dplyr::select(prov_terr = prov_terrs, fcst_zone) |>
  tidyr::separate_longer_delim(cols = prov_terr, delim = ",") |>
  dplyr::mutate(
    prov_terr = prov_terr |>
      factor(levels(canadata::provinces_and_territories$abbreviation))
  )

prov_terr_zone_counts <- fcst_zones_clean |>
  dplyr::summarise(n_zones = dplyr::n(), .by = prov_terr)
