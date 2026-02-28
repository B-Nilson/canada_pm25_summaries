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

monitor_groups <- c(
  "FEM and PA" = "FEM and PA",
  FEM = "FEM Only",
  PA = "PA Only"
)
monitor_groups_cleaned <- monitor_groups |>
  stringr::str_to_lower() |>
  stringr::str_replace_all(" ", "_")

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

provinces_n_territories <- canadata::provinces_and_territories$abbreviation |>
  as.character() |>
  dplyr::replace_values("Quebec" ~ "Québec") |> # TODO: remove once database in cync with canadata
  setNames(canadata::provinces_and_territories$name_en) |>
  as.list()

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
