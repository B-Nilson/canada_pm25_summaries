connect_to_db <- function(local = TRUE, ...) {
  host <- ifelse(local, "localhost", "https://aqmap.ca/")
  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = "aqmap",
    host = host,
    port = Sys.getenv('aqmap_db_port'),
    user = Sys.getenv('aqmap_db_user'),
    password = Sys.getenv('aqmap_db_pass'),
    ...
  )
}

load_report_data <- function(
  date_range,
  db = NULL,
  fem_tables = list(obs = "fem_obs", meta = "fem_meta"),
  lcm_tables = list(obs = "pa_obs", meta = "pa_meta"),
  meta_cols
) {
  if (is.null(db)) {
    db <- connect_to_db()
    on.exit({
      DBI::dbDisconnect(db)
    })
  }
  fem_report_data_query <- date_range |>
    load_fem_report_data(
      db = db,
      obs_table = fem_tables$obs,
      meta_table = fem_tables$meta,
      meta_cols = meta_cols
    )

  lcm_report_data_query <- date_range |>
    load_lcm_report_data(
      db = db,
      obs_tables = lcm_tables$obs,
      meta_tables = lcm_tables$meta,
      meta_cols = meta_cols
    )

  pt_order <- levels(canadata::provinces_and_territories$name_en) |> 
    dplyr::replace_values("Quebec" ~ "Québec") # TODO: remove once col in database matches canadata
  fem_report_data_query |>
    dplyr::union_all(lcm_report_data_query) |>
    dplyr::collect() |>
    dplyr::mutate(prov_terr = prov_terr |> factor(levels = pt_order)) |>
    # Infill Date Gaps
    dplyr::group_by(dplyr::pick(dplyr::all_of(c("monitor", meta_cols)))) |>
    tidyr::complete(date = make_hourly_seq(date_range))
}

load_fem_report_data <- function(
  date_range,
  db = connect_to_db(),
  obs_table = "fem_obs",
  meta_table = "fem_meta",
  meta_cols
) {
  min_date <- date_range[1]
  max_date <- date_range[2]
  obs_cols <- c("site_id", "date", "pm25")
  obs_query <- db |>
    dplyr::tbl(obs_table) |>
    dplyr::select(dplyr::all_of(obs_cols)) |>
    dplyr::filter(date |> dplyr::between(min_date, max_date)) |>
    dplyr::mutate(monitor = "FEM")

  meta_query <- db |>
    dplyr::tbl(meta_table) |>
    dplyr::filter(prov_terr != "United States") |>
    dplyr::select(dplyr::any_of(meta_cols))

  obs_query |>
    dplyr::left_join(meta_query, by = "site_id") |>
    dplyr::filter(!is.na(lat)) # handle obs sites without meta
}

load_lcm_report_data <- function(
  date_range,
  db = connect_to_db(),
  obs_tables = "pa_obs", # TODO: ensure vectorised
  meta_tables = "pa_meta",
  meta_cols
) {
  min_date <- date_range[1]
  max_date <- date_range[2]
  obs_cols <- c(
    "site_id",
    "date",
    pm25 = "pm2.5_validated_corrected",
    pm25_a = "pm2.5_a",
    pm25_b = "pm2.5_b",
    "temperature",
    "rh",
    "qaqc_flag_pm2.5"
  )
  sql_round_to_hourly <- "TO_TIMESTAMP(CEIL(EXTRACT(EPOCH FROM date) / 3600) * 3600)"
  obs_query <- obs_tables |>
    handyr::for_each(
      .show_progress = FALSE,
      .as_list = TRUE,
      \(obs_table) {
        monitor_name <- obs_table |> stringr::str_remove("_obs") |> toupper()
        db |>
          dplyr::tbl(obs_table) |>
          dplyr::select(dplyr::any_of(obs_cols)) |>
          dplyr::filter(date |> dplyr::between(min_date, max_date)) |>
          dplyr::mutate(monitor = monitor_name, site_id = as.character(site_id))
      }
    )
  if (length(obs_tables) > 1) {
    obs_query <- obs_query |> do.call(what = dplyr::union_all)
  } else {
    obs_query <- obs_query[[1]]
  }
  obs_query <- obs_query |>
    dplyr::group_by(
      monitor,
      site_id,
      date = dplyr::sql(!!sql_round_to_hourly)
    ) |>
    dplyr::summarise(
      dplyr::across(dplyr::everything(), \(x) mean(x, na.rm = TRUE)),
      .groups = "drop"
    )

  meta_query <- meta_tables |>
    handyr::for_each(
      .show_progress = FALSE,
      .as_list = TRUE,
      \(meta_table) {
        monitor_name <- meta_table |> stringr::str_remove("_meta") |> toupper()
        db |>
          dplyr::tbl(meta_table) |>
          dplyr::select(dplyr::all_of(meta_cols)) |>
          dplyr::mutate(
            monitor = monitor_name,
            site_id = as.character(site_id)
          ) |>
          dplyr::filter(prov_terr != "United States")
      }
    )
  if (length(meta_tables) > 1) {
    meta_query <- meta_query |> do.call(what = dplyr::union_all)
  } else {
    meta_query <- meta_query[[1]]
  }

  obs_query |>
    dplyr::left_join(meta_query, by = c("monitor", "site_id")) |>
    dplyr::filter(!is.na(lat)) # handle obs sites without meta
}
