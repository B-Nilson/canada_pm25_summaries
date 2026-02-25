get_pm_events <- function(
  obs,
  groups = c("site_id"),
  clean_air_events = FALSE
) {
  out <- obs |>
    dplyr::mutate(
      aqhip = aqhi_p(pm25, use_risk = TRUE),
      date2 = date,
      date = lubridate::floor_date(date, "days")
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groups, "date")))) |>
    dplyr::mutate(
      mean_pm25 = mean(pm25, na.rm = TRUE),
      max_pm25 = max(pm25, na.rm = TRUE),
      min_pm25 = min(pm25, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groups, "date", "aqhip")))) |>
    dplyr::summarise(
      n = length(unique(date2)),
      n_pa = sum(!duplicated(site_id[monitor == "PA"])),
      n_fem = sum(!duplicated(site_id[monitor == "FEM"])),
      mean_pm25 = mean(mean_pm25, na.rm = TRUE),
      min_pm25 = min(min_pm25, na.rm = TRUE),
      max_pm25 = max(max_pm25, na.rm = TRUE)
    ) |>
    dplyr::mutate(n = ifelse(is.na(aqhip), 0, n), p = n / 24) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groups, "date"))))

  if (clean_air_events) {
    out <- out |>
      dplyr::filter(aqhip == "Low [1-3]" & n == 24)
  } else {
    out <- out |>
      dplyr::filter(aqhip != "Low [1-3]")
  }
  out |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(groups, "date")))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groups)))) |>
    dplyr::filter(!duplicated(date)) |>
    dplyr::mutate(
      not_seq = difftime(date, lag(date, 1), units = "days") != 1,
      not_seq = ifelse(is.na(not_seq), FALSE, not_seq),
      event_id = cumsum(not_seq) + 1
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groups, "event_id")))) |>
    dplyr::summarise(
      min_date = min(date),
      max_date = max(date),
      duration = difftime(max_date, min_date, units = "days") + 1,
      n_pa = max(n_pa, na.rm = TRUE),
      n_fem = max(n_fem, na.rm = TRUE),
      max_pm25 = max(max_pm25, na.rm = TRUE) |> round(1),
      min_pm25 = min(min_pm25, na.rm = TRUE) |> round(1),
      mean_pm25 = mean(mean_pm25, na.rm = TRUE) |> round(1),
      .groups = "drop"
    ) |>
    # dplyr::filter(max_pm25 >= 35) |>
    dplyr::mutate(
      event_magnitude = mean_pm25 * as.numeric(duration) |> round(1)
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
    dplyr::mutate(event_rank = rank(-event_magnitude))
}
