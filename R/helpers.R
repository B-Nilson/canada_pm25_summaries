get_season <- function(date = Sys.time(), months_in_seasons) {
  season <- names(months_in_seasons)[
    sapply(months_in_seasons, \(months) lubridate::month(date) %in% months)
  ]
  year <- lubridate::year(date) -
    ifelse(
      season == "Winter" & lubridate::month(date) < 6,
      1,
      0
    )
  paste0(year, "-", season)
}

max <- handyr::max # TODO: make into safe_max? adjust all base::max refs
min <- handyr::min

safe_mean <- function(x, digits = 1) {
  mean(x, na.rm = TRUE) |>
    suppressWarnings() |>
    dplyr::replace_values(NaN ~ NA_real_) |>
    round(digits = digits)
}

make_aqmap_link <- function(lat, lng, zoom = 12, lang = "EN") {
  paste0(
    "https://aqmap.ca/aqmap/",
    tolower(lang),
    "/#",
    zoom,
    "/",
    lat,
    "/",
    lng
  )
}

# TODO: replace with handyr::sentence_range()
shorten_number_list <- function(x) {
  if (length(x) == 0) {
    return("")
  }
  paste(
    tapply(x, cumsum(c(1, diff(x) != 1)), \(i) {
      ifelse(
        length(i) > 2,
        paste0(head(i, 1), '-', tail(i, 1)),
        paste(i, collapse = ', ')
      )
    }),
    collapse = ', '
  )
}

join_list_sentence <- function(l, oxford = FALSE, type = "regions") {
  if (length(l) == 0) {
    return(paste("No", type))
  }
  if (length(l) == 1) {
    return(l)
  }
  out <- paste(l[-length(l)], collapse = ", ")
  out <- paste0(out, ifelse(oxford, ", and ", " and "), dplyr::last(l))
  return(out)
}

plot_card <- function(
  plot_src,
  text,
  iframe = FALSE,
  title = NA,
  iframe_height = 615,
  is_table = FALSE,
  plot_timestamp
) {
  img <- ifelse(
    iframe,
    paste0(
      "<iframe loading='lazy' style='height:",
      iframe_height,
      "px' src='$plot_src' class='card-img-top p-2' data-external='1' frameborder='0' scrolling='no' width='100%'></iframe>"
    ),
    "<div><img loading='lazy' src='$plot_src' class='card-img-top p-2'/></div>"
  ) |>
    stringr::str_replace("\\$plot_src", plot_src)
  if (!is.na(title)) {
    img <- paste0(
      "<center style='padding-top:0.5rem;'><big>",
      title,
      "</big></center>",
      img
    )
  }
  "
:::::: {.plot-card #$TYPE-$ID}
  
$IMG
  
$text

::::::
" |>
    stringr::str_replace("\\$TYPE", ifelse(is_table, "tbl", "fig")) |>
    stringr::str_replace("\\$IMG", img) |>
    stringr::str_replace("\\$text", text |> stringr::str_replace("'", "\\'")) |>
    stringr::str_replace(
      "\\$ID",
      gsub(
        "[^a-zA-Z0-9_\\-\\.]",
        "",
        basename(plot_src) |>
          tools::file_path_sans_ext() |>
          stringr::str_remove(stringr::fixed(paste0("_", plot_timestamp)))
      )
    )
}

make_summary_chunk <- function(contents) {
  template <- ':::: summary-card

%s

::::'
  template |> sprintf(contents)
}

build_tabs <- function(
  tab_names,
  plot_paths,
  plot_captions,
  report_dir = "./",
  table_buttons = NULL,
  tables = NULL,
  table_captions = NULL,
  iframe = FALSE,
  iframe_height = 600,
  plot_timestamp
) {
  chunks <- tab_names |>
    sapply(\(tab_name) {
      card <- plot_paths[[tab_name]] |>
        stringr::str_replace(stringr::fixed(report_dir), "./") |>
        plot_card(
          plot_captions[[tab_name]],
          iframe = iframe,
          iframe_height = iframe_height[1],
          plot_timestamp = plot_timestamp
        )
      "## %s\n%s" |> sprintf(tab_name, card)
    })

  if (!is.null(tables)) {
    chunks <- tab_names |>
      lapply(\(tab_name) {
        "%s\n%s" |>
          sprintf(
            chunks[[names(tab_names[tab_names == tab_name])]],
            tables[[tab_name]] |>
              stringr::str_replace(stringr::fixed(report_dir), "./") |>
              plot_card(
                table_captions[[tab_name]],
                iframe = TRUE,
                is_table = TRUE,
                iframe_height = iframe_height[2],
                plot_timestamp = plot_timestamp
              ) |>
              stringr::str_replace(
                "</iframe>",
                paste0("</iframe>\n", table_buttons[[tab_name]])
              )
          )
      })
  }

  ":::::::: panel-tabset
%s
::::::::" |>
    sprintf(paste(chunks, collapse = "\n")) |>
    knitr::asis_output()
}

dl_button_html <- function(outdir, file) {
  paste0(
    '<button class="btn btn-default" sep="," has_icon="TRUE" type="submit" onclick="window.open(\'',
    outdir,
    file,
    '\')"><i class="fa fa-save"></i> Download data as csv</button>'
  ) |>
    htmltools::HTML()
}

make_download_button <- function(data_for_download, data_dir, file_path) {
  sanitized_header <- data_for_download |>
    names() |>
    stringr::str_replace_all("\n", " ")
  data_for_download |>
    setNames(sanitized_header) |>
    data.table::fwrite(file_path, dateTimeAs = "write.csv")
  dl_button_html(outdir = data_dir |> paste0("/"), file = basename(file_path))
}

abbrev_text <- function(x) {
  x_safe <- x |>
    stringr::str_replace_all("'", "&rsquo;") |>
    stringr::str_replace_all("\"", "&quot;")

  '<div style="display:table; table-layout:fixed; width:100%;">' |>
    paste0(
      '<p title="%s" style="overflow-x:hidden; text-overflow:ellipsis; white-space:nowrap">%s</p></div>' |>
        sprintf(x_safe, x)
    )
}

escape_md <- function(x) {
  # Order matters: escape backslash first
  x <- gsub("\\\\", "\\\\\\\\", x)

  # Escape Markdown special characters
  specials <- c(
    "`",
    "\\*",
    "_",
    "\\{",
    "\\}",
    "\\[",
    "\\]",
    "\\(",
    "\\)",
    "#",
    "\\+",
    "-",
    "\\.",
    "!",
    "\\|"
  )

  for (s in specials) {
    x <- gsub(s, paste0("\\\\", s), x)
  }

  x
}

# Convert strptime pattern to regex, escaping special (regex) characters as needed
# i.e. "%Y%m%d%H%M%S" -> "\\d{4}\\d{2}\\d{2}\\d{2}\\d{2}\\d{2}"
strptime_to_regex <- function(strptime_pattern) {
  handled <- c("%Y", "%m", "%d", "%H", "%M", "%S", "%F", "%T")
  components <- strptime_pattern |>
    stringr::str_extract_all("%\\w") |>
    unlist()
  is_handled <- components %in% handled
  if (!all(is_handled)) {
    stop(
      "pattern contains unhandled components:",
      paste(components[!is_handled], collapse = ", ")
    )
  }

  strptime_pattern |>
    stringr::str_escape() |>
    stringr::str_replace_all("%F", "%Y-%m-%d") |>
    stringr::str_replace_all("%T", "%H:%M:%S") |>
    stringr::str_replace_all("%Y", "\\\\d{4}") |>
    stringr::str_replace_all("%m|%d|%H|%M|%S", "\\\\d{2}")
}

make_hourly_seq <- function(date_range) {
  date_range |>
    handyr::as_interval() |>
    seq(by = "1 hours")
}

make_plot_captions <- function(
  date_range,
  date_format = "%b %d, %Y %H:%M UTC",
  networks = c("FEM", "PA")
) {
  date_range <- sort(date_range) |> format(date_format)
  caption_range <- paste(date_range, collapse = " to ")

  "Obs. from Canadian %s monitors from %s" |>
    sprintf(networks, caption_range) |>
    setNames(networks) |>
    as.list()
}
