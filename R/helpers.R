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
  iframe_height = 600
) {
  chunks <- tab_names |>
    sapply(\(tab_name) {
      card <- plot_paths[[tab_name]] |>
        stringr::str_replace(stringr::fixed(report_dir), "./") |>
        plot_card(
          plot_captions[[tab_name]],
          iframe = iframe,
          iframe_height = iframe_height[1]
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
                iframe_height = iframe_height[2]
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
