make_summary_chunk <- function(contents) {
  template <- ':::: card

::::: card-body

<details>
<summary>Click for an automated text summary.</summary>

%s
  
</details>

:::::

::::'
  template |> sprintf(contents)
}

build_tabs <- function(
  tab_names,
  plot_paths,
  plot_captions,
  report_dir = "./"
) {
  chunks <- tab_names |>
    sapply(\(tab_name) {
      card <- plot_paths[[tab_name]] |>
        stringr::str_replace(stringr::fixed(report_dir), "./") |>
        plot_card(plot_captions[[tab_name]])
      "## %s\n%s" |> sprintf(tab_name, card)
    })

  ":::::::: panel-tabset
%s
::::::::" |>
    sprintf(paste(chunks, collapse = "\n")) |>
    knitr::asis_output()
}
