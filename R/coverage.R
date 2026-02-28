pop_coverage_barchart <- function(pd, pop_type = "total", leg_rows = 1) {
  prov_order <- list(
    West = c(
      bc = "British Columbia",
      ab = "Alberta",
      sk = "Saskatchewan",
      mb = "Manitoba"
    ),
    Central = c(on = "Ontario", qc = "Québec"),
    East = c(
      ns = "Nova Scotia",
      nb = "New Brunswick",
      nl = "Newfoundland and Labrador",
      pe = "Prince Edward Island"
    ),
    North = c(yk = "Yukon", nt = "Northwest Territories", nu = "Nunavut")
  )

  colours <- c(
    "FEM Only" = "#F0BE19",
    "PA Only" = "#944BE7",
    "FEM and PA" = "#F0BE19",
    "No Monitor" = "#AF1B3F"
  )
  totals <- pd |>
    dplyr::group_by(region, prov_terr) |>
    dplyr::summarise(pop = sum(pop))
  ymax <- 1.05
  if (pop_type %in% c("urban", "rural")) {
    totals$pop <- sapply(totals$pop, \(p) {
      paste0(
        format(round(p / 10000), big.mark = ",", scientific = FALSE),
        "\nx10k"
      )
    })
    ymax <- 1.1
  } else if (pop_type == "total") {
    totals$pop <- sapply(totals$pop, \(p) {
      paste0(format(round(p / 10000), big.mark = ","), "\nx10k")
    })
    ymax <- 1.1
  }

  tmp <- lengths(prov_order) |> c(Canada = 1)
  pat_spacing <- rep(0.05, nlevels(pd$region)) *
    tmp[1] /
    tmp
  pd |>
    # Initiate plot
    ggplot2::ggplot(ggplot2::aes(
      x = prov_terr,
      y = p_pop,
      fill = forcats::fct_rev(network),
      pattern = network == "FEM and PA",
      pattern_spacing = region
    )) +
    # Draw bars, adding stripes to the FEM and PA category
    ggpattern::geom_bar_pattern(
      stat = "identity",
      colour = "black",
      pattern_alpha = 0.8,
      alpha = 0.8,
      pattern_fill = colours[names(colours) == "PA Only"],
      # pattern_angle = 45,
      pattern_density = 0.45,
      # pattern_spacing = 0.025,
      pattern_key_scale_factor = 0.6
    ) +
    # Add total counts to tops of bars
    ggplot2::geom_text(
      data = totals,
      ggplot2::aes(x = prov_terr, label = pop),
      vjust = 0,
      hjust = 0.5,
      family = "Inter",
      y = 1.02,
      inherit.aes = FALSE
    ) +
    # One plot panel per region
    ggplot2::facet_grid(
      ~region,
      scales = "free_x",
      space = "free",
      switch = "x"
    ) +
    # Use percent scales for y, 0-100 on left, 5-95 on right
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(0),
      sec.axis = ggplot2::dup_axis(breaks = seq(0.05, 1, 0.1)),
      labels = scales::percent_format(),
      breaks = seq(0, 1, 0.1),
      limits = c(0, ymax)
    ) +
    # Use specified colours
    ggplot2::scale_fill_manual(values = colours) +
    # Specify pattern drawn for FEM and PA category
    ggpattern::scale_pattern_manual(
      values = c(`TRUE` = "stripe", `FALSE` = "none"),
      guide = "none"
    ) +
    ggpattern::scale_pattern_spacing_manual(
      values = pat_spacing,
      guide = "none"
    ) +
    # Change appearance of plot
    ggpubr::theme_pubr(border = FALSE) +
    ggplot2::theme(
      panel.spacing = ggplot2::unit(0, 'npc'),
      panel.grid.major.y = ggplot2::element_line(
        colour = "grey",
        linewidth = 0.5,
        linetype = "solid"
      ),
      legend.spacing.y = ggplot2::unit(1.0, 'mm'),
      axis.title.y.right = ggplot2::element_blank()
    ) +
    # Add pattern to the legend
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        reverse = TRUE,
        nrow = leg_rows,
        byrow = TRUE,
        override.aes = list(
          pattern = c("none", "stripe", rep("none", length(colours) - 2)),
          linewidth = 2
        )
      )
    ) +
    # Add basic labels
    ggplot2::labs(
      y = ifelse(
        pop_type == "fnic",
        "FN & Inuit Communities",
        paste(stringr::str_to_title(pop_type), "Population (2016 Census)")
      ),
      x = "Province / Territory",
      fill = "Within 25 km of:",
      caption = plot_captions$`FEM and PA`
    )
}
