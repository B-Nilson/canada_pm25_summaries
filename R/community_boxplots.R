make_community_boxplots <- function(pd, m = "FEM and PA") {
  box <- boxplot(n_hours_above_100 ~ prov_terr, pd, plot = FALSE)
  pd <- pd |>
    dplyr::mutate(
      is_outlier = n_hours_above_100 > box$stats[4, as.numeric(prov_terr)]
    )
  outliers <- subset(pd, is_outlier)

  pd |>
    plotly::plot_ly(y = ~n_hours_above_100, x = ~prov_terr) |>
    plotly::add_boxplot(
      name = "",
      boxpoints = FALSE,
      color = I('black'),
      alpha = 0.2
    ) |>
    plotly::layout(
      title = paste(
        "Number of Community-Level, Very High Exceedances by Province/Territory for",
        format(max_date, "%B %Y")
      ),
      showlegend = FALSE,
      xaxis = list(
        title = "Province | Territory",
        zeroline = TRUE,
        showgrid = FALSE
      ),
      yaxis = list(
        title = paste0(
          "# Hours Community Exceeds PM2.5 of 100 ug/m3 (",
          m,
          ")"
        ),
        zeroline = TRUE,
        showgrid = FALSE
      )
    ) |>
    plotly::add_markers(
      data = outliers,
      text = ~ paste0(
        "<b>",
        nearest_community,
        "</b>",
        # "<br>",community_type,
        "<br># of observation sites: ",
        n_sites,
        "<br>1-month mean: ",
        round(pm25_mean, 1),
        " ug/m3",
        "<br>1-month max: ",
        round(pm25_max, 1),
        " ug/m3",
        "<br># hours >60 ug/m3: ",
        n_hours_above_60,
        " / ",
        length(hours_to_summarise),
        "<br># hours >100 ug/m3: ",
        n_hours_above_100,
        " / ",
        length(hours_to_summarise),
        "<br>Forecast Zone: ",
        fcst_zone
      ),
      hoverinfo = 'text',
      marker = list(
        color = "rgb(107,174,214)",
        line = list(color = "black", width = 1)
      )
    )
}
