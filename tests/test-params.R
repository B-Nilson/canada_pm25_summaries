# able to set daily params
{
  setwd("daily")

  # Read yaml header and parse params
  daily_lines <- "index.qmd" |> readLines(n = 100)
  yaml_start_end <- which(daily_lines == "---")
  daily_yaml <- daily_lines[(yaml_start_end[1] + 1):(yaml_start_end[2] - 1)]
  params <- yaml::read_yaml(text = daily_yaml, eval.expr = TRUE)$params

  # Handle "value" key for multi-key params
  for (param_name in names(params)) {
    param <- params[[param_name]]
    if ("value" %in% names(param)) {
      params[[param_name]] <- param$value
    }
  }
  
  setwd("../")
}
