# This function will make light shading pattern for ggplot plots
#' @param params The params from `config.yaml` that will tell what the target dates are and what the light schedule is.
#' @param df A `data.frame` containing a `datetime` column that will provide the ranges of the experimental data

make_lights <- function(params, df) {
  # get the range of the experiment
  # this might fail if no FED has been tried before, shouldn't be a problem if we test all feds can deliver a pellet before putting them in the cage
  experiment_range <- range(df$datetime)
  
  # Regardless of whether there is habituation dates or not, we will use all data present on the FEDs
  #if (purrr::is_empty(params$habituation_dates)) {
  #  # check whether there was habituation days
  #  # habituation days will have data for fed but not be on the target dates
  #  hab <- experiment_range %>% min() %>% lubridate::date() < as.Date(min(params$experimental_dates))
  #  if (hab) {
  #    params$fed_dates <- seq(date(experiment_range[1]), date(experiment_range[2]), "1 day")
  #  }
  #} else {
  #  params$fed_dates <- seq(date(experiment_range[1]), date(experiment_range[2]), "1 day")
  #}
  
  params$fed_dates <- seq(lubridate::date(experiment_range[1]),
                          lubridate::date(experiment_range[2]),
                          "1 day")
  
  #make the dates
  # this vector will have on, off, on, off..
  # the first value might be before the experiment starts, the last value after experiment ends
  light_changes <-
    lapply(params$fed_dates, function(tt)
      paste(tt, params$lights)) %>%
    unlist()
  #make it datetime
  light_changes <- lubridate::as_datetime(light_changes)
  # subset
  light_changes <-
    light_changes[between(light_changes, experiment_range[1], experiment_range[2])]
  
  light_diff <- diff(hms::as_hms(params$lights))
  
  # we need even number light changes to make rectangles
  # light changes could be odd when starting and finishing animals at different light cycles
  if (length(light_changes) %% 2 == 1) {
    # It might happen that the first light change is lights-on because
    # the animals started during on lights off
    # check for that
    # if the first time is lights on, add the previous lights off
    if (hms::as_hms(params$lights[1]) == hms::as_hms(light_changes[1])) {
      # calculate the beginning of the lights-off
      # first lights off will be the previous day
      first_lights_off <- dplyr::first(light_changes) - light_diff
      # it was done like this previously "light_changes[1] - lubridate::hours(12)"
      # but it might not be a 12 hs cycle
      light_changes <-
        purrr::prepend(light_changes, first_lights_off)
      print(glue::glue("Adding {first_lights_off}"))
      # the xmin should be the even one in this case
      full_exp_shade <-
        annotate(
          "rect",
          xmin = light_changes[seq_along(light_changes) %% 2 == 0],
          xmax = light_changes[seq_along(light_changes) %% 2 > 0],
          ymin = 0,
          ymax = Inf,
          fill = "gray80",
          alpha = 0.5
        )
    } else {
      # if the first value is lights-off, add the last lights-on
      last_lights_on <- dplyr::last(light_changes) + light_diff
      light_changes <-
        append(light_changes, values = last_lights_on)
      full_exp_shade <-
        annotate(
          "rect",
          xmin = light_changes[seq_along(light_changes) %% 2 > 0],
          xmax = light_changes[seq_along(light_changes) %% 2 == 0],
          ymin = 0,
          ymax = Inf,
          fill = "gray80",
          alpha = 0.5
        )
    }
  } else {
    full_exp_shade <-
      annotate(
        "rect",
        # the xmin should be the odd one in this case
        xmin = light_changes[seq_along(light_changes) %% 2 > 0],
        xmax = light_changes[seq_along(light_changes) %% 2 == 0],
        ymin = 0,
        ymax = Inf,
        fill = "gray80",
        alpha = 0.5
      )
  }
  
  # light_changes[seq_along(light_changes) %% 2 > 0]
  # light_changes[seq_along(light_changes) %% 2 == 0]
  #full_exp_shade <-
  #  annotate("rect",
  #           xmin=light_changes[seq_along(light_changes) %% 2 > 0],
  #           xmax=light_changes[seq_along(light_changes) %% 2 == 0],
  #           ymin=0, ymax = Inf,
  #           fill="gray80", alpha=0.5)
  
  # get the lights as a ITime
  #lights <- data.table::as.ITime(params$lights)
  
  return(full_exp_shade)
}
