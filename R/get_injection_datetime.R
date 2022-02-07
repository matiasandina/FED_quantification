#' This function gets the injection times for the experiment
#' it assumes the injection happened in the last reset of each ID in the input data.frame
#' This might lead to errors if the recording was stopped & restarted after the injection
#' 
#'  @param configs: `data.frame` of configs for all animals in the experiment
#'  

get_injection_datetime <- function(configs) {
  configs %>%
    group_by(ID, mac, Treatment) %>%
    summarise(inj = lubridate::as_datetime(last(datetime)))
}