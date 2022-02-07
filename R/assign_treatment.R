#' This function binds and assigns treatment to configuration files read by `read_configs()`
#' @param `config_list` a list of configs read by `purrr::map(files, read_config)`
#' @param `unique_treatment`: boolean, whether to assume only one treatment per animal and fill NAs accordingly (default = FALSE)
#' @param `reference_group`: character, name of the reference group present on the data. It will get used as the reference factor level of the column Treatment.

assign_treatment <- function(config_list,
                             unique_treatment = FALSE, 
                             reference_group = "saline") {
  
  # bind them as df
  configs <- config_list %>% bind_rows()
  
  # Fill treatment values if needed
  if (unique_treatment == TRUE) {
    # the configs might have NA because animals were not assigned 
    configs <- configs %>%
      # we assume each animal was only injected with one thing!
      group_by(ID) %>% 
      mutate(Treatment = factor(unique(Treatment[!is.na(Treatment)]))) %>% 
      ungroup() %>% 
      mutate(
        # this puts saline as reference (aka, first) regardless of the other level(s)
        Treatment = fct_relevel(Treatment, ref=reference_group)
      )
  }
  return(configs)
}