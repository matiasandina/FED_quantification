#' This function reads configuration files in a particular path
#' Because the experiments are done on a particular date, it gets the particular date as input
#' @param `target_date`: "yyyy-mm-dd" character vector with the days  we are targeting
#' @param `path`: full path to the directory containing the experiment configs

read_config_files <- function(target_date,
                              path="/home/choilab/raspberry_IP/"){
  dirs <- list.dirs(path = path, recursive=FALSE)
  pattern_to_find <- paste0(target_date,".*_config.csv")
  files_inside <- lapply(dirs,
                         function(tt)
                           list.files(tt, 
                                      pattern = pattern_to_find,
                                      full.names=TRUE)) %>%
    unlist()
  
  if (length(files_inside) < 0) {
    usethis::ui_stop("No files found with `target_date` of {target_date}")
  }
  
  
  # classes of the columns on the config
  col_classes <- cols(
    mac = col_character(),
    date = col_character(),
    ID = col_character(),
    Treatment = col_character(),
    Dose = col_character(),
    Comment = col_character()
  )
  # Actually read them
  configs <- purrr::map(files_inside,
                        function(x) read_csv(x, col_types = col_classes)) %>% 
    bind_rows() 

  # Rename `date`` to `datetime` 
  if (nrow(configs) > 0) {
    configs <- configs %>% rename(datetime = date)
  }
  return(configs)
}


