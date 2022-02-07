#' This function gets all FED files using target dates
#' 
#' 

find_files <- function(target_dates = NULL){
  if(is.null(target_dates)){
    # read config
    usethis::ui_info("No `target_dates`, select config file manually")
    config_path <- choices::choose_files(title="Select config.yaml", method="rstudioapi") 
    config <- yaml::read_yaml(config_path)
    target_dates <- c(config$habituation_dates, config$experimental_dates)
    # convert to date format
    target_dates <- as.POSIXct(target_dates)
  } 
    # fed dates are given in mmddyy
    fed_dates <- format(target_dates, "%m%d%y")
    coll_fed_dates <- paste(fed_dates, collapse = "|")
    
    fed_backup <- choices::choose_directory(
      title="Select FED backup folder",
      method = "rstudioapi"
      )
    # files are stored in year/month subdir
    folders_to_scan <- file.path(fed_backup,
                                 lubridate::year(target_dates),
                                 stringr::str_pad(
                                   lubridate::month(target_dates),
                                 pad=0, width=2)
                                 )
    folder_to_scan <- unique(folders_to_scan)
    all_files <- list.files(folder_to_scan, 
                            pattern = coll_fed_dates,
                            full.names = T)
  
  
  # TODO: ask if you want to copy files by calling copy_files()  
    
  return(all_files)
  
}
