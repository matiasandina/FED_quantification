#' This function takes FED files from folder on SD card and stores it on the backup for dropbox
#' This needs interactive session for user to choose folders 
#' We assume interactive session likely thorugh Rstudio
#' The core of this function is a moving files function
#' It uses the pattern "FED"
#' It uses the date as MMDDYY to create folders on the backup location
library(dplyr)

backup <- function(){
  
  sd_card <- choices::choose_directory(title = "Select sd card directory")
  backup_dest <- choices::choose_directory(title = "Select backup folder")
  
  fed_files <- list.files(path=sd_card,
                          pattern = "FED[0-9]+.+CSV",
                          full.names = T)
  
  dates <- stringr::str_extract(fed_files, "_[0-9]+_")
  dates <- stringr::str_remove_all(dates, "_")
  
  df <- tibble(
    old_files = fed_files,
    # dates are stored as month day year
    dates_parsed = lubridate::mdy(dates),
    year = lubridate::year(dates_parsed),
    mon = lubridate::month(dates_parsed)) %>% 
    # generate paths from existing data
    mutate(mon = stringr::str_pad(mon, width = 2, pad = 0),
           new_path = file.path(backup_dest, year, mon)) %>% 
    # only keep those that actually happened
    distinct() %>% 
    # generate new files
    mutate(new_files = file.path(new_path, basename(old_files)))
  
  # get the dirs to create
  dirs_to_create <- unique(df$new_path) 
  # create them and provide some feedback
  fs::dir_create(dirs_to_create, recurse = T)
  usethis::ui_info("Directories are set up")
  fs::file_move(df$old_files, df$new_files)
  # move files
  usethis::ui_done("{length(df$old_files)} files were moved to\n{backup_dest}")
}
