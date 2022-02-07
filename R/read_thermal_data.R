#' This function reads
#' @param path path where to the folder with all raspberry pi (e.g., "/home/choilab/raspberry_IP/") 
#' @param target_date the date "yyyy-mm-dd" of the recording that is going to be loaded
#' @return df `data.frame` bind of all temperatures saved by python function as temperatures.csv  
read_thermal_data <- function(path, target_date){
  all_units <- list.dirs(path = path, recursive = FALSE)
  # mac address has ":" unlikely in anothed dir
  all_units <- all_units[str_detect(all_units, ":")]
  all_dirs <- lapply(all_units, function(tt) list.dirs(path = tt, recursive = FALSE)) %>%
    unlist()
  # there shouldn't be children but
  # we paste0 to only get the date folder and not the children of that folder
  relevant_dirs <- all_dirs[str_detect(all_dirs, paste0(target_date, "$"))]
  all_files <- lapply(relevant_dirs, function(tt) list.files(tt, pattern = "temperatures.csv", full.names = TRUE)) %>%
    unlist()
  
  types <- cols(
    min = col_double(),
    max = col_double(),
    mean = col_double(),
    median = col_double(),
    std = col_double(),
    px = col_double(),
    file = col_character(),
    x = col_double(),
    y = col_double(),
    temp_center = col_double()
  )
  
  df <- lapply(all_files, function(tt) read_csv(tt, col_types = types)) %>%
    bind_rows() %>%
    mutate(mac = str_extract(file, "/[a-z|0-9]{2}:[a-z|0-9]{2}:[a-z|0-9]{2}:[a-z|0-9]{2}:[a-z|0-9]{2}:[a-z|0-9]{2}/"),
           mac = str_remove_all(mac, "/"),
           base = basename(file),
           timepoint = str_remove(base, "_capture.jpg"))
  
  
  df <- df %>% 
    mutate(timepoint = lubridate::as_datetime(timepoint),
           date = lubridate::date(timepoint),
           # create a common time by rounding
           common_time = lubridate::floor_date(timepoint, unit="30 sec"))
  return(df)
}
