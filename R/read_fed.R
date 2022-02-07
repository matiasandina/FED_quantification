# This function reads CSV files as saved by FED3 Library
# It will do basic parsing of column classes and return a data.frame

read_fed <- function(filename){
  
  # col classes
  FED3_classes <-  cols(
    `MM:DD:YYYY hh:mm:ss` = col_character(),
    Device_Number = col_double(),
    Battery_Voltage = col_double(),
    Motor_Turns = col_double(),
    #FR_Ratio = col_character(),
    Active_Poke = col_character(),
    Left_Poke_Count = col_double(),
    Right_Poke_Count = col_double(),
    Pellet_Count = col_double(),
    Retrieval_Time = col_double()
  )
  
  X <- read_csv(filename, col_types = FED3_classes) %>% 
    mutate(datetime = lubridate::parse_date_time(`MM:DD:YYYY hh:mm:ss`, "mdy HMS"),
           session = stringr::str_extract(filename, pattern="_[0-9]+\\.CSV"),
           session = stringr::str_extract(session, pattern="[0-9]+"))
  
  
  # Add year, day month columns
  X <- X %>%
    mutate(day = lubridate::day(datetime),
           month = lubridate::month(datetime),
           year = lubridate::year(datetime))
  
  ## Add FR_Ratio (make compatible with FED2)
  if("FR_Ratio" %in% names(X) == FALSE){
    X <- X %>% mutate(FR_Ratio = 1)
  }

  return(X)
}
