library(tidyverse)
file_list <- list.files(path = "data/",
                        pattern = ".csv|.CSV",
                        full.names = TRUE)

FED3_classes <-  cols(
    `MM:DD:YYYY hh:mm:ss` = col_character(),
    Device_Number = col_double(),
    Battery_Voltage = col_double(),
    Motor_Turns = col_double(),
    FR_Ratio = col_character(),
    Active_Poke = col_character(),
    Left_Poke_Count = col_double(),
    Right_Poke_Count = col_double(),
    Pellet_Count = col_double(),
    Retrieval_Time = col_double()
)

li <- lapply(file_list,
             function(x) readr::read_csv(x, col_types = FED3_classes))

df <- bind_rows(li) %>%
  rename(date = `MM:DD:YYYY hh:mm:ss`) %>%
  mutate(date = as.POSIXct(date, format = "%m/%d/%Y %H:%M:%S"),
         day = lubridate::day(date),
         month = lubridate::month(date),
         year = lubridate::year(date))

filter(df, FR_Ratio == "FED") %>%
  mutate(floor_minute = lubridate::floor_date(date, unit = "1 minute")) %>%
  ggplot(aes(date, Pellet_Count)) +
  geom_point()