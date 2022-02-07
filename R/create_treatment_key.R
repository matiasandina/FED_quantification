library(dplyr)
source("R/read_configs.R")

create_treatment_key <- function(params){
  # check dates are fine ----
  if (purrr::is_empty(params$experimental_dates)) {
    usethis::ui_stop("No dates were provided on configuration file.")
  } 
  # check they are in the correct format
  try_parse_date <- lubridate::as_date(params$experimental_dates)
  if (any(is.na(try_parse_date))) {
    usethis::ui_info("Some dates failed to parse.
                   Check they were entered like `yyyy-mm-dd`"
    )
    print(data.frame(original = params$experimental_dates, parsed = try_parse_date))
    usethis::ui_stop("Dates failed to parse, modify the config file")
    
  }
  
  # read configs ----
  all_dates <- c(params$habituation_dates, params$experimental_dates)
  configs <- purrr::map(all_dates, read_config_files) %>%
    bind_rows()
  
  if (nrow(configs) == 0) {
    usethis::ui_stop("Could not find configs.
                   Check that your dates are correct in the config file.")
  }
  
  # this is not checking what's receiving
  label_fed <- function(string){
    return(paste0("FED",
                  stringr::str_pad(readr::parse_number(string),
                                   width=3,
                                   pad=0)))
  }
  
  
  configs <- mutate(configs, 
                    FED=ifelse(!is.na(Comment),
                               label_fed(Comment), 
                               NA),
                    ID = stringr::str_pad(ID, pad=0, width=3))
  
  # create animal ids ----
  animal_ids <- glue::glue("{params$animals$min_ID:params$animals$max_ID}")
  to_remove <- glue::glue("{params$animals$to_remove}")
  
  # if we have things to remove
  if(purrr::is_empty(to_remove) == FALSE){
    # mind the ! to negate the detection
    to_keep <- !stringr::str_detect(animal_ids, to_remove)
    animal_ids <- animal_ids[to_keep]
  }

  # pad with 3
  animal_ids <- stringr::str_pad(readr::parse_number(animal_ids),
                                 width=3, pad=0)
  # create IDs with prefix
  animal_ids <- paste0(params$animals$ID_prefix, animal_ids)
  
  # filter animals -----
  configs <- filter(configs, ID %in% animal_ids)
  
  # fix treatment column -----
  configs %>% 
    group_by(ID) %>% 
    summarise(unique_treatment = unique(Treatment[!is.na(Treatment)])) -> unique_treatments_in_data
  
  # check length of unique treatments match
  if (nrow(unique_treatments_in_data) != configs %>% pull(ID) %>% unique() %>% length()) {
    usethis::ui_info("There are {nrow(unique_treatments_in_data)} animal -> treatment combinations")
    usethis::ui_info("There are {configs %>% pull(ID) %>% unique() %>% length} animals in the dataset")
    usethis::ui_stop("You have more animal->treatment than animals. Check your configs")
  }
  
  # We assume each animal was treated with just one thing.
  # We fill NAs accordingly
  configs <- configs %>% 
  group_by(ID) %>% 
    mutate(Treatment = unique(Treatment[!is.na(Treatment)]))
  
  # create treatment key ----
  treatment_key <- configs %>%
    select(ID, FED, Treatment) %>%
    tidyr::drop_na() %>% 
    distinct(ID, Treatment, FED)
  # feedback
  message("Inspect the treatment table below")
  print(treatment_key)
  
  # Do inspection of the treatment levels
  treatment_match <- identical(sort(unique(treatment_key$Treatment)),
                               sort(params$treatment))
  
  # TODO: fix mispell automagically ?
  
  if (treatment_match == FALSE) {
    usethis::ui_info(paste("Treatment levels on config.yaml are:",
                           stringr::str_c(params$treatment, collapse = ", ")
    ))
    usethis::ui_todo("Inspect your treatment key below for errors in spelling")
    print(treatment_key)
    usethis::ui_stop("Treatment levels don't match.
                   Check spelling on configs")
  }
  
  #readr::write_csv(treatment_key, "treatment_key.csv")
  #usethis::ui_done("Looks like everything is fine, writing to file")
  return(treatment_key)

}

