#' This function will create all needed things for a new homecage experiment
#' 
#'

new_HC_experiment <- function(experiment_name, experiment_folder = "/home/choilab/Experiments/Liu/") {
  # create folder
  new_folder <- file.path(experiment_folder, experiment_name)
  if (dir.exists(new_folder) == FALSE) {dir.create(new_folder)}
  
} 