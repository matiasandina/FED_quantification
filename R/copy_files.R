#' This function copies provided files from a path to a new folder at destiny
#' The normal use is to copy from the backup folder
#' This function will also delete empty files 
#' @seealso `find_files()`

copy_files <- function(file_list, target_dir = NULL) {
  if (is.null(target_dir)) {
    target_dir <- choices::choose_directory(title =,
                                            method = "rstudioapi")
    
  }
  # create new folder called FED_data
  new_fed <- file.path(target_dir, "FED_data")
  fs::dir_create(new_fed)
  new_files <- file.path(new_fed, basename(file_list))
  
  file.copy(from = file_list, new_fed)
  # Feedback
  usethis::ui_done(glue::glue("{length(new_files)} FED files were copied"))
  # remove empty files
  file_df <- data.frame(
    new_files = new_files,
    size = file.size(new_files),
    stringsAsFactors = FALSE
  )
  
  files_to_remove <- file_df[file_df$size == 0,]$new_files
  file.remove(files_to_remove)
  usethis::ui_info(glue::glue("{length(files_to_remove)} files were empty and thus removed from selected folder"))
  usethis::ui_line("You can find the data at:")
  usethis::ui_path(glue::glue("{new_fed}"))
}
