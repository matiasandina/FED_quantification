#' this is a legacy function from the thermal data analysis
#' It was used to trim the datetimes in the thermal data using params on an Rmd file
#' I decided that it's better to not have it always as part of the pipeline
#' Main reasons:
#' 1) it requires to set up more parameters 
#' 2) we always end up filtering at different points anyway
#' keeping it only for future reference
trim_experiment <- function(df){
  trim <- case_when(is.null(params$start) & is.null(params$end) ~ "keep_all",
                    is.null(params$start) & !is.null(params$end) ~ "end_only",
                    !is.null(params$start) & is.null(params$end) ~ "start_only",
                    !is.null(params$start) & !is.null(params$end) ~ "both")
  print(trim)
  switch(trim,
         "keep_all" = return(df),
         "start_only" = return(filter(df, timepoint > params$start)),
         "end_only" = return(filter(df, timepoint < params$end)),
         "both" = return(filter(df, between(timepoint,
                                            parse_datetime(params$start),
                                            parse_datetime(params$end)))
         )
  ) 
}
