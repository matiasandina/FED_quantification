#' This file contains useful functions to filter data
#' 

low_pass <- function(x, hz=0.1, type="low"){
bf <- signal::butter(3, hz) # 10 Hz  
# create filter without lag (forward and reverse filter)
y <- signal::filtfilt(bf, x)
# apply filter
z <- signal::filter(bf, x)
# replace the n first points with the median
# this is done to avoid edge effects
med <- runmed(x, 11)
y[1:11] <- med[1:11]
y[(length(y)-11):length(y)] <- med[(length(med)-11):length(med)]
z[1:11] <- med[1:11]
return(y)
}

#' FFT filter that uses signal::fftfilt with average window
#' it replaces the first values with the median for window to mitigate fft explosion at edges
#' 
#' @param x signal to filter
#' @param window length of the window to use for average weights and median mitigation of first points

fft_filter <- function(x, window=10){
  # apply n-point averaging filter
  weights <- rep(1, window)/window
  z <- signal::fftfilt(weights, x) 
  # replace the n first points with the median
  # this is done to avoid edge effects
  med <- runmed(x, window)
  z[1:window] <- med[1:window]
  return(z)
}