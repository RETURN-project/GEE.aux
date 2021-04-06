#' Get the mean climate variable for each month using data after a predefined start date
#'
#' @param br brick of CRU climate data
#' @param startdt start date of the data that should be processed
#'
#' @return mean monthly climate variable
#' @export
#'
getMonthlyMeanClim <- function(br, startdt){
  br_yrs <- as.Date(names(br),'X%Y.%m.%d')
  br <- br[[which(br_yrs> as.Date(startdt))]]
  br_yrs <- br_yrs[which(br_yrs> as.Date(startdt))]
  indices <- as.numeric(format(br_yrs, format = "%m"))
  month_br<- stackApply(br, indices, fun = mean)# calculate mean montlhy precipitation
  names(month_br) <- month.abb
  return(month_br)
}
