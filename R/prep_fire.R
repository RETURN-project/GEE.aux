#' Extract date from CCI fire image name
#'
#' @param x image name
#'
#' @return date
#' @export
#'
extract_date_fire <- function(x){
  sp <- strsplit(x,'_')
  L <- lapply(sp, function(x){x[1:3]})
  L <- as.numeric(str_replace(unlist(L),"'",""))
  L[1]*10000 + L[2]*100 + L[3]
}

#' Extract the dates of fire events
#'
#' @param ts_conf time series of the fire confidence level (vector of observations)
#' @param ts_doy time series of the fire day of year (vector of observations)
#' @param dts dates (vector of Date objects)
#' @param conf_level minimum fire confidence level to consider a date as fire event
#'
#' @return dates of fire events
#' @export
#'
getFireDate <- function(ts_conf, ts_doy, dts, conf_level){
  # extract the fire events of interest
  ind <- which((ts_conf > conf_level) & (ts_doy>0))# confidence level should exceed threshold and doy should be larger than
  if(length(ind)>0){
    # get the dates of the fire events
    doy <- ts_doy[ind]
    yr <- format(as.Date(dts[ind]),'%Y')
    dt <- as.Date(doy-1, format = "%j", origin = as.Date(paste0(yr,"-01-01")))
  }else{
    dt <- NA
  }
  return(dt)
}

#' Extract a date frame of fire doy and fire confidence level from data downloaded via GEE
#'
#' @param dat_fire dataframe of fire related data
#'
#' @return date frame of fire doy and fire confidence level
#' @export
#' @import dplyr
#' @import tidyr
#'
tidy_fire_date <- function(dat_fire){
  # count the number of observations per time series
  dat_fire <- dat_fire%>%
    mutate(nimg=str_count(img_conf_,",")+1)
  max_img <- max(dat_fire$nimg)
  # define column names
  nvars_dt=sprintf("dt__%04i",seq(from=1,to=max_img))# column names for the dates
  nvars_doy_obs=sprintf("DOYobs__%04i",seq(from=1,to=max_img))# column names for the time series (doy of fire)
  nvars_conf_obs=sprintf("CONFobs__%04i",seq(from=1,to=max_img))# column names for the time series (confidence level of fire)
  # split time series string into separate columns, extract date from image name, and replace None by NA
  dat_tidy_fire <- dat_fire%>%
    separate(img_doy_,into=nvars_dt,sep=", ",convert=TRUE)%>%
    separate(doy_fire_,into=nvars_doy_obs,sep=", ",convert=TRUE)%>%
    separate(conf_fire_,into=nvars_conf_obs,sep=", ",convert=TRUE)%>%
    mutate_at(vars(starts_with("dt__")),extract_date_fire)%>%
    mutate_all(~str_replace(.,"None","NA"))
  # get names of the fire columns
  firecols <- names(dat_tidy_fire)
  confcols <- firecols[startsWith(firecols,"CONFobs__")]# column names of the columns containing the observation dates
  doycols <- firecols[startsWith(firecols,"DOYobs_")]# column names of the columns containing the
  dtcols <- firecols[startsWith(firecols,"dt_")]
  auxcols <- firecols[!startsWith(firecols,"dt_") & !startsWith(firecols,"CONFobs__") & !startsWith(firecols,"DOYobs_") & !startsWith(firecols,"img_conf_")]
  # make dataframe of fire doy
  fire_doy <- dat_tidy_fire[,doycols]
  fire_doy <- as.data.frame(sapply(fire_doy, as.numeric))
  firedts <- as.Date(as.character(dat_tidy_fire[1,dtcols]),'%Y%m%d')
  names(fire_doy) <- firedts
  fire_doy <- cbind(dat_tidy_fire[,auxcols], fire_doy)
  # make dataframe of fire confidence
  fire_conf<- dat_tidy_fire[,confcols]
  fire_conf <- as.data.frame(sapply(fire_conf, as.numeric))
  names(fire_conf) <- firedts
  fire_conf <- cbind(dat_tidy_fire[,auxcols], fire_conf)
  # export dataframes
  out <- list(fire_doy, fire_conf)
  names(out) <- c('fire_doy', 'fire_conf')
  return(out)
}
