#' Tidy GEE data
#'
#' @param x input data
#' @param col_names column names associated with the input data
#'
#' @return
#' @export
#' @import tidyverse
#' @import stringr
#' @import readr
#'
tidy_data <- function(x, col_names){
  # a <- str_split(tools::file_path_sans_ext(file),"_")%>%unlist
  # remove the '[' at the beginning and end of the file
  y <-  substr(x,2,str_length(x)-1)
  # replace the "]" by "["
  z <- gsub( "]", "[", y )
  #z <- z[-1]
  # write to temporary file
  tmpf <- tempfile()
  cat(z, file=tmpf, sep="\n")

  # load data again, to get rid of the "[" seperator and assign column names
  dat_orig <- readr::read_csv(tmpf, quote="[",
                       col_names=col_names)
  # count number of images per time series
  dat_format <- dat_orig%>%
    mutate(nimg=str_count(img_names,",")+1)

  # get the maximum number of observations per time series
  max_img <- max(dat_format$nimg)
  nvars_img=sprintf("p__%04i",seq(from=1,to=max_img))# column names for the dates
  nvars_1=sprintf("series__%04i",seq(from=1,to=max_img))# column names for the time series observations

  # separate the string of image names and NBR series into columns, extract date from image name, and replace 'None' by 'NA'
  dat_format_img <- dat_format%>%
    separate(img_names,into=nvars_img,sep=", ",convert=TRUE)%>%
    separate(series,into=nvars_1,sep=", ",convert=TRUE)%>%
    mutate_at(vars(starts_with("p__")),extract_date)%>%
    mutate_all(~str_replace(.,"None","NA"))

  return(dat_format_img)
}

#' Extract date from image name
#'
#' Helper function for tidy_data
#'
#' @param x image name
#'
#' @return date
#' @export
#'
extract_date <- function(x){
  sp <- strsplit(x,'_')
  L <- lapply(sp, function(x) {
    if(length(x) > 1){x[length(x)-1]
    } else{x[1]}} )
  as.numeric(unlist(L))
}


