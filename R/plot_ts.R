#' Plot recovery metrics
#'
#' @param dts vector of dates
#' @param obsi time series - vector of observations
#' @param main title of the plot (string)
#' @param lossdt_start start uncertainty window of disturbance date
#' @param lossdt_end end uncertainty window of disturbance date
#' @param lossdt_start1 start date for time window of accepted breaks
#' @param lossdt_end1 end date for time window of accepted breaks
#' @param trend fitted trend component - vector of observations
#' @param met_rri derived RRI metric
#' @param met_r80p derived R80p metric
#' @param met_yryr derived YrYr metric
#'
#' @return plot
#' @export
#'
plot_aux <- function(dts, obsi,main, lossdt_start,lossdt_end,predt_start,distdt_end,postdt_start,postdt_end,
                     trend,met_rri,met_r80p,met_yryr,...){
  plot(dts,obsi, xlab = 'Time', ylab = 'NBR [-]', main = paste0(main),...)
  lines(dts[!is.na(obsi)],obsi[!is.na(obsi)])
  abline(v = lossdt_start, col = 'red')
  abline(v = lossdt_end, col = 'red')
  abline(v = distdt_end, col = 'gray')
  abline(v = predt_start, col = 'gray')
  abline(v = postdt_start, col = 'gray')
  abline(v = postdt_end, col = 'gray')
  if(length(trend)>1){
    lines(dts,trend, col = 'green')
  }
  mtext(paste0('RRI = ', sprintf('%.2f',met_rri),
               '\nR80p = ', sprintf('%.2f',met_r80p),
               '\nYrYr = ', sprintf('%.4f',met_yryr)), side=3, line=0.5, adj = 0)
}


