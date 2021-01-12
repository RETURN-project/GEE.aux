#' Title
#'
#' @param dts
#' @param obsi
#' @param main
#' @param lossdt_start
#' @param lossdt_end
#' @param lossdt_start1
#' @param lossdt_end1
#' @param trend
#' @param met_rri
#' @param met_r80p
#' @param met_yryr
#'
#' @return
#' @export
#'
#' @examples
plot_aux <- function(dts, obsi,main, lossdt_start,lossdt_end,lossdt_start1,lossdt_end1,
                     trend,met_rri,met_r80p,met_yryr){
  plot(dts,obsi, xlab = 'Time', ylab = 'NBR [-]', main = paste0(id))
  lines(dts[!is.na(obsi)],obsi[!is.na(obsi)])
  abline(v = lossdt_start, col = 'red')
  abline(v = lossdt_end, col = 'red')
  abline(v = lossdt_start1, col = 'blue')
  abline(v = lossdt_end1, col = 'blue')
  if(length(trend)>1){
    lines(dts,trend, col = 'green')
  }
  mtext(paste0('RRI = ', sprintf('%.2f',met_rri),
               '\nR80p = ', sprintf('%.2f',met_r80p),
               '\nYrYr = ', sprintf('%.4f',met_yryr)), side=3, line=0.5, adj = 0)
}


