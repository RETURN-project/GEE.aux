ifolder <- '/home/wanda/Documents/data/upscaleRecovery/test'

load(file.path(ifolder, 'reg_test2.Rdata'))
load(file.path(ifolder, 'seg_test2.Rdata'))
load(file.path(ifolder, 'rec_test2.Rdata'))

dts <- as.Date(names(df_out)[-c(1:5)])

# helper function
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
PlotData <- function(id, df_out, seg, df_rec){
  lossdt_start <- as.Date(paste0(as.character(2000+df_out$lossyr[id]),'-01-01'))
  lossdt_end <- as.Date(paste0(as.character(2000+df_out$lossyr[id]),'-12-31'))
  lossdt_start1 <- as.Date(paste0(as.character(2000+df_out$lossyr[id]-1),'-01-01'))
  lossdt_end1 <- as.Date(paste0(as.character(2000+df_out$lossyr[id]+1),'-12-31'))
  
  obsi <- df_out[id,-c(1:5)] 
  trend <- seg[[id]]$trend
  met_rri <- df_rec$RRI[id]
  met_r80p <- df_rec$R80P[id]
  met_yryr <- df_rec$YrYr[id]
  
  plot_aux(dts, obsi,id, lossdt_start,lossdt_end,lossdt_start1,lossdt_end1,
         trend,met_rri,met_r80p,met_yryr)
}

for (id in 1:50){
  PlotData(id, df_out, seg, df_rec)
}

# plot time series with max, median and min RRI
id <- which(df_rec$RRI == max(df_rec$RRI, na.rm = T))
PlotData(id, df_out, seg, df_rec)

id <- which(df_rec$RRI == median(df_rec$RRI, na.rm = T))
PlotData(id, df_out, seg, df_rec)

id <- which(df_rec$RRI == min(df_rec$RRI, na.rm = T))
PlotData(id, df_out, seg, df_rec)
