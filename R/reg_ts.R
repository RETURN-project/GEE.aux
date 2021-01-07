library(zoo)

ifolder <- '/home/wanda/Documents/data/upscaleRecovery/test'
ifile <- 'tidy_test2.csv'

toRegularTS <- function(tsi, dts, fun, resol){
  # len <- length(tsi)
  # tdist <- tsi[1:(len/2)]
  # tsi <- tsi[(1+(len/2)):len]
  tsi <- as.numeric(tsi)
  if(resol == 'monthly'){
    z <- zoo(tsi, dts) ## create a zoo (time) series
    if(fun == 'max'){
      mz <- as.ts(aggregate(z, as.yearmon, mmax)) ## max
    } else if(fun == 'mean'){
      mz <- as.ts(aggregate(z, as.yearmon, function(x) mean(x,na.rm = TRUE))) ## mean
    }
  }else if (resol == 'daily'){
    mz <- bfastts(tsi, dts, type = "irregular")
  }else if (resol == 'quart'){
    z <- zoo(tsi, dts) ## create a zoo (time) series
    if(fun == 'max'){
      mz <- as.ts(aggregate(z, as.yearqtr, mmax)) ## max
    }
    if(fun == 'mean'){
      mz <- as.ts(aggregate(z, as.yearqtr, function(x) mean(x,na.rm = TRUE))) ## mean
    }
  }
  return(mz)
}

dat <- read_csv(file.path(ifolder,ifile))

cols <- names(dat)
dtcols <- cols[startsWith(cols,"p__")]
obscols <- cols[startsWith(cols,"series__")]
auxcols <- cols[!startsWith(cols,"p__") & !startsWith(cols,"series__")]
mnyr <- format(as.Date(as.character(min(dat[,dtcols], na.rm = T)),'%Y%m%d'),'%Y')
mxyr <- format(as.Date(as.character(max(dat[,dtcols], na.rm = T)),'%Y%m%d'),'%Y')

startdt <- as.Date(paste0(mnyr,'-01-01'))
enddt <- as.Date(paste0(mxyr,'-12-31'))
dts <- seq(startdt,enddt,by='month')

# dat_reg <- dat[]


dtsi <- dat[,dtcols]
dtsi <- cbind(rep(as.numeric(paste0(mnyr,'0101')),dim(dtsi)[1]),rep(as.numeric(paste0(mxyr,'1231'),dim(dtsi)[1])),dtsi)
obsi <- dat[,obscols]
obsi <- cbind(rep(NA,dim(obsi)[1]),rep(NA,dim(obsi)[1]),obsi)
# which(rowSums(!is.na(obsi))<2)

test <- function(dts,obs){
  obs <- obs[!is.na(dts)]
  dts <- dts[!is.na(dts)]
  dts <- as.Date(as.character(dts),'%Y%m%d')
  regts <- toRegularTS(obs, dts, 'mean', 'monthly')
} #sum(is.na(vector1))+sum(vector2)
# out <- apply(test, as.matrix(dtsi), as.matrix(obsi))

# dtsi <- dtsi[1:2,]
# obsi <- obsi[1:2,]

out <- sapply(1:dim(obsi)[1], function(i) test(dtsi[i,], obsi[i,]))
df_out <- as.data.frame(t(out))
names(df_out) <- dts
df_out <- cbind(dat[,auxcols],df_out)
save(df_out, file = file.path(ifolder, 'reg_test2.Rdata'))
# out <- unlist(out)


# obsi <- obsi[!is.na(dtsi)]
# dtsi <- dtsi[!is.na(dtsi)]
# dtsi <- as.Date(as.character(dtsi),'%Y%m%d')
# regts <- toRegularTS(c(obsi,NA,NA), c(dtsi,startdt, enddt), 'mean', 'monthly')


