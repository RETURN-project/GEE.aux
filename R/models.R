#' Plot histogram and marginal effects
#'
#' @param mod_brt Boosted regression tree model
#' @param dat_covar dataframe of covariate values
#' @param names_covars names of the covariates (as used in BRT and the dataframe)
#' @param names_covars_short the covariate names used for plotting (order should be the same as names_covars)
#'
#' @return plot
#' @import reshape2
#' @import dplyr
#' @import tidyverse
#' @import plyr
#' @export
#'
plot_BRT_hist <- function(mod_brt, dat_covar, names_covars, names_covars_short){
  nvar <- length(names_covars)
  smmry <- summary(mod_brt,plotit=F)

  for(i in 1:nvar){
    rspi <- plot.gbm(mod_brt, i.var = i, return=T)
    varnms <- names(rspi)
    names(rspi) <- c('x','y')
    rspi$variable <- varnms[1]
    rspi$infl <- smmry$rel.inf[smmry$var == varnms[1]]
    if(i == 1){rsp <- rspi}else{rsp <- rbind(rsp,rspi)}
  }

  dat_covar_mlt <- melt(dat_covar)
  dat_covar_mlt$tp <- 'Count'
  rsp$tp <- 'Marginal effect'
  rsp <- rsp[ with(rsp, order(infl, decreasing = T)),]

  rsp$variable <- factor(rsp$variable,levels = smmry$var)
  rsp$variable <- mapvalues(rsp$variable, from=names_covars, to=paste0(names_covars_short, " [", sprintf('%.1f',smmry$rel.inf[order(match(smmry$var,names_covars))]),"%]"))

  dat_covar_mlt$variable <- mapvalues(dat_covar_mlt$variable, from=names_covars, to=paste0(names_covars_short, " [", sprintf('%.1f',smmry$rel.inf[order(match(smmry$var,names_covars))]),"%]"))

  smmry$var <- mapvalues(smmry$var, from=names_covars, to=paste0(names_covars_short, " [", sprintf('%.1f',smmry$rel.inf[order(match(smmry$var,names_covars))]),"%]"))
  rsp$variable <- factor(rsp$variable, levels = smmry$var)
  dat_covar_mlt$variable <- factor(dat_covar_mlt$variable, levels = smmry$var)

  ggplot() +
    geom_histogram(data = dat_covar_mlt, aes(x = value), alpha = .5)+
    geom_line(data = rsp, aes(x = x, y = y), col = "red")+
    facet_grid(tp ~ variable, scales='free')

}
