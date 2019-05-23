# ===============================================================================
# Functions for Climate data Analysis | Last Update: 2019-05-23 | v3.0
# Inquiries regarding this code contact:
# gabriel.carrasco@lshtm.ac.uk
# ===============================================================================

# climate_malaria_v3_data -----------------------------------------------------------------
clim_dat<-function (shp, par, start="2000-01-01", end="2017-12-01") {
  AOI = getAOI(clip = shp)
  g = AOI %>%  getTerraClim(param = par, startDate = start, endDate = end)
  ras <- raster::brick(g)
  ras <- raster::mask(ras, shp)
  df = as.data.frame(
    raster::extract(ras, shp, fun = mean, sp = T,  na.rm = T,
                    weights = F, normalizeWeights=F, df = T, # Change weights parameters
                    layer = 1, nl = ras@data@nlayers))
  return(df)
}

# climate_malaria_v3_plots -----------------------------------------------------------------
filled.contour.gce <- function(dat, var, col, lev) {
  
  matrix.axes <- function(data) {
    # Do the rows, las=2 for text perpendicular to the axis
    x <- (1:dim(data)[1] - 1) / (dim(data)[1] - 1);
    axis(side=1, at=x, labels=rownames(data), las=2);
    # Do the columns
    x <- (1:dim(data)[2] - 1) / (dim(data)[2] - 1);
    axis(side=2, at=x, labels=colnames(data), las=2);
  }
  
  var1 <- enquo(var)
  ff1 <- dat %>%
    dplyr::select(month,year, !!var1) %>%
    spread(year, !!var1)
  h<-as.matrix(ff1[,2:ncol(ff1)])
  rownames(h)<-ff1$month
  filled.contour(h, plot.axes=matrix.axes(h), main=deparse(substitute(var)), col = col(lev), nlevels = lev)
}

filled.contour.tidy <- function(dat, var, palette, trans = "identity", log1 = T, dire = 1) {
  library(ggisoband)
  var1 <- enquo(var)
  name <- dat %>% ungroup() %>% dplyr::select(!!var1)
  name <- colnames(name)
  
  dat %>% dplyr::select(month,year,!!var1) %>%
    ggplot(aes(month, year)) +
    {if(log1) {geom_isobands(aes(z =  log10(!!var1), fill = stat(z)), color = NA)}
      else{geom_isobands(aes(z = !!var1, fill = stat(z)), color = NA)}
    } +
    scale_fill_distiller(palette = palette,  trans = trans, direction = dire) +
    labs(title = var1) +
    {if(log1) {labs(fill = paste0("log10 \n", name))}
      else{labs(fill = paste0(name))}
    } +
    coord_cartesian(expand = FALSE) +
    geom_hline(yintercept = c(2006,2010),  linetype  = "dashed") +
    theme_bw()
}
