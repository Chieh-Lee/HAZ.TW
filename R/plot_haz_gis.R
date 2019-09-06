#' @title Plot HAZ GIS
#' 
#' @import knitr
#' @import mapview
#' @import sf
#' @param fault_gis character: Result of \code{read_haz_gis()}
#' 
#' @export
#'
#' @return figure of \code{fault_gis}

plot_haz_gis <- function(fault_gis){
  for(i in 1:nrow(fault_gis)){
    fg_g <- fault_gis$lonlat[i]
    fg_g <- strsplit(fg_g, '\t|\n') %>% unlist() %>% as.numeric()
    fgmat <- matrix(fg_g, nrow = (length(fg_g)/2), ncol = 2, byrow = TRUE) %>% data.frame(); colnames(fgmat) <- c('lon', 'lat')
    fault_gis$'geometry'[i] <- st_as_sf(fgmat, coords = c("lon", "lat"), crs = '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs') %>% st_combine() %>% st_cast("LINESTRING")
  }
  fault_gis$'lonlat' <- NULL
  
  fault_gis <- st_as_sf(fault_gis, crs = '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs')
  mapview(fault_gis, label = paste(fault_gis$Name, fault_gis$Segment, sep = '_'), legend = FALSE)
}


