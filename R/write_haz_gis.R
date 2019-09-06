#' @title Figure Information
#' 
#' @import knitr
#' @import sf
#' @param fault_gis character: Result of \code{read_haz_gis()}
#' @param fault_gis_out character: Output file name. 
#' @param driver character: Output driver. Use \code{st_drivers()} to get more information; default: \code{FALSE}
#' 
#' @export
#'
#' @return shapefile of \code{fault_gis}

write_haz_gis <- function(fault_gis, fault_gis_out, driver = "MapInfo File"){
  if (file.exists(fault_gis_out)) warning('File exists. Please set a new outputname.')
  for(i in 1:nrow(fault_gis)){
    fg_g <- fault_gis$lonlat[i]
    fg_g <- strsplit(fg_g, '\t|\n') %>% unlist() %>% as.numeric()
    fgmat <- matrix(fg_g, nrow = (length(fg_g)/2), ncol = 2, byrow = TRUE) %>% data.frame(); colnames(fgmat) <- c('lon', 'lat')
    fault_gis$'geometry'[i] <- st_as_sf(fgmat, coords = c("lon", "lat"), crs = '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs') %>% st_combine() %>% st_cast("LINESTRING")
  }
  fault_gis$'lonlat' <- NULL
  
  fault_gis <- st_as_sf(fault_gis, crs = '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs')
  st_write(fault_gis, fault_gis_out, driver = driver)
}
