#' @title Read HAZ GIS
#' 
#' @import knitr
#' @import sf
#' @param fault_tab character string: File name of figure (.TAB)
#' 
#' @export
#'
#' @return table of \code{fault_tab}


read_haz_gis <- function(fault_tab){
  dfall <- st_read(fault_tab) %>% data.frame()

  #df = data exclude lon/lat geometry
  df <- dfall[, -which(names(dfall) == 'geometry')]
  df <- data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  
  ##extract lon/lat geometry
  df_lonlat <- dfall[, which(names(dfall) == 'geometry')] 
  for (i in 1:length(df_lonlat)){ 
    n1 <- df_lonlat [[i]] %>% as.numeric() %>% unlist()
    n1 <- matrix(n1, (length(n1)/2), 2, byrow = FALSE) %>% data.frame()
    df$npts[i] <- nrow(n1)
    n2 <- sapply(1:nrow(n1), function(j){
      paste(n1[j, 1], n1[j, 2], sep = '\t')
    })
    df$lonlat[i] <- paste(n2, collapse = '\n')
  }

 return(df)
}
