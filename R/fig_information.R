#' @title Figure Information
#' 
#' @import knitr
#' @import sf
#' @param tabfile character string: File name of figure (.TAB)
#' @param outputname character string: Set the new output name 
#' 
#' @export
#'
#' @return table of \code{tabfile}


fig_information <- function(tabfile, outputname){
  dfall <- st_read(tabfile) %>% data.frame()

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
  
  ##combine and remove
  combine_all <- function(char, df){
    value_df <- df[, grep(char, names(df))]
    value = data.frame()
    for (i in 1:nrow(value_df)){
      value[i,1] <- do.call(paste, value_df[i, ])
    }
    vtitle <- combine_words(names(value_df), sep = " ", and = ' ') %>% as.character() #library(knitr)
    df[, grep(char, names(df))] <- NULL
    df <- cbind(df, value)
    colnames(df)[ncol(df)] <- vtitle
    return(df)
  }
  
  df <- combine_all('SR', df)
  df <- combine_all('VR', df)
  df <- combine_all('Dip', df)
  df <- combine_all('Depth', df)
  
  if (file.exists(outputname)) warning('File exists. Please set a new outputname.')
  
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      cc <- paste(df[i, j], names(df[j]), sep = '\t\t')
      write.table(cc, file = outputname, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "NA", append = TRUE)
    }
  }
 return(cc)
}
