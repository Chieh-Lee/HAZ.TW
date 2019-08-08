#' @title Read file about fault.src
#'
#' @import dplyr
#' @import stringr
#' @import magrittr
#' @importFrom stats setNames
#' @importFrom stats na.omit
#'
#' @param path character: File path. Including all of additional file.
#' @param faultfile character: File name of 'fault.src'
#' @export
#'
#' @return a list of \code{faultfile}


read_fault<-function(path,faultfile){
  setwd(path)
  f1 <- scan(faultfile, what = 'c', sep = '\n', allowEscapes = TRUE, quiet = TRUE) %>% data.frame()
  fvalue <- as.character(word(f1[, 1], sep = fixed('\t\t'))) %>% data.frame()
  fvalue <- as.character(gsub("\t{1,}", " ", fvalue[, 1]), 1) %>% data.frame()
  segname <- grep("@@", as.character (as.matrix.data.frame( fvalue )))

  fname <- as.character(word(f1[, 1], - 1, sep = fixed('\t\t'))) %>% data.frame()
  fname <- as.character( gsub("\t{1,}", " ", fname[, 1]), 1) %>% data.frame()
  fname <- as.character( gsub(" ,", ",", fname[, 1]), 1) %>% data.frame()

  all <- lapply(1:length(segname), function(i){
    if (i < length(segname)) {
      ggvalue <- fvalue[ (segname[i]) : (segname[i + 1] - 1), 1]
      ggname  <- fname[ (segname[i]) : (segname[i + 1] - 1), 1]
    } else if (i == length(segname)) {
      ggvalue <- fvalue[ (segname[i]) : (dim(fvalue)[1]), 1]
      ggname  <- fname[ (segname[i]) : (dim(fvalue)[1]), 1]
    }
    return(list(ggname, ggvalue))
  })

  gg.2 <- lapply(1:length(all), function(i){
    no_seg <- grep('^[A-Z]+', all[[i]][[2]])
    lapply(1:length(no_seg), function(k){
      if (k < length(no_seg)) {
        ggvalue.3 <- data.frame(all[[i]][[2]])[(no_seg[k]) : (no_seg[k + 1] - 1), 1]
        ggname.3 <- data.frame(all[[i]][[1]])[(no_seg[k]) : (no_seg[k + 1] - 1), 1]
        ggname.3 <- gsub("^\\s|\\s$", "", ggname.3) #remove space before the title
      } else if (k == length(no_seg)){
        ggvalue.3 <- data.frame(all[[i]][[2]])[(no_seg[k]) : length(all[[i]][[2]]), 1]
        ggname.3 <- data.frame(all[[i]][[1]])[(no_seg[k]) : length(all[[i]][[2]]), 1]
        ggname.3 <- gsub("^\\s|\\s$", "", ggname.3) #remove space before the title
      }

      #extract only numeric title(location)
      ggn4 <- ggname.3[grepl("^[0-9]", ggname.3)]
      ggn5 <- ggn4[(grepl("[\\D]", ggn4))]
      ggn6 <- setdiff(ggn4, ggn5)

      ca <- sapply( 1:length(ggvalue.3), function(x){
        ifelse(x %in% grep('^[A-Za-z]+', ggvalue.3),
               ca <- strsplit(as.character(ggvalue.3[x]), "\\s") ,
               ca <- suppressWarnings(as.numeric(unlist(strsplit(as.character(ggvalue.3[x]), "\\s")))) %>% na.omit() %>% list())
      })

      ggname.3[1] <- 'fName'; ca <- setNames(ca, ggname.3)
      stype <- ca[[2]][[1]]  #stype = source type

      if (stype == 1 | stype == 2){ #source_type=1|2
        c1 <- ca[ggn6] %>% unlist() #extract lon/lats
        c1 <- c1[c1 != 0]
        ca[[length(ca)+1]] <- matrix(c1, (length(c1)/2), 2, byrow = TRUE) %>% data.frame()
        ca[c((5+1):(5+ca[[5]]))] <- NULL #remove lonlatpts
        names(ca)[[length(ca)]] <- "npts_lonlat"

      } else if ( stype == 3 | stype == 4) { #source_type=3|4
        ca[[5]] <- as.character(ca[[5]])
        Data <- scan(ca[[5]], what = 'c', sep = '\n', allowEscapes = TRUE, quiet = TRUE)
        D1_1 <- Data[1:3] #information
        D1_2 <- Data[4:length(Data)]; D1_2 <- unlist(str_split(D1_2, " ")) #location
        D1_3 <- matrix(D1_2, (length(D1_2)/3), 3, byrow = TRUE) %>% data.frame()
        ca[[5]] <- list(as.character(ca[[5]]), D1_1, D1_3)

      } else if ( stype == 5 | stype == -5) { #source_type=5
        nDownDip <- as.numeric(ca[[4]][[1]]); npts <- as.numeric(ca[[4]][[2]])
        c1 <- ca[grep("^[0-9]+", names(ca))] %>% unlist() #extract lon/lats/depth
        c2 <- matrix(c1, (nDownDip * npts), 3, byrow = TRUE) %>% data.frame()
        c2[, 3] <- c2[, 3] * (-1)                         #make depth to negative
        c2[, 4] <- rep(1:npts, each = nDownDip)           #set different group
        colnames(c2) <- c('lon', 'lat', 'depth', 'group')
        ca[[length(ca)+1]] <- c2 ; names(ca)[[length(ca)]] <- "npts_lonlat"
        ca[grep("^[0-9]+", names(ca))] <- NULL
      }
      return(ca)
    })
  })
}
