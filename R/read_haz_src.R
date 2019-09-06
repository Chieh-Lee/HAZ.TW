#' @title Fortran to Table
#'
#' @import dplyr
#' @import here
#' @import stringr
#' @import magrittr
#' @import knitr
#' @importFrom stats setNames
#' @importFrom stats na.omit
#' 
#' @param fault.src character: File name of 'fault.src'
#' 
#' @export
#'
#' @return table of \code{fault.src}

read_haz_src <- function(fault.src){
  f1 <- scan(fault.src, what = 'c', sep = '\n', allowEscapes = TRUE, quiet = TRUE) %>% data.frame()
  fvalue <- as.character(word(f1[, 1], sep = fixed('\t\t\t'))) 
  fvalue <- as.character(gsub("\t{1,}", " ", fvalue)) 
  segname <- grep("@@", fvalue)
  
  fname <- as.character(word(f1[, 1], -1, sep = fixed('\t\t')))
  fname <- as.character( gsub("\t{1,}", " ", fname))
  fname <- as.character( gsub(" ,", ",", fname)) 
  
  all <- lapply(1:length(segname), function(i){
    if (i < length(segname)) {
      ggvalue <- fvalue[ (segname[i]) : (segname[i + 1] - 1)]
      ggname  <- fname[ (segname[i]) : (segname[i + 1] - 1)]
    } else if (i == length(segname)) {
      ggvalue <- fvalue[ (segname[i]) : length(fvalue)]
      ggname  <- fname[ (segname[i]) : length(fvalue)]
    }
    return(list(ggname, ggvalue))
  })
  
  data <- lapply(1:length(all), function(i){ 
    no_seg <- grep('^[A-Z]+|^\'', all[[i]][[2]])
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
        ifelse(x %in% !grepl('^[0-9]+|\\.', ggvalue.3),
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
        #Data <- scan(here::here(dirname(fault.src), names(ca)[[5]]), what = 'c', sep = '\n', allowEscapes = TRUE, quiet = TRUE)
        Data <- scan(paste(dirname(fault.src), names(ca)[[5]], sep = '/'), what = 'c', sep = '\n', allowEscapes = TRUE, quiet = TRUE)
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
  
  firstup <- function(x) {
    substr(x, 1, 1) <- tolower(substr(x, 1, 1))
    x
  }
  funfun <- function(nseq){
    newname <- matrix()
    if (nseq == "n_ftype"){
      if (n_ftype > 0){
        namelist <- c('fault Mech Model Wt for model', 'number of mechanisms for model', 
                      'ftype for model', 'wts for observation for model')
        n <- rep(1:n_ftype, each = 4)
        newname <- paste0(namelist, n)
      }
    } else if (nseq == 'n_recur'){
      if (n_recur > 0){
        n <- rep(1:n_recur)
        newname <- paste0('delta_M1, delta_M2, delta_M3 for model', n)
      }
    } 
    return (newname)
  }
  all_2 <- data.frame()
  
  for( i in 1:length(all)){ 
    df_name <- all[[i]][[1]] 
    df_value <- all[[i]][[2]]  
    
    df <- seq(1, length(data[[i]])) %>% data.frame(); colnames(df) <- "segID"
    #df$fName <- df_value[grep('^[A-Z]+', as.character(df_value))]
    df$fName <- df_value[!grepl('^[0-9]+|^@@|^\\-|^\\.|.txt$|^\\s+', as.character(df_value))]
    df$faultName <- rep(df_value[grep("@@", as.character(df_value))], times = length(data[[i]]))
    df$faultID <- rep(i, times = length(data[[i]]))
    df$'Prob Activity' <- df_value[2]
    df$'nSeg model' <- as.numeric(as.character(df_value[3]))
    df$'segWt' <- df_value[4]
    df$'Total nb of sources' <- length(df_value[grep('^[A-Z]+', df_value)])
    
    if (as.numeric(as.character(df_value[3])) == 1){
      df$'faultflag' <- df_value[6]
    } else {
      df$'faultflag' <- paste(df_value[6 : (6 + as.numeric(as.character(df_value[3])) - 1)], collapse = "\n")
    }
    
    if (length(unique(df$segID)) != length(unique(df$fName))){ #same segname
      new_segnameID <- letters[1:(length(unique(df$segID)))]
      new_segname <- as.character(paste0(gsub(' ', '', unlist(df$fName)), new_segnameID))
      df$fName <- new_segname
    }
    

    for (k in 1:length(data[[i]])){
      dn1 <- data[[i]][[k]]
      dn1 <- dn1[-(which(sapply(dn1, is.list)))]
      
      dn1 <- plyr::ldply(dn1, rbind)
      dn1 <- sapply(dn1, as.character)
      dn1[is.na(dn1)] <- ""
      
      dn3 <- as.character( gsub("\\s{1,}$", "", dn1[, 1])) %>% data.frame() #parameter name
      dn3$orderid <- 1:nrow(dn3)
      dn3$seg <- sapply(1:nrow(dn1), function(m){ combine_words(dn1[m, 2:(ncol(dn1))], sep = " ", and = '') }) %>% data.frame()
      
      dn3[, 1] <- as.character(dn3[, 1]) %>% firstup()

      if (length(grep('^nTHick|number of fault thickness|number of fault widths', dn3[, 1])) != 0){
        n_thick <- dn3[grep('^nTHick|number of fault thickness|number of fault widths', dn3[, 1]), 'seg']
        n_thick <- as.numeric(as.character(n_thick$.))
        if (n_thick != length(grep('weights for max mag', dn3[, 1]))) stop(paste0('Missing parameters: weights for max mag (Fault:', i, ' Segment:', k, ')'))
        n <- rep(1:n_thick)
        dn3[grep('^number of maximum magnitudes', dn3[, 1]), 1] <- paste(dn3[grep('^number of maximum magnitudes', dn3[, 1]), 1], 'for model', n, sep = ' ')
        dn3[grep('^maximum magnitudes|^reference magnitude', dn3[, 1]), 1] <- paste(dn3[grep('^maximum magnitudes|^reference magnitude', dn3[, 1]), 1], 'for model', n, sep = ' ')
        dn3[grep('^weights for max mag', dn3[, 1]), 1] <- paste(dn3[grep('^weights for max mag', dn3[, 1]), 1], 'for model', n, sep = ' ')
        
      } 
      
      if (length(grep('nRecur', dn3[, 1])) != 0){
        n_recur <- dn3[grep('nRecur', dn3[, 1]), 'seg']
        n_recur <- as.numeric(as.character(n_recur$.))
        if (n_recur != length(grep('delta_M1|pdf param for exp model', dn3[, 1]))) stop(paste0('Missing parameters: delta_M1, delta_M2, delta_M3 (Fault:', i, ' Segment:', k, ')'))
        dn3[grep('delta_M1, delta_M2,delta_M3|delta_M1, delta_M2, delta_M3|pdf param for exp model', dn3[, 1]), 1] <- funfun("n_recur")
      }
      
      if (length(grep('number of Fault Mechanism Models', dn3[, 1])) != 0){
        n_ftype <- dn3[grep('Fault Mechanism Models', dn3[, 1]), 'seg']
        n_ftype <- as.numeric(as.character(n_ftype$.))
        nth <- c((grep('Fault Mechanism Models', dn3[, 1]) + 1) : (grep('Fault Mechanism Models', dn3[, 1]) + 4 * n_ftype))
        if (n_ftype != (length(nth)/4)) stop(paste0('Missing parameters about Fault Mechanism Models (Fault:', i, ' Segment:', k, ')' ))
        dn3[nth, 1] <- funfun("n_ftype")
      }
      
      ifelse(k == 1,
             dnall <- dn3,
             dnall <- dplyr::full_join(x = dnall, y = dn3[, c('.','seg')], by = names(dn3[1])))
      
    }
    
    if (ncol(dnall) > 3){
      sum_of_seg <- ncol(dnall) - 2 #title & ID 
      if (length(unique(as.character(unlist(dnall[1, 3:ncol(dnall)])))) != sum_of_seg){ #same segname
        new_segnameID <- letters[1:(sum_of_seg)]
        new_segname <- as.character(paste0(gsub(' ', '', unlist(dnall[1, 3:ncol(dnall)])), new_segnameID))
        
        dnall <- dnall %>% as.matrix()
        dnall[1, 3:ncol(dnall) ] <- new_segname; 
        dnall <- dnall %>% data.frame()
      } 
    }

    #####################################################
    dnall <- dnall[order(dnall$orderid), ]; dnall$orderid <- NULL
    dnall <- as.data.frame(t(dnall), stringsAsFactors=FALSE)
    colnames(dnall) <- dnall[1, ]; dnall <- dnall[-1, ]
    dnall$fName <- as.character( gsub(" ", "", dnall[, 1]))
    
    j1 <- 1; j2 <- 1; j3 <- 1
    for (p in 1:nrow(dnall)){
      stype <- dnall[p, grep('^source type', names(dnall))] 
      stype <- as.character(gsub("^\\s|\\s$", "", stype))
      stype <- strsplit(stype, split = ' ') %>% unlist() %>% as.numeric()
      stype <- stype[1] %>% as.numeric()
      
      if (stype == 1 | stype == 2){
        num <- df_value[grep('npts', df_name)] %>% as.character() %>% as.numeric()
        if (is.na(dnall$npts)[p] == TRUE){
          dnall$geometry[p] <- NA
        } else {
          if (length(df_name[(grep('npts', df_name)[j1] + 1):(grep('npts', df_name)[j1] + num[j1])]) < 3){
            dnall$geometry[p] <- combine_words(df_name[(grep('npts', df_name)[j1] + 1):(grep('npts', df_name)[j1] + num[j1])], 
                                               sep = "\r\n", and = '\r\n' )
          } else {
            dnall$geometry[p] <- combine_words(df_name[(grep('npts', df_name)[j1] + 1):(grep('npts', df_name)[j1] + num[j1])], 
                                               sep = "\r\n", and = '' )
          }
          j1 <- j1 + 1
        }
      } else if (stype == 3 | stype == 4){
        num <- df_value[grep('.txt', df_name)][p]
        dnall$geometry[p] <- as.character(num)
        j2 <- j2 + 1
      } else if (stype == 5 | stype == -5){
        num <- sub("^\\s+", "", df_value[grep('npts', df_name)][j3])
        num <- strsplit(num, split = ' ') %>% unlist() %>% as.character() %>% as.numeric()
        nDowndip <- num[1]; npts <- num[2]
        if (length(df_name[(grep('npts', df_name)[j3] + 1):(grep('npts', df_name)[j3] + npts)]) < 3){
          dnall$geometry[p] <- combine_words(df_name[(grep('npts', df_name)[j3] + 1):(grep('npts', df_name)[j3] + npts)], 
                                             sep = "\r\n", and = '\r\n' )
        } else {
          dnall$geometry[p] <- combine_words(df_name[(grep('npts', df_name)[j3] + 1):(grep('npts', df_name)[j3] + npts)], 
                                             sep = "\r\n", and = '' )
        }
        j3 <- j3+1
      }
    }
    
    all_1 <- merge(df, dnall, by = 'fName', all = TRUE)
    
    ifelse(length(all_2) < 1,
           all_2 <- all_1,
           all_2 <- merge(all_2, all_1, all = TRUE))
  
  }
  all_2 <- dplyr::arrange(all_2, all_2$faultID, all_2$segID)

  return(all_2)
}
