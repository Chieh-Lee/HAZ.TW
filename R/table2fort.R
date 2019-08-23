#' @title Table to Fortran
#' 
#' @import knitr
#' @param icoor numeric: The unit of the source coordinate, 0 is km, 1 is latitude and longitude
#' @param inputfile character string: Table of 'fault.src'
#' @param outputname character string: Set the new output name 
#' 
#' @export
#'
#' @return fortran file of \code{faultfile}

table2fort <- function(icoor, inputfile, outputname){
  aa <- inputfile
  pp <- function(paravalue, paraname){
    if (length(paravalue) == 0) warning(paste0('Missing parameter:', paraname, ' (Fault: ', i, '; Segment:', j, ')'))
    paste(paravalue, paraname, sep = '\t\t')
  }
  all_ot <- vector()
  
  all_ot[1] <- icoor
  all_ot[2] <- length(unique(aa$faultID))
  allall <- all_ot %>% data.frame()
  
  for (i in unique(aa$faultName)){ 
    
    seg_ot <- vector(); seg_ot2 <- vector()
    #fault information
    segdata <- aa[aa$faultName == i, ]
    seg_ot[1] <- i; segdata$faultName <- NULL; segdata$faultID <- NULL; segdata$segID <- NULL#faultName
    seg_ot[2] <- paste(unique(segdata$`Prob Activity`), 'Prob Activity', sep = '\t\t'); segdata$`Prob Activity` <- NULL #prob activity
    seg_ot[3] <- paste(unique(segdata$`nSeg model`), 'nseg model', sep = '\t\t'); segdata$`nSeg model` <- NULL  #nseg model
    seg_ot[4] <- paste(unique(segdata$segWt), 'wts for nseg models', sep = '\t\t'); segdata$segWt <- NULL #wts for nseg models
    seg_ot[5] <- paste(unique(segdata$`Total nb of sources`), 'total nb of sources', sep = '\t\t'); segdata$`Total nb of sources` <- NULL#total nb of sources
    seg_ot[6] <- unique(segdata$sources); segdata$sources <- NULL#sources 
    
    seg_ot <- seg_ot %>% data.frame()
    seg222 <- segdata[, colSums(is.na(segdata)) != nrow(segdata) ]
    
    for (j in 1:nrow(segdata)){ 
      segdata <- seg222[j, ]
      segdata <- segdata[, colSums(is.na(segdata)) != nrow(segdata) ]
      
      seg_ot2 <- vector()
      seg_ot2[1] <- segdata$fName
      seg_ot2[2] <- segdata[, grep('source type',names(segdata))] %>% pp(paraname = 'source type, atten type, sampleStep (km), dirflag(0=no directivity effect), synchron')
      seg_ot2[3] <- segdata[, grep('aleatory seg',names(segdata))] %>% pp(paraname = 'aleatory seg wt.')
      
      #check source type-npts-geometry
      stype <- seg_ot2[2]
      stype <- substr(stype, 0, 2) %>% as.numeric()
      
      #npts part
      if (stype == 1 | stype == 2){
        seg_ot2_npts1 <- segdata[, grep('^dip, ', names(segdata))] %>% pp(paraname = 'dip, depth to top')
        seg_ot2_npts2 <- segdata[, grep('npts', names(segdata))] %>% pp(paraname = 'npts')
        seg_ot2[4] <- paste(seg_ot2_npts1, seg_ot2_npts2, sep = '\r\n') 
        seg_ot2[5] <- segdata[, grep('geometry', names(segdata))]  #geometry
      } else if (stype == 3 | stype == 4){
        seg_ot2[4] <- segdata[, grep('depth to top', names(segdata))] %>% pp(paraname = 'dip, depth to top')
        seg_ot2[5] <- segdata[, grep('geometry', names(segdata))]  #filename
      } else if (stype == 5 | stype == -5){
        seg_ot2[4] <- segdata[, grep('npts', names(segdata))] %>% pp(paraname = 'npts')
        seg_ot2[5] <- segdata[, grep('geometry', names(segdata))]  #geometry
      }
      
      #number of dip 
      if (stype == 5 | stype == -5){
        seg_ot2[6] <- NA
      } else {
        seg_ot2_dip1 <- segdata[, grep('^number of dip', names(segdata))] %>% pp(paraname = 'number of dip variations')
        seg_ot2_dip2 <- segdata[, grep('^dip variations$|n_Dip values of DeltaDip', names(segdata))] %>% pp(paraname = names(segdata)[grep('^dip variations$|n_Dip values of DeltaDip', names(segdata))])
        seg_ot2_dip3 <- segdata[, grep('^wt for dip|n_Dip values of dipWt', names(segdata))] %>% pp(paraname = names(segdata)[grep('^wt for dip|n_Dip values of dipWt', names(segdata))])
        seg_ot2[6] <- paste(seg_ot2_dip1, seg_ot2_dip2, seg_ot2_dip3, sep = '\r\n')
      }
      
      
      #number of b-value
      seg_ot2_b1 <- segdata[, grep('^number of b-values', names(segdata))] %>% pp(paraname = 'number of b-values')
      if (as.numeric(substr(seg_ot2_b1, 0, 2)) > 0){
        seg_ot2_b2 <- segdata[, grep('^b-values', names(segdata))] %>% pp(paraname = 'b-values')
        seg_ot2_b3 <- segdata[, grep('^weights for b-values', names(segdata))] %>% pp(paraname = 'weights for b-values')
        seg_ot2[7] <- paste(seg_ot2_b1, seg_ot2_b2, seg_ot2_b3, sep = '\r\n')
      } else {
        seg_ot2[7] <- seg_ot2_b1
      }
      
      #number of activity rate 
      seg_ot2_ar1 <- segdata[, grep('^number of Act. Rates|number of activity rates', names(segdata))] %>% pp(paraname = 'number of Act. Rates')
      if (as.numeric(substr(seg_ot2_ar1, 0, 2)) == 3){
        seg_ot2_ar2 <- segdata[, grep('mLH b_value-std', names(segdata))] %>% pp(paraname = 'mLH b_value-std, act rate (N(m0=5.0)), wts')
        seg_ot2_ar3 <- segdata[, grep('^mLH b_value,|mLH b_value\\s{1,}', names(segdata))] %>% pp(paraname = 'mLH b_value    , act rate (N(m0=5.0)), wts')
        seg_ot2_ar4 <- segdata[, grep('mLH b_value\\+std', names(segdata))] %>% pp(paraname = 'mLH b_value+std, act rate (N(m0=5.0)), wts')
        seg_ot2[8] <- paste(seg_ot2_ar1, seg_ot2_ar2, seg_ot2_ar3, seg_ot2_ar4, sep = '\r\n')
      } else if (as.numeric(substr(seg_ot2_ar1, 0, 2)) == 1){
        seg_ot2_ar2 <- segdata[, grep('^mLH b_value,|mLH b_value\\s{1,}', names(segdata))] %>% pp(paraname = 'mLH b_value    , act rate (N(m0=5.0)), wts')
        seg_ot2[8] <- paste(seg_ot2_ar1, seg_ot2_ar2, sep = '\r\n')
      } else if (as.numeric(substr(seg_ot2_ar1, 0, 2)) == 0) {
        seg_ot2[8] <- seg_ot2_ar1
      }
      
      #weights for sr, activity rate, rec int, MoRate branches
      seg_ot2[9] <- segdata[, grep('weights for sr', names(segdata))] %>% pp(paraname = 'weights for sr, activity rate, rec int, MoRate branches')
      
      #number of slip rate/vertical rate 
      seg_ot2_sr1 <- segdata[, grep('^number of slip-rates|^number of vertical rates', names(segdata))] %>% 
        pp(paraname = names(segdata)[grep('^number of slip-rates|^number of vertical rates', names(segdata))])
      if (as.numeric(substr(seg_ot2_sr1, 0, 2)) > 0){
        srvr <- segdata[, grep('^slip-rates|^slip rates|^vertical rates', names(segdata))]
        srvr <- word(srvr, sep = fixed(' '))
        if (srvr != -999){
          seg_ot2_sr2 <- segdata[, grep('^slip-rates|^slip rates', names(segdata))] %>% 
            pp(paraname = names(segdata)[grep('^slip-rates|^slip rates', names(segdata))])
          seg_ot2_sr3 <- segdata[, grep('^weights for slip rates', names(segdata))] %>% 
            pp(paraname = names(segdata)[grep('^weights for slip rates', names(segdata))])
          seg_ot2[10] <- paste(seg_ot2_sr1, seg_ot2_sr2, seg_ot2_sr3, sep = '\r\n')
        } else if (srvr == -999){
          seg_ot2_sr2 <- segdata[, grep('^vertical rates \\(mm/yr\\)|^slip-rates|^slip rates', names(segdata))] %>% 
            pp(paraname = names(segdata)[grep('^vertical rates \\(mm/yr\\)|^slip-rates|^slip rates', names(segdata))])
          seg_ot2_sr3 <- segdata[, grep('^rake for each slip rate', names(segdata))] %>% 
            pp(paraname = names(segdata)[grep('^rake for each slip rate', names(segdata))])
          seg_ot2_sr4 <- segdata[, grep('^weights for slip rates', names(segdata))] %>% 
            pp(paraname = names(segdata)[grep('^weights for slip rates', names(segdata))])
          seg_ot2[10] <- paste(seg_ot2_sr1, seg_ot2_sr2, seg_ot2_sr3, seg_ot2_sr4, sep = '\r\n')
        }
      } else if(as.numeric(substr(seg_ot2_sr1, 0, 2)) == 0){
        seg_ot2[10] <- seg_ot2_sr1
      }
      
      #nRecint 
      seg_ot2_ri1 <- segdata[, grep('^number of rec', names(segdata))] %>% pp(paraname = 'number of rec int')
      if (as.numeric(substr(seg_ot2_ri1, 0, 2)) > 0){
        ri1 <- segdata[, grep('^recint', names(segdata))] %>% 
          pp(paraname = names(segdata)[grep('^rmoRate Depth', names(segdata))])
        ri2 <- segdata[, grep('^weights for recint|^wts for recint|^wt_recint', names(segdata))] %>% 
          pp(paraname = names(segdata)[grep('^weights for moRate|w^eights for MoRate|^wts for moRate|^wts for Morate', names(segdata))])
        
        ri <- cbind(ri1, ri2) %>% t() %>% as.vector()
        seg_ot2_ri2 <- combine_words(ri, sep = '\r\n', and = '')
        
        seg_ot2[11] <- paste(seg_ot2_ri1, seg_ot2_ri2, sep = '\r\n')
      } else if(as.numeric(substr(seg_ot2_ri1, 0, 2)) == 0){
        seg_ot2[11] <- seg_ot2_ri1
      }
      
      #moment rates
      seg_ot2_mr1 <- segdata[, grep('^number of moment rates|^number MoRates', names(segdata))] %>% pp(paraname = 'number of moment rates')
      if (as.numeric(substr(seg_ot2_mr1, 0, 2)) > 0){
        mr1 <- segdata[, grep('^moRate', names(segdata))] %>% 
          pp(paraname = names(segdata)[grep('^moRate', names(segdata))])
        mr2 <- segdata[, grep('^moRate Depth', names(segdata))] %>% 
          pp(paraname = names(segdata)[grep('^rmoRate Depth', names(segdata))])
        mr3 <- segdata[, grep('^weights for moRate|^weights for MoRate|^wts for moRate|^wts for Morate|^wt_MoRate', names(segdata))] %>% 
          pp(paraname = names(segdata)[grep('^weights for moRate|^weights for MoRate|^wts for moRate|^wts for Morate', names(segdata))])
        
        mr <- cbind(mr1, mr2, mr3) %>% t() %>% as.vector()
        seg_ot2_mr2 <- combine_words(mr, sep = '\r\n', and = '')
        
        seg_ot2[12] <- paste(seg_ot2_mr1, seg_ot2_mr2, sep = '\r\n')
      } else if(as.numeric(substr(seg_ot2_mr1, 0, 2)) == 0){
        seg_ot2[12] <- seg_ot2_mr1
      }
      
      #nRecur 
      seg_ot2_nrecur1 <- segdata[, grep('^nRecur', names(segdata))] %>% pp(paraname = 'nRecur')
      seg_ot2_nrecur2 <- segdata[, grep('^recur model', names(segdata))] %>% pp(paraname = 'Recur model (0=Char, 1=exp, 3=max mag,6=BC Hydro Char)')
      seg_ot2_nrecur3 <- segdata[, grep('^char mag recurrence', names(segdata))] %>% pp(paraname = 'char mag recurrence, exponential mag recurrence weights')
      if (as.numeric(substr(seg_ot2_nrecur1, 0, 2)) > 0){
        seg_ot2_nrecur4 <- segdata[, grep('^delta_M1', names(segdata))] %>% pp(paraname = names(segdata)[grep('^delta_M1', names(segdata))])
        seg_ot2_nrecur4 <- combine_words(seg_ot2_nrecur4, sep = '\r\n', and = '\r\n')
        seg_ot2[13] <- paste(seg_ot2_nrecur1, seg_ot2_nrecur2, seg_ot2_nrecur3, seg_ot2_nrecur4, sep = '\r\n')
      } else if(as.numeric(substr(seg_ot2_nrecur1, 0, 2)) == 0){
        seg_ot2[13] <- paste(seg_ot2_nrecur1, seg_ot2_nrecur2, seg_ot2_nrecur3, sep = '\r\n')
      }
      
      #nThick 
      if (stype == 5){  #if source type == 5, skip nTHick, fault thickness, wts for fault thickness
        seg_ot2_nthick4 <- segdata[, grep('^depth pdf model', names(segdata))] %>% pp(paraname = 'depth pdf model and parameters')
        
        th1 <- segdata[, grep('^number of maximum magnitudes', names(segdata))] %>% 
          pp(paraname = names(segdata)[grep('^number of maximum magnitudes', names(segdata))])
        th2 <- segdata[, grep('^reference magnitude|^maximum magnitudes', names(segdata))] %>% 
          pp(paraname = names(segdata)[grep('^reference magnitude', names(segdata))])
        th3 <- segdata[, grep('^weights for max mag', names(segdata))] %>% 
          pp(paraname = names(segdata)[grep('^weights for max mag', names(segdata))])
        
        nthick <- cbind(th1, th2, th3) %>% t() %>% as.vector()
        seg_ot2_nthick5 <- combine_words(nthick, sep = '\r\n', and = '')
        
        seg_ot2[14] <- paste(seg_ot2_nthick4, seg_ot2_nthick5, sep = '\r\n')
        
      } else {
        seg_ot2_nthick1 <- segdata[, grep('^nTHick|number of fault thickness|number of fault widths', names(segdata))] %>% pp(paraname = 'nTHick')
        seg_ot2_nthick2 <- segdata[, grep('^fault thickness$|^thickness$|^fault width', names(segdata))] %>% pp(paraname = 'thickness')
        seg_ot2_nthick3 <- segdata[, grep('^thickness weighting|weights for fault thickness|weights for fault widths', names(segdata))] %>% pp(paraname = 'thickness weighting')
        seg_ot2_nthick4 <- segdata[, grep('^depth pdf model', names(segdata))] %>% pp(paraname = 'depth pdf model and parameters')
        if (as.numeric(substr(seg_ot2_nthick1, 0, 2)) > 0){
          th1 <- segdata[, grep('^number of maximum magnitudes', names(segdata))] %>% 
            pp(paraname = names(segdata)[grep('^number of maximum magnitudes', names(segdata))])
          th2 <- segdata[, grep('^reference magnitude|^maximum magnitudes', names(segdata))] %>% 
            pp(paraname = names(segdata)[grep('^reference magnitude', names(segdata))])
          th3 <- segdata[, grep('^weights for max mag', names(segdata))] %>% 
            pp(paraname = names(segdata)[grep('^weights for max mag', names(segdata))])
          
          nthick <- cbind(th1, th2, th3) %>% t() %>% as.vector()
          seg_ot2_nthick5 <- combine_words(nthick, sep = '\r\n', and = '')
          
          seg_ot2[14] <- paste(seg_ot2_nthick1, seg_ot2_nthick2, seg_ot2_nthick3, seg_ot2_nthick4, seg_ot2_nthick5, sep = '\r\n')
        } else if(as.numeric(substr(seg_ot2_nthick1, 0, 2)) == 0){
          seg_ot2[14] <- paste(seg_ot2_nthick1, seg_ot2_nthick2, seg_ot2_nthick3, seg_ot2_nthick4, sep = '\r\n')
        }
      }
      
      seg_ot2[15] <- segdata[, grep('^minmag, magstep, hxStep',names(segdata))] %>% pp(paraname = 'minmag, magstep, hxStep, hzStep, nRupArea, nRupWidth, minDepth')
      seg_ot2[16] <- segdata[, grep('^rupArea',names(segdata))] %>% pp(paraname = 'rupArea: a, b, sigma in log10 units. W&C all')
      seg_ot2[17] <- segdata[, grep('^rupWidth',names(segdata))] %>% pp(paraname = 'rupWidth: a, b, sigma in log10 units. W&C all')
      
      #number of Fault Mechanism Models
      seg_ot2_fm1 <- segdata[, grep('^number of Fault Mechanism Models', names(segdata))] %>% pp(paraname = 'number of Fault Mechanism Models')
      if (as.numeric(substr(seg_ot2_fm1, 0, 2)) > 0){
        fm1 <- segdata[, grep('^fault Mech Model Wt', names(segdata))] %>% 
          pp(paraname = names(segdata)[grep('^fault Mech Model Wt', names(segdata))])
        fm2 <- segdata[, grep('^number of mechanisms', names(segdata))] %>% 
          pp(paraname = names(segdata)[grep('^number of mechanisms', names(segdata))])
        fm3 <- segdata[, grep('^ftype', names(segdata))] %>% 
          pp(paraname = names(segdata)[grep('^ftype', names(segdata))])
        fm4 <- segdata[, grep('^wts', names(segdata))] %>% 
          pp(paraname = names(segdata)[grep('^wts', names(segdata))])
        
        fm <- cbind(fm1, fm2, fm3, fm4) %>% t() %>% as.vector()
        seg_ot2_fm2 <- combine_words(fm, sep = '\r\n', and = '')
        
        seg_ot2[18] <- paste(seg_ot2_fm1, seg_ot2_fm2, sep = '\r\n')
      } else if (as.numeric(substr(seg_ot2_fm1, 0, 2)) ==  0){
        seg_ot2[18] <- seg_ot2_fm1
      }
      
      seg_ot2 <- seg_ot2 %>% data.frame()
      rm(list = ls(pattern="seg_ot2_"))
      
      ifelse(j == 1,
             seg_ot2all <- seg_ot2,
             seg_ot2all <- rbind(seg_ot2all, seg_ot2))
      
    }
    
    
    ot1ot2 <- rbind(seg_ot, seg_ot2all)
    ot1ot2 <- ot1ot2[rowSums(is.na(ot1ot2)) != ncol(ot1ot2), ] %>% data.frame()
    allall <- rbind(allall, ot1ot2) %>% as.vector()
    
  }
  
  unlink(outputname)
  write.table(allall, file = outputname, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "", append = TRUE) 
  
  return(allall)
}