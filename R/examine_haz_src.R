#' @title Examine the FORTRAN file matches the format or not
#'
#' @import knitr
#' @import stringi
#' @import stringr
#' @import utils
#' @importFrom Hmisc all.is.numeric
#' @param fault.src character: File name of 'fault.src'
#' @param fault_output character: New file name of 'fault.src'
#' 
#' @export
#'
#' @return new FORTRAN file of \code{fault.src}
#' 
examine_haz_src <- function(fault.src, fault_output){
  if (file.exists(fault_output)) warning('File exists. Please set a new outputname.')
  
  f1 <- scan(fault.src, what = 'c', sep = '\n', allowEscapes = TRUE, quiet = TRUE, fileEncoding = 'BIG5') 
  f1 <- sub("^\t{1,}|^\\s{1,}|\t{1,}$|\\s{1,}$", "", as.character(as.factor(f1)))
  f1 <- sub('\\s+([A-Za-z])|\\s+\\(','\t\t\t\t\t\\1', f1)
  newf1 <- as.character()
  for (j in 1:length(f1)){
    if (grepl('\t{2,}', f1[j]) == FALSE){
      newf1[j] <- gsub(" {4,}", "\t\t", as.character(as.factor(f1[j])))
    } else {
      newf1[j] <- f1[j]
    }
  }
  
  #minmag, magstep, hxStep, hzStep, nRupArea, nRupWidth, minDepth
  for (n in grep('minmag, magstep, hxStep', newf1)){
    aa <- newf1[n] 
    aa <- gsub('minmag|\\(minmag','\t\t\tminmag', as.character(aa))
    aa_n <- as.character(word(aa, -1, sep = fixed('\t\t\t')))
    
    aa_v <- as.character(word(aa, sep = fixed('\t\t\t')))
    vk <- strsplit(aa_v,' |\t{1,}') %>% unlist()
    vk <- vk[nchar(vk) != 0]
    aa_vk <- combine_words(vk[1:7], sep = " ", and = '')
    newf1[n] <- paste(aa_vk, aa_n, sep = '\t\t\t')
  }
  
  #rupWidth
  for (n in grep('rupWidth', newf1)){
    aa <- newf1[n] 
    aa <- gsub('rupWidth|\\(rupWidth','\t\t\t(rupWidth', as.character(aa))
    aa_n <- as.character(word(aa, -1, sep = fixed('\t\t\t')))
    
    aa_v <- as.character(word(aa, sep = fixed('\t\t\t')))
    vk <- strsplit(aa_v,' |\t{1,}') %>% unlist()
    vk <- vk[nchar(vk) != 0]
    aa_vk <- combine_words(vk[1:3], sep = " ", and = '')
    newf1[n] <- paste(aa_vk, aa_n, sep = '\t\t\t')
  }
  
  #b-value
  for (n in grep('number of b-value|Number of b-value', newf1)){
    aa <- newf1[n] 
    if (as.numeric(str_sub(aa, 0, 1)) != 0){
      aa_v1 <- as.character(word(newf1[n + 1], sep = fixed('\t\t\t')))
      vk <- strsplit(aa_v1,' |\t{1,}') %>% unlist()
      vk <- vk[nchar(vk) != 0]
      aa_vk <- combine_words(vk, sep = " ", and = '')
      newf1[n + 1] <- paste(aa_vk, 'b-values', sep = '\t\t\t\t' )
      newf1[n + 2] <- paste(aa_vk, 'weights for b-values', sep = '\t\t\t\t' )
    } 
  }
  
  #rupArea
  for (n in grep('rupArea', newf1)){
    aa <- newf1[n] 
    aa <- gsub('rupArea|\\(rupArea','\t\t\t(rupArea', as.character(aa))
    aa_n <- as.character(word(aa, -1, sep = fixed('\t\t\t')))
    
    aa_v <- as.character(word(aa, sep = fixed('\t\t\t')))
    vk <- strsplit(aa_v,' |\t{1,}') %>% unlist()
    vk <- vk[nchar(vk) != 0]
    aa_vk <- combine_words(vk[1:3], sep = " ", and = '')
    newf1[n] <- paste(aa_vk, aa_n, sep = '\t\t\t')
  }
  
  #check Act. Rates
  for (n in grep('number of Act. Rates|Number of Act. Rates|Number of activity rates|number of activity rates', newf1)){
    aa <- newf1[n] 
    if (as.numeric(str_sub(aa, 0, 1)) == 1){
      aa_v1 <- as.character(word(newf1[n + 1], sep = fixed('\t\t\t')))
      vk <- strsplit(aa_v1,' |\t{1,}') %>% unlist()
      vk <- vk[nchar(vk) != 0]
      aa_vk <- combine_words(vk[1:3], sep = " ", and = '')
      newf1[n + 1] <- paste(aa_vk, 'MLH b_value	, act rate, wts', sep = '\t\t\t\t' )
    } else if (as.numeric(str_sub(aa, 0, 1)) == 3){
      for (p in 1:3){
        aa_v1 <- as.character(word(newf1[n + p], sep = fixed('\t\t\t\t')))
        vk <- strsplit(aa_v1,' |\t{1,}') %>% unlist()
        vk <- vk[nchar(vk) != 0]
        aa_vk <- combine_words(vk[1:3], sep = " ", and = '')
        switch(p,
               newf1[n + 1] <- paste(aa_vk, 'MLH b_value-std, act rate, wts', sep = '\t\t\t\t' ),
               newf1[n + 2] <- paste(aa_vk, 'MLH b_value	, act rate, wts', sep = '\t\t\t\t' ),
               newf1[n + 3] <- paste(aa_vk, 'MLH b_value+std, act rate, wts', sep = '\t\t\t\t' ))
      }
    }
  }
  
  for (n in grep('number of maximum magnitudes|Number of maximum magnitudes', newf1)){
    aa <- newf1[n] 
    for (p in 1:2){
      aa_v1 <- as.character(word(newf1[n + p], sep = fixed('\t\t\t\t')))
      vk <- strsplit(aa_v1,' |\t{1,}') %>% unlist()
      vk <- vk[nchar(vk) != 0]
      aa_vk <- combine_words(vk , sep = " ", and = '')
      switch(p,
             newf1[n + 1] <- paste(aa_vk, 'maximum magnitudes ', sep = '\t\t\t\t' ),
             newf1[n + 2] <- paste(aa_vk, 'weights for max mag', sep = '\t\t\t\t' ))
      }
    }
  

  newf1 <- sub('\t\t','\t\t\t', as.character(newf1))
  
  nfvalue <- as.character(word(newf1, sep = fixed('\t\t\t'))) 
  nfvalue <- sub("^\t{1,}|^\\s{1,}", "", as.character(as.factor(nfvalue)))
  
  nfname <- as.character(word(newf1, -1, sep = fixed('\t\t\t')))
  nfname <- sub("^\t{1,}|^\\s{1,}", "", as.character(as.factor(nfname)))
  
  nflt <- strsplit(nfvalue[2], split = ' ') %>% unlist() 
  nflt <- nflt[1] %>% as.numeric() 
  
  if ((nflt != grep("@@", nfvalue)) || (length(grep("@@", nfvalue)) == 0)){
    probact <- grep("prob Activity|prob activity|Prob activity|Prob Activity", nfname)
    fauname <- nfvalue[probact - 1] %>% as.character() 
    fseq <- probact - 1
    for (k in 1:length(fauname)){
      if (grepl('@@', fauname[k]) == FALSE){
        nfvalue[fseq[k]] <- paste0('@@', fauname[k])
      } 
    }
  }
  nff <- cbind(nfvalue, nfname)
  letters_only <- function(x) grepl("[A-Za-z]", x)

  
  #compare two string
  for (m in 1:nrow(nff)){ 
    if (stri_compare(nff[m, 1], nff[m, 2]) == 0){ #0 = same/ -1 = different
      same_column <- nff[m, 1]
      same_column <- strsplit(same_column, '  |\t{2,}') %>% unlist()
      
      if (length(same_column) > 1){
        if (any(letters_only(same_column) == TRUE)){
          if (all.is.numeric(same_column) == TRUE){
            #situation1: 'num char' -> 'num' 'char'
            nff[m, 1] <- combine_words(head(same_column, -1), sep = "  ", and = '  ')
            nff[m, 2] <- tail(same_column, 1)
          } else if (any(grepl('[A-Za-z]',same_column) == TRUE)) {
            #siyuation2: 'num' 'char' -> 'num' 'char'
            nff[m, 1] <- combine_words(same_column[grepl('[A-Za-z]',same_column) == FALSE], sep = "  ", and = '  ')
            nff[m, 2] <- same_column[grepl('[A-Za-z]',same_column) == TRUE]
          } 
        } else {
          #lack of param name
          nff[m, 1] <- combine_words(same_column, sep = " ", and = '')
          message_return_1 <- paste0('Line ', m, ' have some problems. Value : ', nff[m, 1],' lack its name.')
          message_return_2 <- paste('The previous 5 lines are : ', combine_words(f1[(m - 5):(m - 1)], sep = '\n', and = ''), sep = '\n')
          cat(paste(message_return_1, message_return_2, sep = '\n'))
          nff[m, 2] <- readline('This parameter is : ')
        }
      } else { 
        same_column <- strsplit(nff[m, 1], '  |\\s{1,}') %>% unlist()
        if (length(same_column) == 2){
          #lon/lat
          nff[m, 1] <- nff[m, 1]
          nff[m, 2] <- nff[m, 2]
        } else if (any(grepl('\\D{1,}', same_column)) == TRUE & (all.is.numeric(same_column) == FALSE)){
          #fault/segment name
          nff[m, 1] <- nff[m, 1]
          nff[m, 2] <- nff[m, 2]
        } else {
          #lack of param name
          nff[m, 1] <- combine_words(same_column, sep = " ", and = '')
          message_return_1 <- paste0('Line ', m, ' have some problems. Value : ', nff[m, 1],' lack its name.')
          if (m > 5){
            message_return_2 <- paste('The previous 5 lines are : ', combine_words(f1[(m - 5):(m - 1)], sep = '\n', and = ''), sep = '\n')
          } else {
            message_return_2 <- paste('The previous lines are : ', combine_words(f1[1:(m - 1)], sep = '\n', and = ''), sep = '\n')
          }
          cat(paste(message_return_1, message_return_2, sep = '\n'))
          nff[m, 2] <- readline('This parameter is : ')
        }

      } 
    } 
  }

  nff_all <- as.character()
  for (m in 1:nrow(nff)){
    if (stri_compare(nff[m, 1], nff[m, 2]) == 0){
      nff_all[m] <- paste(nff[m, 1])
    } else if (stri_compare(nff[m, 1], nff[m, 2]) != 0){
      ifelse(grepl('^[0-9]+|^\\.', nff[m, 2]) == TRUE,
             nff_all[m] <- paste(nff[m, 1], nff[m, 2], sep = '\t'),
             nff_all[m] <- paste(nff[m, 1], nff[m, 2], sep = '\t\t\t\t'))
    } 
  }
  write.table(nff_all, file = fault_output, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "", append = TRUE)
}



