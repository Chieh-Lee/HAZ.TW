#' Read file about input.inp
#'
#' @import stringr
#' @param input.inp character string : input.inp file
#' @export
#'
#' @return a list of \code{input.inp}
#'


read_input<-function(input.inp){
  #requireNamespace("stringr")
  #library(stringr)
  c1 <- scan(input.inp,what = 'c',sep = '\n',allowEscapes = TRUE,quiet = TRUE) %>% data.frame()
  c1.2 <- as.character(word(c1[,1], sep = fixed('\t'))) %>% data.frame()
  c1.21 <- c1.2[1:22, 1] %>% data.frame()
  c1.22 <- c1.2[23:dim(c1.2)[1], 1] %>% data.frame()
  deln <- which(c1.22[,1]=="");if (length(deln) != 0) {c1.22 <- c1.22[-deln, 1] %>% data.frame()}

  iname <- as.character(word(c1[1:22,1], -1, sep = fixed('\t')))%>% data.frame()
  iname <- as.character(gsub(" {3,}","",iname[,1]),1)%>% data.frame() #space*3 to space*1

  haz=list()
  for (i in c(1:dim(c1.21)[1])){
    haz[[i]] <- data.frame(t(data.frame(str_split(c1.21[i,1],' '))))
    deln <- which(haz[[i]]==""); if (length(deln) != 0) { haz[[i]] <- haz[[i]][-deln] }
    if (i == 1) { haz[[i]] <-as.character(word(haz[[i]],2, sep = fixed('.'))) }
    else if (i == 5) { haz[[i]] <-as.character(c1.21[i,1]) }
    else{ haz[[i]] <-suppressWarnings(as.numeric(as.character(unlist(haz[[i]])))) }
    names(haz)[[i]] <- as.character(iname[i,1])
  }

  df <- data.frame(matrix(NA,(dim(c1.22)[1]/8),9)); num=1
  while (num < (dim(c1.22)[1])) {
    for (j in c(1:(dim(c1.22)[1]/8))) {
      df[j,1] = paste0('Site', j)
      cc <- as.character(word(gsub(" {1,}"," ",c1.22[num,1]),1,sep = fixed('(')))
      lonlat <- word(cc[1],1,sep=fixed('   ')); other <- word(cc[1],2,sep=fixed('     '))
      df[j,2] = as.numeric(as.character(word(cc[1],1,sep = fixed(' '))))
      df[j,3] = as.numeric(as.character(word(cc[1],2,sep = fixed(' '))))
      df[j,4] = as.numeric(as.character(word(cc[1],3,sep = fixed(' '))))
      df[j,5] = as.numeric(as.character(word(cc[1],4,sep = fixed(' '))))
      df[j,6] = as.numeric(as.character(word(cc[1],5,sep = fixed(' '))))
      df[j,7] = as.numeric(as.character(word(cc[1],6,sep = fixed(' '))))
      df[j,8] = as.numeric(as.character(word(cc[1],7,sep = fixed(' '))))
      df[j,9] = as.numeric(as.character(word(cc[1],8,sep = fixed(' '))))
      num=num+8
    }}

  colnames(df) <- c('Site','sx','sy','Vs','DVs1.0','DVs1.5','DVs2.5','Vrup','FABA');haz$site<-df

  return(haz)
}
