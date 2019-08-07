#' @title Table output
#' @description Table of all fault segements,including Fault Name, probAct0, nSeg Model, segWt, nFlt2 and Fault ID.
#'
#' @import formattable
#' @param path character: File path. Including all of additional file.
#' @param faultfile character: File name of 'fault.src'
#'
#' @return FName1 : Fault Name.
#' @return probAct0 : Probability of activity.
#' @return nSeg Model : Number of segment model.
#' @return segWt : Weights for each segment model.
#' @return nFlt2 : Number of segment.
#' @return Fault ID : ID number for each fault. This value will be use in \code{output_table}

output_fault_table<-function(path, faultfile){
  setwd(path)
  f1 <- scan(faultfile, what = 'c', sep = '\n', allowEscapes = TRUE, quiet = TRUE) %>% data.frame()
  fvalue <- as.character(word(f1[, 1], sep = fixed('\t\t'))) %>% data.frame()
  fvalue <- as.character(gsub("\t{1,}"," ",fvalue[,1]),1) %>% data.frame()
  ff13 <- grep("@@", as.character(as.matrix.data.frame(fvalue))) %>% data.frame()

  fname <- as.character(word(f1[, 1], -1, sep = fixed('\t\t'))) %>% data.frame()
  fname <- as.character(gsub("\t{1,}"," ",fname[,1]),1) %>% data.frame()
  fname <- as.character(gsub(" ,",",",fname[,1]),1) %>% data.frame()

  all<-lapply(1:nrow(ff13), function(i){
    gg = list();ggname=list()
    if (i < (dim(ff13)[1])) {
      gg <- fvalue[ (ff13[i,1]) : (ff13[i+1,1]-1), 1]
      ggname <- fname[ (ff13[i,1]) : (ff13[i+1,1]-1), 1]
    } else if (i == (dim(ff13)[1])) {
      gg<- fvalue[ (ff13[i,1]) : (dim(fvalue)[1]), 1]
      ggname <- fname[ (ff13[i,1]) : (dim(fvalue)[1]), 1]
    }
    return(list(ggname,gg))
  })

  gg.1<-data.frame()
  for (i in c(1:length(all))){
    for (j in c(1:4)){
      gg.1[i,j] <- all[[i]][[2]][[j]] }
    gg.1[i,5] <- as.character(word(all[[i]][[2]][[5]], 1, sep = fixed(' ')))
    gg.1[i,6]<-i
  }

  colnames(gg.1)<-c('FName1','probAct0','nSeg Model','segWt','nFlt2','Fault ID')
  return(gg.1)
}
