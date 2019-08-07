#' @title Table output
#'
#' @param path character string: File path. Including all of additional file.
#' @param faultfile character string: File name of 'fault.src'
#' @param ID integer: fault ID
#' @export
#'
#' @return table of \code{faultfile}


output_segment_table<-function(path,faultfile,ID){
  setwd(path)
  f1 <- scan(faultfile,what = 'c',sep = '\n',allowEscapes = TRUE,quiet = TRUE) %>% data.frame()
  fvalue <- as.character(word(f1[, 1], sep = fixed('\t\t'))) %>% data.frame()
  fvalue <- as.character(gsub("\t{1,}"," ",fvalue[,1]),1) %>% data.frame()
  ff13 <- grep("@@", as.character(as.matrix.data.frame(fvalue))) %>% data.frame()

  fname <- as.character(word(f1[, 1], -1, sep = fixed('\t\t'))) %>% data.frame()
  fname <- as.character(gsub("\t{1,}"," ",fname[,1]),1) %>% data.frame()
  fname <- as.character(gsub(" ,",",",fname[,1]),1) %>% data.frame()
  fname <- as.character(gsub("(^[[:space:]]+|[[:space:]]+$)","",fname[,1]),1) %>% data.frame()

  all<-lapply(1:nrow(ff13), function(i){
    gg = list();ggname=list()
    if (i < (dim(ff13)[1])) {
      gg <- fvalue[ (ff13[i,1]) : (ff13[i+1,1]-1), 1]
      ggname <- fname[ (ff13[i,1]) : (ff13[i+1,1]-1), 1]}
    else if (i == (dim(ff13)[1])) {
      gg<- fvalue[ (ff13[i,1]) : (dim(fvalue)[1]), 1]
      ggname <- fname[ (ff13[i,1]) : (dim(fvalue)[1]), 1] }
    return(list(ggname,gg))
  })

  s_value<-all[[ID]][[2]]%>%data.frame(); s_name<-all[[ID]][[1]]%>%data.frame()
  s_value<- as.character(gsub(" ","\n",s_value[ ,1]),1) %>% data.frame()
  s9df<-cbind(s_name,s_value)
  s_value_1 <- grep('^[A-Z]+', s_value[ ,1]) %>% data.frame()
  sall<-data.frame();sss<-data.frame()

  for (i in c(1:nrow(s_value_1))){
    sall<-data.frame()
    if (i == nrow(s_value_1)){
      for (j in c(1:(nrow(s9df)-s_value_1[i,1]+1))){
        sall[j,1]<-s9df[(s_value_1[i,1]+j-1),1] %>% as.character()
        sall[j,2]<-s9df[(s_value_1[i,1]+j-1),2] %>% as.character()
        sall$id <- 1:nrow(sall)
        if (length(grep('^[0-9]+', sall[ ,1]))>0){
          sall<-sall[-(grep('^[0-9]+', sall[ ,1])), ]
        } else if (length(grep('^ [0-9]+', sall[ ,1]))>0){
          sall<-sall[-(grep('^ [0-9]+', sall[ ,1])), ]
        }
      }
    } else {
      for (j in c(1:((s_value_1[i+1,1])-s_value_1[i,1]))){
        sall[j,1]<-s9df[(s_value_1[i,1]-1+j),1] %>% as.character()
        sall[j,2]<-s9df[(s_value_1[i,1]-1+j),2] %>% as.character()
        if (length(grep('^[0-9]+', sall[ ,1]))>0){
          sall<-sall[-(grep('^[0-9]+', sall[ ,1])), ]
        } else if (length(grep('^ [0-9]+', sall[ ,1]))>0){
          sall<-sall[-(grep('^ [0-9]+', sall[ ,1])), ]
        } else if (length(grep('^[a-z]+', sall[ ,2]))>0){
          sall[(grep('^[a-z]+', sall[ ,2])),1]<-'filename'
        }
      }
    }
    sall[1,1] <- 'fName'
    names(sall)[2]<-sall[1,2]
    sall<-na.omit(sall)

    k=which(sall$V1=='Number of Fault Mechanism Models')
    b<-rep(1:(sall[which(sall$V1=='Number of Fault Mechanism Models'),2]),each=4)
    q=1
    for (c in c((k+1):(k+4*as.numeric(sall[k,2])))){
      sall[c,1]<-paste0(sall[c,1],'_',b[q])
      q=q+1
    }
    if (sall[which(sall$V1=='Number of Fault Mechanism Models'),2]==1){
      sall[k+4,1]<-'wts for observation_1'
    }

    assign(paste0("sall_",i),sall)

    if (i ==1) { sss <- sall
    } else if (i == 2) {
      if (length(which(duplicated(sall$V1))) == 0){#if value not repeat,just merge
        sss <- merge(sss, sall, by= 'V1', all=TRUE)
      } else if (nrow(sss) == nrow(sall)){
        if ((length(which(sall$V1=='nTHick'))==0)||(sall[which(sall$V1=='nTHick'),2]==sss[which(sss$V1=='nTHick'),2])){
          if ((length(which(sall$V1=='Number of Fault Mechanism Models'))==0)||(sall[which(sall$V1=='Number of Fault Mechanism Models'),2]==sss[which(sss$V1=='Number of Fault Mechanism Models'),2])){
            if ((length(which(sall$V1=='Number of Act. Rates'))==0)||(sall[which(sall$V1=='Number of Act. Rates'),2]==sss[which(sss$V1=='Number of Act. Rates'),2])){
              sss <- cbind(sss, sall[-1])
            }
          }
        }
      } else {
        sss <- merge(sss, sall, by = 'V1', all=TRUE)
      }
    } else if (i >2){
      if (length(which(duplicated(sall$V1))) == 0){#if value not repeat,just merge
        sss <- merge(sss, sall, by= 'V1', all=TRUE)
      } else if (nrow(sss) == nrow(sall)){
        if ((length(which(sall$V1=='nTHick'))==0) || (sall[which(sall$V1=='nTHick'),2]==sss[which(sss$V1=='nTHick'),2])){
          if ((length(which(sall$V1=='Number of Fault Mechanism Models'))==0) || (sall[which(sall$V1=='Number of Fault Mechanism Models'),2]==sss[which(sss$V1=='Number of Fault Mechanism Models'),2])){
            if((length(which(sall$V1=='Number of Act. Rates'))==0) || (sall[which(sall$V1=='Number of Act. Rates'),2]==sss[which(sss$V1=='Number of Act. Rates'),2])){
              sss <- cbind(sss, sall[-1])
            }
          }
        }
      }
    } else { sss <- merge(sss, sall, by = 'V1', all=TRUE) }
    rm(list = ls(pattern = "^sall_"))
  }
  sss<-sss[order(sss$id), ]
  rownames(sss) <- NULL ; sss$id<-NULL
  colnames(sss)[1]<-'Parameters'
  return(sss)
}

