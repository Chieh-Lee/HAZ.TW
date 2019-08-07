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
  f1 <- scan(faultfile,what = 'c',sep = '\n',allowEscapes = TRUE,quiet = TRUE) %>% data.frame()
  fvalue <- as.character(word(f1[, 1], sep = fixed('\t\t'))) %>% data.frame()
  fvalue <- as.character(gsub("\t{1,}"," ",fvalue[,1]),1) %>% data.frame()
  segname <- grep("@@", as.character(as.matrix.data.frame(fvalue))) %>% data.frame()

  fname <- as.character(word(f1[, 1], -1, sep = fixed('\t\t'))) %>% data.frame()
  fname <- as.character(gsub("\t{1,}"," ",fname[,1]),1) %>% data.frame()
  fname <- as.character(gsub(" ,",",",fname[,1]),1) %>% data.frame()

  all<-lapply(1:nrow(segname), function(i){
    gg = list();ggname=list()
    if (i < (dim(segname)[1])) { gg <- fvalue[ (segname[i,1]) : (segname[i+1,1]-1), 1]
    ggname <- fname[ (segname[i,1]) : (segname[i+1,1]-1), 1]}
    else if (i == (dim(segname)[1])) { gg<- fvalue[ (segname[i,1]) : (dim(fvalue)[1]), 1]
    ggname <- fname[ (segname[i,1]) : (dim(fvalue)[1]), 1] }
    return(list(ggname,gg))
  })

  gg.2<-lapply(1:length(all),function(i){
    no_seg <- grep('^[A-Z]+', all[[i]][[2]])%>% data.frame()
    lapply(1:nrow(no_seg),function(k){
      if (k < nrow(no_seg)) {
        ggvalue.3 <- data.frame(all[[i]][[2]])[(no_seg[k,1]) : (no_seg[k+1,1]-1), 1]
        ggname.3 <- data.frame(all[[i]][[1]])[(no_seg[k,1]) : (no_seg[k+1,1]-1), 1] %>% as.character()
        ca<-sapply(1:length(ggvalue.3),function(x){
          ifelse(x %in% grep('^[A-Za-z]+', ggvalue.3),
                 ca<-strsplit(as.character(ggvalue.3[x]), "\\s") ,
                 ca<-suppressWarnings(as.numeric(unlist(strsplit(as.character(ggvalue.3[x]), "\\s")))) %>% na.omit() %>% list())
        })
        ggname.3[1]<-'fName';ca<-setNames(ca,ggname.3)
        if (ca[[2]][[1]]==1|ca[[2]][[1]]==2){ #source_type=1|2
          if(length(ca[[6]])<3){
            c1<-ca[c((5+1):(5+ca[[5]]))] %>% unlist()
          }else{
            c1<-ca[c((5+1):(5+ca[[5]]))] %>% data.frame()
            c1<-c1[-3,]%>% unlist()
          }
          ca[[length(ca)+1]] <- matrix(c1,(length(c1)/2),2,byrow = TRUE) %>% data.frame()
          ca[c((5+1):(5+ca[[5]]))]<-NULL #remove lonlatpts
          names(ca)[[length(ca)]] <- "npts_lonlat"

        } else if ( ca[[2]][[1]] == 3 | ca[[2]][[1]] == 4) { #source_type=3|4
          ca[[5]] <- as.character(ca[[5]])
          #Dsource<-paste0(path,"/",ca[[5]])
          Data1 <- scan(ca[[5]],what = 'c',sep = '\n',allowEscapes = TRUE,quiet = TRUE)
          D1_1 <- Data1[1:3] #information
          D1_2 <- Data1[4:length(Data1)];D1_2 <- unlist(str_split(D1_2," ")) #location
          D1_3 <- matrix(D1_2,(length(D1_2)/3),3,byrow = TRUE) %>% data.frame()
          ca[[5]]<-list(as.character(ca[[5]]),D1_1,D1_3)

        } else if ( ca[[2]][[1]] == 5 | ca[[2]][[1]] == -5) { #source_type=5
          cc<-matrix(unlist(ca[c((4+1)):(4+ca[[4]][[2]])]),3*ca[[4]][[1]],ca[[4]][[2]])%>%data.frame() #create lonlatpts df #ca[[5]]==npts
          ca[[length(ca)+1]] <- matrix(unlist(cc),(ca[[4]][[1]]*ca[[4]][[2]]),3,byrow = TRUE)%>%data.frame()
          ca[[length(ca)]][,4]<-rep(1:(ca[[4]][[2]]),each=(ca[[4]][[1]]))
          ca[[length(ca)]][,3]<-ca[[length(ca)]][,3]*(-1) #make depth to nagative
          colnames(ca[[length(ca)]])<-c('lon','lat','depth','group')
          ca[c((4+1)):(4+ca[[4]][[2]])]<-NULL #remove lonlatpts
          names(ca)[[length(ca)]] <- "npts_lonlat"
        }

      } else if (k == nrow(no_seg)) {
        ggvalue.3 <- data.frame(all[[i]][[2]])[(no_seg[k,1]) : length(all[[i]][[2]]), 1]
        ggname.3 <- data.frame(all[[i]][[1]])[(no_seg[k,1]) : length(all[[i]][[2]]), 1]%>% as.character()
        ca<-sapply(1:length(ggvalue.3),function(x){
          ifelse(x %in% grep('^[A-Za-z]+', ggvalue.3),
                 ca<-strsplit(as.character(ggvalue.3[x]), "\\s") ,
                 ca<-suppressWarnings(as.numeric(unlist(strsplit(as.character(ggvalue.3[x]), "\\s")))) %>% na.omit() %>% list())
        })
        ggname.3[1]<-'fName';ca<-setNames(ca,ggname.3)
        if (ca[[2]][[1]] == 1 | ca[[2]][[1]] == 2){ #source_type=1|2
          if(length(ca[[6]])<3){
            c1<-ca[c((5+1):(5+ca[[5]]))] %>% unlist()
          }else{
            c1<-ca[c((5+1):(5+ca[[5]]))] %>% data.frame()
            c1<-c1[-3,]%>% unlist()
          }
          ca[[length(ca)+1]] <- matrix(c1,(length(c1)/2),2,byrow = TRUE) %>% data.frame()
          ca[c((5+1):(5+ca[[5]]))]<-NULL #remove lonlatpts
          names(ca)[[length(ca)]] <- "npts_lonlat"

        } else if ( ca[[2]][[1]] == 3 | ca[[2]][[1]] == 4) { #source_type=3|4
          ca[[5]] <- as.character(ca[[5]])
          Data1 <- scan(ca[[5]],what = 'c',sep = '\n',allowEscapes = TRUE,quiet = TRUE)
          D1_1 <- Data1[1:3] #information
          D1_2 <- Data1[4:length(Data1)];D1_2 <- unlist(str_split(D1_2," ")) #location
          D1_3 <- matrix(D1_2,(length(D1_2)/3),3,byrow = TRUE) %>% data.frame()
          ca[[5]]<-list(as.character(ca[[5]]),D1_1,D1_3)

        } else if ( ca[[2]][[1]] == 5 | ca[[2]][[1]] == -5) { #source_type=5
          cc<-matrix(unlist(ca[c((4+1)):(4+ca[[4]][[2]])]),3*ca[[4]][[1]],ca[[4]][[2]])%>%data.frame() #create lonlatpts df #ca[[5]]==npts
          ca[[length(ca)+1]] <- matrix(unlist(cc),(ca[[4]][[1]]*ca[[4]][[2]]),3,byrow = TRUE)%>%data.frame()
          ca[[length(ca)]][,4]<-rep(1:(ca[[4]][[2]]),each=(ca[[4]][[1]]))
          ca[[length(ca)]][,3]<-ca[[length(ca)]][,3]*(-1) #make depth to nagative
          colnames(ca[[length(ca)]])<-c('lon','lat','depth','group')
          ca[c((4+1)):(4+ca[[4]][[2]])]<-NULL #remove lonlatpts
          names(ca)[[length(ca)]] <- "npts_lonlat"
        }
      }
      return(ca)
    })

  })
}
