#' @title Plot map of all fault segments
#'
#' @importFrom raster raster
#' @param faultfile list: result of \code{\link{read_fault}}
#' @export
#'
#' @return leaflet map


plot_all_map<-function(faultfile){
  n<-20 ; palette <- distinctColorPalette(n)

  map<-leaflet() %>% addTiles()

  gg.2<-faultfile

  for (i in c(1:(length(gg.2)))){
    for (j in c(1:(length(gg.2[[i]])))){
      if (gg.2[[i]][[j]][[2]][[1]]==1|gg.2[[i]][[j]][[2]][[1]]==2){ #type = 1|2
        m <- data.matrix(gg.2[[i]][[j]][[length(gg.2[[i]][[j]])]])
      } else if(gg.2[[i]][[j]][[2]][[1]]==3|gg.2[[i]][[j]][[2]][[1]]==4){
        dfr_lon<-gg.2[[i]][[j]][[5]][[3]][[1]] %>% as.character()%>%as.numeric()
        dfr_lat<-gg.2[[i]][[j]][[5]][[3]][[2]] %>% as.character()%>%as.numeric()
        dfr <- cbind(dfr_lon,dfr_lat)%>%data.frame()
        s = SpatialPixelsDataFrame(dfr, data = dfr)
        proj4string(s) = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
        s = raster(s)
      } else if(gg.2[[i]][[j]][[2]][[1]]==5|gg.2[[i]][[j]][[2]][[1]]==-5){#type=5
        g <- gg.2[[i]][[j]]$'npts_lonlat'%>%data.frame()
        coco <-colorNumeric(c("green", "blue"), 1:length(gg.2[[i]]))
      }
      ###plot map##
      if (gg.2[[i]][[j]][[2]][[1]]==1){ #type = 1
        map <- addPolylines(map, data= m, color='blue', popup = paste(sep = "<br/>", gg.2[[i]][[j]][[1]]), group='Type1')
      } else if(gg.2[[i]][[j]][[2]][[1]]==2){ #type = 2
        map <- addPolygons(map, data = m, color = 'red', popup = paste(sep = "<br/>", gg.2[[i]][[j]][[1]]), fill=FALSE, group='Type2')
      } else if(gg.2[[i]][[j]][[2]][[1]]==3|gg.2[[i]][[j]][[2]][[1]]==4){ #type = 3|4
        map <- addRasterImage(map, s, colors = palette[i], opacity = 0.3, group='Type3 | Type4' ) %>%
          addMarkers(mean(dfr[[1]]), mean(dfr[[2]]), label = paste(gg.2[[i]][[j]][[1]]), group='Type3 | Type4')
      } else if(gg.2[[i]][[j]][[2]][[1]]==5|gg.2[[i]][[j]][[2]][[1]]==-5){#type=5
        for (group in unique(g$depth)){
          df <- g[g$depth==group,]
          map <- addPolylines(map, data=df, lng=~lon, lat=~lat, color=coco(j),
                              label = paste0(gg.2[[i]][[j]][[1]],',','Depth: ', df$depth,' km'), group='Type5 | Type-5')
        }
      }
    }

    map <- addLayersControl(map,
                            overlayGroups = c("Type1", "Type2", "Type3 | Type4", 'Type5 | Type-5'),
                            options = layersControlOptions(collapsed = FALSE) )
  }
  return(map)
}
