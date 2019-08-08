#' @title Plot map of specific fault segment
#'
#' @import leaflet
#' @import randomcoloR
#' @import sp
#'
#' @param faultfile list: result of \code{\link{read_fault}}
#' @param ID intrger: fault ID
#'
#' @export
#'
#' @return leaflet map


plot_segment<-function(faultfile,ID){
  palette <- distinctColorPalette(20)
  gg.2 <- faultfile[ID]

  map <- leaflet() %>% addTiles()
  for (i in c( 1:(length(gg.2)))){
    for (j in c( 1:(length(gg.2[[i]])))){
      fdata <- gg.2[[i]][[j]]
      stype <- gg.2[[i]][[j]][[2]][[1]]

      if (stype == 1 | stype == 2){ #type = 1|2
        m <- data.matrix(fdata[[length(fdata)]])
      } else if (stype == 3 | stype == 4){
        dfr_lon <- fdata[[5]][[3]][[1]] %>% as.character() %>% as.numeric()
        dfr_lat <- fdata[[5]][[3]][[2]] %>% as.character() %>% as.numeric()
        dfr <- cbind(dfr_lon,dfr_lat) %>% data.frame()
        s = SpatialPixelsDataFrame(dfr, data = dfr)
        proj4string(s) = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
        s = raster(s)
      } else if(stype == 5 | stype == -5){#type=5
        g <- fdata$'npts_lonlat' %>% data.frame()
        coco <- colorNumeric( c("green", "blue"), 1:length(gg.2[[i]]))
      }
      ###plot map##
      if (stype == 1){ #type = 1
        map <- addPolylines( map, data = m, color ='blue',
                             popup = paste( sep = "<br/>", fdata[[1]]), group ='Type1')
      } else if(stype == 2){ #type = 2
        map <- addPolygons( map, data = m, color = 'red',
                            popup = paste( sep = "<br/>", fdata[[1]]), fill = FALSE, group ='Type2')
      } else if(stype == 3 | stype == 4){ #type = 3|4
        map <- addRasterImage(map, s, colors = palette[i], opacity = 0.45, group ='Type3 | Type4' ) %>%
          addMarkers(mean(dfr[[1]]), mean(dfr[[2]]), label = paste(fdata[[1]]), group ='Type3 | Type4')
      } else if(stype == 5 | stype == -5){#type=5
        for (group in unique(g$depth)){
          df <- g[ g$depth == group,]
          map <- addPolylines(map, data = df, lng = ~lon, lat = ~lat, color = coco(j),
                              label = paste0( fdata[[1]], ',', 'Depth: ', df$depth, ' km'), group = 'Type5 | Type-5')
        }
      }
    }

    map <- addLayersControl(map,
                            overlayGroups = c("Type1", "Type2", "Type3 | Type4", 'Type5 | Type-5'),
                            options = layersControlOptions(collapsed = FALSE) )
  }
  return(map)
}
