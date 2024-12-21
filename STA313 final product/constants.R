library(leaflet)
library(data.table)
library(stringr)

load('dataNeeded.RData')

# Update MCI_CATEGORY: Replace "STOLEN" with "Stolen"
info$MCI_CATEGORY[info$MCI_CATEGORY == "STOLEN"] <- "Stolen"
unique(info$MCI_CATEGORY)

ditie<-subwaysite[,!'id']
qid<-c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
mci.yanse<-c('Stolen'= "#E41A1C",
'Robbery'= "#377EB8",
'Theft Over'="#4DAF4A")


poi[, who:=str_to_title(who)]

whos<-poi[,unique(who)]
mcis<-info[,unique(MCI_CATEGORY)]

timetypes1<-c(
'Midterm Exam (Feb & Oct)',
'Final Exam (Apr & Dec)')

timetypes2<-'Travel Season (Jun-Aug)'

iconscrime <- awesomeIcons(
  icon = 'bolt',
  markerColor='pink',
  iconColor = 'black',
  library = 'fa',
  text=fontawesome::fa('bolt',fill='red')
) 

iconslibrary <- awesomeIcons(
  icon = 'book',
  markerColor='white',
  iconColor = 'black',
  library = 'fa',
  text=fontawesome::fa('book',fill='orange')
) 

iconstour <- awesomeIcons(
  icon = 'tree',
  markerColor='green',
  iconColor = 'green',
  library = 'fa',
  text=fontawesome::fa('tree',fill='green')
) 

iconssubway <- awesomeIcons(
  icon = 'subway',
  markerColor='white',
  iconColor = 'purple',
  library = 'fa',
  text=fontawesome::fa('subway',fill='purple')
) 
##################

basemap<-leaflet() %>%
       setView(lat=43.7, lng=-79.42,zoom=11)%>%
	   addProviderTiles(providers$CartoDB.Positron, group = "Default") %>%
	   addTiles(group = "Terrain") %>%
	   addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
	   addLayersControl(baseGroups=c('Default','Terrain','Satellite'),
                       options = layersControlOptions(collapsed=FALSE),
	                   position='topright') %>%
 	    addAwesomeMarkers(data=info,
    	                     lng=~LONG_WGS84,lat=~LAT_WGS84,
    	                     clusterOptions = markerClusterOptions(),
    	                     label=lapply(info$lab,htmltools::HTML),
    	                     icon = iconscrime,
    	                     group='CrimeEvents') %>%
	  addAwesomeMarkers(data=poi[who!='Student'],
   	                     lng=~long,lat=~lat,
   	                     label=~where,
   	                     icon = iconstour,
   	                     group='TourSites') %>%
      addAwesomeMarkers(data=poi[who=='Student'],
  	                     lng=~long,lat=~lat,
  	                     label=~where,
  	                     icon = iconslibrary,
  	                     group='libraries')



temp<-info[(OCC_HOUR %in% c(6:8,15:19) & !OCC_DOW %in% c('Sunday','Saturday'))]

basemapP2<-leaflet() %>%
       setView(lat=43.7, lng=-79.42,zoom=11)%>%
	   addProviderTiles(providers$CartoDB.Positron, group = "Default") %>%
	   addTiles(group = "Terrain") %>%
	   addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
	   addLayersControl(baseGroups=c('Default','Terrain','Satellite'),
                       options = layersControlOptions(collapsed=FALSE),
	                   position='topright') %>%
 	    addAwesomeMarkers(data=temp,
    	                     lng=~LONG_WGS84,lat=~LAT_WGS84,
    	                     clusterOptions = markerClusterOptions(),
    	                     label=lapply(temp$lab,htmltools::HTML),
    	                     icon = iconscrime,
    	                     group='CrimeEvents') %>%
       addAwesomeMarkers(data=poi[who=='Student'],
   	                     lng=~long,lat=~lat,
   	                     label=~where,
   	                     icon = iconslibrary,
   	                     group='libraries') %>%
	  addAwesomeMarkers(data=poi[who!='Student'],
   	                     lng=~long,lat=~lat,
   	                     label=~where,
   	                     icon = iconstour,
   	                     group='TourSites') %>%
		  addAwesomeMarkers(data=ditie,
   	                     lng=~long,lat=~lat,
   	                     label=~where,
   	                     icon = iconssubway,
   	                     group='subwaySites')


