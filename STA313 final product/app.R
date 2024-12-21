library(data.table)
library(shiny)
library(bslib)
library(stringr)
library(shinyWidgets)
library(shinycssloaders)
library(stringr)
library(leaflet)
library(ggplot2)
library(plotly)
library(glue)
library(shinyjs)
library(shinycssloaders)

source('constants.R')


ui<-page_navbar(
useShinyjs(),  # Set up shinyjs
title="",

# New Introduction Page
nav_panel(
  title = "Introduction",
  layout_columns(
    card(
      # CSS Styling for Center Alignment and Colors
      tags$style(HTML("
          body {
            background-color: #f8f9fa; /* Light grey background */
          }
          h4, h5 {
            text-align: center;
            margin-top: 20px;
            margin-bottom: 10px;
            color: #2c3e50; /* Dark Blue */
            font-weight: bold;
          }
          p, li {
            text-align: center;
            line-height: 1.6;
            color: #555555; /* Dark grey text */
            font-size: 16px;
          }
          ul, ol {
            margin: 0 auto;
            padding-left: 0;
            list-style-position: inside;
          }
          .card {
            background-color: #ffffff; /* White card background */
            border: 1px solid #dddddd;
            box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);
          }
        ")),
      
      # Page Title
      card_header(tags$h4("Interactive Analysis of the Main Crime Types in Toronto from 2020 to 2024")),
      
      # Background Section
      tags$h4("Background"),
      tags$p("Stolen, Robbery, and Theft are important factors affecting urban safety and residents' trust. 
                Such crimes not only cause property losses but also increase safety concerns in the community, 
                affecting the quality of life and the city's image."),
      tags$p("The student population and tourists are two major groups affected by these crimes, especially 
                during specific times like exam periods or peak tourist seasons."),
      
      # Main Target Audience
      tags$h4("Main Target Audience"),
      tags$ul(
        tags$li(tags$b("Students:"), " Focus on safety during exam periods."),
        tags$li(tags$b("Tourists:"), " Be cautious during peak travel seasons.")
      ),
      
      # Filters and Functions
      tags$h4("Main Filters and Functions"),
      tags$ol(
        tags$li(tags$b("Location Filtering:"), " View crime data for specific areas or across the GTA."),
        tags$li(tags$b("Time Filtering:"), " Analyze data for selected time ranges, including exams and travel seasons.")
      ),
      
      # Visualization Tools
      tags$h4("Visualization Tools"),
      tags$h5("Page 1:"),
      tags$ul(
        tags$li(tags$b("Time Series Graph:"), " Shows dynamic changes of crime types over time."),
        tags$li(tags$b("Map:"), " Displays crime hotspots and important locations.")
      ),
      tags$h5("Page 2:"),
      tags$ul(
        tags$li(tags$b("Map:"), " Visualizes crimes around TTC."),
        tags$li(tags$b("Pie Chart:"), " Displays proportions of crime types."),
        tags$li(tags$b("Bar Chart:"), " Compares crime distributions during different time periods.")
      ),
      
      # Conclusion
      tags$h4("Conclusion"),
      tags$p("This app provides insights into theft, robbery, and grand larceny crimes in Toronto. It is designed 
                to help students, tourists, and relevant stakeholders make informed decisions to enhance urban safety.")
    )
  )
),

nav_panel(
title='Crime around UTSG libraries and attractions in Toronto',

theme=bs_theme(version='5',bootswatch='lumen') %>%
      bs_add_rules(list(".shiny-output-error { visibility: hidden; }",
                      ".shiny-output-error:before { visibility: hidden; }")),
					  
					  
layout_sidebar(
sidebar=sidebar(

switchInput(
   inputId = "locationSE",
   label = "Select Location",
onLabel='',
offLabel='',   
   labelWidth = "120px"
),

shinyjs::hidden(
div(id='siteSHOW',
pickerInput(
   inputId = "whichSite",
   label = "Which location you want to know more?", 
    choices = list(
    Student = poi[who=='Student',where],
    Tourist = poi[who=='Tourist',where]),
   options = pickerOptions(container = "body"), 
    width = "100%"
))),

prettyCheckboxGroup(
   inputId = "whichMCI",
   label = "What Crime?", 
    choices = mcis,
	selected=mcis,
	status='danger',
   icon = icon("fire"), 
   #animation = "tada"
),


prettyRadioButtons(
   inputId = "whatTimeP1",
   label = "Specific Time Peroid?", 
   choices=c('Select interested time',timetypes1,timetypes2),
   status='warning',
   icon = icon("timeline"), 
),



shinyjs::hidden(
div(id='timeP1Se',
sliderInput('whatYearP1','Year',min=2020,max=2023,value=c(2020,2023)),
sliderInput('whatMonthP1','Month',min=1,max=12,value=c(1,12)),
sliderTextInput(
   inputId = "whatdayP1",
   label = "Day of Week",
   grid = TRUE, 
   force_edges = TRUE,
   choices = qid,
   selected=c(qid[1],qid[7])
),
sliderInput('whatHourP1','Hour of Day',min=0,max=23,value=c(0,23))
)),


uiOutput('filterP1Summary')
),

#withSpinner(leafletOutput('mapP1',height=600)),
leafletOutput('mapP1',height=600),
verbatimTextOutput('checkTS'),
plotlyOutput('tsPlot',height=250)
)),  ###end page1

nav_panel(
title='Crime around the nearest subway stations to UTSG libraries and attractions in Toronto',
layout_columns(
col_widths=c(6,6),

layout_columns(
col_widths=c(12,12),
row_heights=c(1.5,1),
card(
leafletOutput('mapP2')
),
card(
navset_tab(
nav_panel(title ='Week',plotlyOutput('bar1',height=160)),
nav_panel(title ='Hour',plotlyOutput('bar2',height=160))
)
)
),##left 

layout_columns(
col_widths=c(12,12),
card(card_header('Your interested time'),
switchInput(
   inputId = "locationSEP2",
   label = "Select Location",
onLabel='',
offLabel='',   
   labelWidth = "120px"
),

shinyjs::hidden(
div(id='siteSHOWP2',
pickerInput(
   inputId = "whichSiteP2",
   label = "Which location you want to know more?", 
    choices = list(
    Student = poi[who=='Student',where],
    Tourist = poi[who=='Tourist',where]),
   options = pickerOptions(container = "body"), 
    width = "100%"
))),

prettyRadioButtons(
   inputId = "whichTimeIntervalP2",
   label = NULL, 
   choices = c('Non-peak time','Peak time','Select interested time'),
   icon = icon("timeline"),
   status='warning',
   inline=TRUE,
   selected='Peak time',
   animation = "tada",
),
uiOutput('timeAnnoUI'),

shinyjs::hidden(div(id='timeFreeUIdiv',
tagList(

sliderTextInput(
   inputId = "whatdayP2",
   label = "Week",
   grid = TRUE, 
   force_edges = TRUE,
   choices = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
   selected=c("Tue","Fri")
),


sliderInput('whatHourP2','Hour',min=0,max=23,value=c(8,18),step=1)
)
))

),
card(card_header('pie'),
plotlyOutput('pieP2'),
verbatimTextOutput('check')
)
)###right
)
)
)


server<-function(input,output,session){
####
observe({
shinyjs::toggle('timeP1Se',condition=input$whatTimeP1=='Select interested time')
shinyjs::toggle('siteSHOW',condition=input$locationSE)

shinyjs::toggle('siteSHOWP2',condition=input$locationSEP2)
shinyjs::toggle('timeFreeUIdiv',condition=input$whichTimeIntervalP2=='Select interested time')
})

###
output$mapP1<-renderLeaflet({
basemap
})

####
getFilterDataP1<-reactive({
info[MCI_CATEGORY %in% input$whichMCI,]->inter.ok

if(input$whatTimeP1=='Select interested time'){
input$whatYearP1->ytP1
ytP1<-ytP1[1]:ytP1[2]
input$whatMonthP1->mtP1
mtP1<-mtP1[1]:mtP1[2]
input$whatdayP1->dtP1
qid[match(dtP1[1],qid):match(dtP1[2],qid)]->dtP1
input$whatHourP1->htP1
htP1<-htP1[1]:htP1[2]

inter.ok[
OCC_YEAR %in% ytP1 &
mon %in% mtP1 &
OCC_HOUR %in% htP1 &
str_sub(OCC_DOW,1,3) %in% dtP1
]->inter.ok

}

if(input$whatTimeP1=='Midterm Exam (Feb & Oct)'){
inter.ok<-inter.ok[mon %in% c(2:3,10:11)]
}

if(input$whatTimeP1=='Final Exam (Apr & Dec)') {
inter.ok<-inter.ok[mon %in% c(10,12)]
}

if(input$whatTimeP1=='Travel Season (Jun-Aug)') {
inter.ok<-inter.ok[travalS==TRUE]
}


inter.ok
})

####
#output$checkTS<-renderPrint({
#inter.ok<-getFilterDataP1()
#str(inter.ok)
#})

output$tsPlot <- renderPlotly({
  inter.ok <- getFilterDataP1()
  inter.ok[, .(count = .N), .(realDate, MCI_CATEGORY)][order(realDate)] -> tsdata
  setnames(tsdata, 'realDate', 'Date')
  
  p1 <- ggplot(tsdata, aes(x = Date, y = count, group = MCI_CATEGORY, colour = MCI_CATEGORY, 
                           text = paste0("Date: ", Date, 
                                         "<br>Count: ", count, 
                                         "<br>Crime Type: ", MCI_CATEGORY))) +
    geom_point(size = 0.3) +
    geom_line(linewidth = 0.1) +
    scale_colour_manual(values = mci.yanse) +
    theme_bw() +
    scale_x_date(expand = expansion(add = 1), date_breaks = '6 months') +
    labs(title = "Crime Count of Selected Days By Crime Type", x = '', y = "Count", colour = 'Crime Type') +
    theme(legend.position = 'bottom')
  
  ggplotly(p1, tooltip = "text")  # Use custom text for tooltips
})

output$filterP1Summary<-renderUI({
inter.ok<-getFilterDataP1()
tags$p(tags$b(NROW(inter.ok)),'Records')
})


###
observe({
#####
inter.ok<-getFilterDataP1()
poi[where==input$whichSite,]->focus


if(input$locationSE)
{
   if(input$whichSite %in% poi[who=='Student',where]){
   aus<-leafletProxy('mapP1') %>%  ##leaflet proxy
        clearMarkers() %>%
        clearMarkerClusters() %>%
   	    clearShapes() %>%
 	    addAwesomeMarkers(data=inter.ok,
    	                     lng=~LONG_WGS84,lat=~LAT_WGS84,
    	                     clusterOptions = markerClusterOptions(),
    	                     label=lapply(inter.ok$lab,htmltools::HTML),
    	                     icon = iconscrime,
    	                     group='CrimeEvents') %>%
       addAwesomeMarkers(data=poi[who=='Student'],
   	                     lng=~long,lat=~lat,
   	                     label=~where,
   	                     icon = iconslibrary,
   	                     group='libraries') %>%
     flyTo(lng=focus$long,lat=focus$lat,zoom=16) %>%
     addCircles(lng=focus$long,lat=focus$lat,radius=300,weight=0.5)
	}
	
  if(input$whichSite %in% poi[who=='Tourist',where] ){
   aus<-leafletProxy('mapP1') %>%  ##leaflet proxy
        clearMarkers() %>%
        clearMarkerClusters() %>%
   	    clearShapes() %>%
 	    addAwesomeMarkers(data=inter.ok,
    	                     lng=~LONG_WGS84,lat=~LAT_WGS84,
    	                     clusterOptions = markerClusterOptions(),
    	                     label=lapply(inter.ok$lab,htmltools::HTML),
    	                     icon = iconscrime,
    	                     group='CrimeEvents') %>%
	  addAwesomeMarkers(data=poi[who!='Student'],
   	                     lng=~long,lat=~lat,
   	                     label=~where,
   	                     icon = iconstour,
   	                     group='TourSites') %>%
     flyTo(lng=focus$long,lat=focus$lat,zoom=16) %>%
     addCircles(lng=focus$long,lat=focus$lat,radius=300,weight=0.5)
	}
} else {
aus<-leafletProxy('mapP1') %>%  ##leaflet proxy
        clearMarkers() %>%
        clearMarkerClusters() %>%
   	    clearShapes() %>%
		setView(lat=43.7, lng=-79.42,zoom=11) %>% ####night
 	    addAwesomeMarkers(data=inter.ok,
    	                     lng=~LONG_WGS84,lat=~LAT_WGS84,
    	                     clusterOptions = markerClusterOptions(),
    	                     label=lapply(inter.ok$lab,htmltools::HTML),
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
   	                     group='TourSites')
}
	
	
})

###
output$mapP2<-renderLeaflet({
basemapP2
})

####
getFilterDataP2<-reactive({

if(!input$locationSEP2)
{
inter.ok2<-copy(info)
whichSTA<-'NONE'
focus2<-NULL
} else{
whichID<-poi[where==input$whichSiteP2,id]
whichSTA<-subwaysite[id==whichID,where]
copy(info)->info2
setnames(info2,whichSTA,'NAGE')
info2[NAGE<=300]->inter.ok2
focus2<-unique(subwaysite[,!'id'])[where==whichSTA]
}

###further filter according to time!!!
input$whichTimeIntervalP2->tip

if(tip=='Non-peak time'){
inter.ok2<-
inter.ok2[
(OCC_HOUR %in% c(9:14,19:23) & !OCC_DOW %in% c('Sunday','Saturday')) |
(OCC_DOW %in% c('Sunday','Saturday')) ,]
}

if(tip=='Peak time'){
inter.ok2<-
inter.ok2[
(OCC_HOUR %in% c(6:8,15:19) & !OCC_DOW %in% c('Sunday','Saturday'))]
}


if(tip=='Select interested time'){
day.se.p2<-input$whatdayP2
hous.se.p2<-input$whatHourP2
day.se.p2<-qid[match(day.se.p2[1],qid):match(day.se.p2[2],qid)]

hous.se.p2<-hous.se.p2[1]:hous.se.p2[2]
inter.ok2<-inter.ok2[(OCC_HOUR %in% hous.se.p2 & str_sub(OCC_DOW,1,3) %in% day.se.p2)]
}

list(inter.ok2,focus2,whichSTA)
})


#output$check<-renderPrint({
#getFilterDataP2()->dataP2
#str(dataP2)
#})

output$pieP2<-renderPlotly({
dataP2<-getFilterDataP2()
inter.ok2<-dataP2[[1]]
if(!NROW(inter.ok2)) return(NULL)

aa<-inter.ok2[,.N,MCI_CATEGORY]
aa[,lab:=glue_data(.SD,'Crime Type: {MCI_CATEGORY}<br>Number: {N}<br>Proportion: {scales::percent(N/sum(N),0.01)}')]

plot_ly(aa,
labels = ~MCI_CATEGORY,
 values = ~N, 
 hoverinfo = 'text',
 text=~lab, 
 type = 'pie',
 marker = list(colors =mci.yanse ),
 textinfo = 'label+percent') %>% 
 layout(title = 'Crime Count of Selected Time by Crime Type') %>%
 hide_legend()
})

####
output$bar1<-renderPlotly({
dataP2<-getFilterDataP2()
inter.ok2<-dataP2[[1]]
if(!NROW(inter.ok2)) return(NULL)

te<-inter.ok2[,unique(str_sub(OCC_DOW,1,3))]
range(match(te,qid))->qiid

inter.ok2.bar1<-inter.ok2[,.N,.(wday=str_sub(OCC_DOW,1,3))]

inter.ok2.bar1<-inter.ok2.bar1[qid,on=.(wday)]
inter.ok2.bar1[is.na(N),N:=0]
inter.ok2.bar1[,wday:=factor(wday,qid)]

pbar1<-ggplot(inter.ok2.bar1,aes(x=wday,y=N))+
geom_bar(stat='identity',colour='black',width=0.5,linewidth=0.1,fill='lightblue')+
theme_bw()+
labs(x='',y='Count', , title = 'Crime count in your selected days in week and hours')
#labs(x='',y='Count',title=glue("WeekDays:{paste(qid[qiid],collapse='~')}"))

ggplotly(pbar1)
})


output$bar2<-renderPlotly({
dataP2<-getFilterDataP2()
inter.ok2<-dataP2[[1]]
if(!NROW(inter.ok2)) return(NULL)

te<-inter.ok2[,unique(as.character(OCC_HOUR))]

inter.ok2.bar2<-inter.ok2[,.N,.(Hour=as.character(OCC_HOUR))]
inter.ok2.bar2<-inter.ok2.bar2[as.character(0:23),on=.(Hour)]
inter.ok2.bar2[is.na(N),N:=0]
inter.ok2.bar2[,Hour:=factor(Hour,as.character(0:23))]

pbar2<-ggplot(inter.ok2.bar2,aes(x=Hour,y=N))+
geom_bar(stat='identity',colour='black',width=0.5,linewidth=0.1,fill='lightblue')+
theme_bw()+
labs(x='',y='Count', title = 'Crime Count of Selected Hours and Days in Week')

ggplotly(pbar2)
})


##
output$timeAnnoUI<-renderUI({
if(input$whichTimeIntervalP2=='Select interested time') return(NULL)
if(input$whichTimeIntervalP2=='Non-peak time') return(tags$em(tags$p('Monday to Friday 09:00 to 14:59 and 19:00 to 23:59, as well as all times on Saturday and Sunday.')))
if(input$whichTimeIntervalP2=='Peak time') return(tags$em(tags$p('Monday to Friday 06:00 to 08:59 and 15:00 to 18:59.')))

})

##tackle map on Page2
###
observe({
#####
dataP2<-getFilterDataP2()
inter.ok2<-dataP2[[1]]
focus2<-dataP2[[2]]
focus2$labb<-paste(focus2$where,'(closest subway to',input$whichSiteP2,')')

if(input$locationSEP2){
leafletProxy('mapP2') %>%	
        clearMarkers() %>%
        clearMarkerClusters() %>%
   	    clearShapes() %>%
		clearPopups() %>%		
 	    addAwesomeMarkers(data=inter.ok2,
    	                     lng=~LONG_WGS84,lat=~LAT_WGS84,
    	                     clusterOptions = markerClusterOptions(),
    	                     label=lapply(inter.ok2$lab,htmltools::HTML),
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
   	                     group='subwaySites') %>%
       flyTo(lng=focus2$long,lat=focus2$lat,zoom=15) %>%
       addCircles(lng=focus2$long,lat=focus2$lat,radius=300,weight=0.5,opacity=0.2)  %>%
	   addPopups(data=focus2,
   	                     lng=~long,lat=~lat,
   	                     popup=~labb,
)
} else {
leafletProxy('mapP2') %>%
        clearMarkers() %>%
        clearMarkerClusters() %>%
   	    clearShapes() %>%
		clearPopups() %>%
		setView(lat=43.7, lng=-79.42,zoom=11)  %>%
 	    addAwesomeMarkers(data=inter.ok2,
    	                     lng=~LONG_WGS84,lat=~LAT_WGS84,
    	                     clusterOptions = markerClusterOptions(),
    	                     label=lapply(inter.ok2$lab,htmltools::HTML),
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
}	
	
})


}


shinyApp(ui,server)

