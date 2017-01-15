#----------#
#--AUTHOR--#
#----------#
# Jason Schatz 
# created: 01/05/17
# last modified: 01/05/17


#---------------#
#--DESCRIPTION--#
#---------------#
# Generates a web app that displays the percent of tested buildings in 
# a given NM small area that exceeded a user-selected level of radon


#--------------------#
#-- open libraries --#
#--------------------#
library(shiny)
library(shinydashboard)
library(rgdal)
library(tigris)
library(RColorBrewer)
library(leaflet)


#---------------#
#-- prep data --#
#---------------#
geom <- readOGR("small_areas.geojson", "OGRGeoJSON")
data <- read.csv('fake_data.csv', header = T)

ffloor <- subset(data, floor == '1st')
basement <- subset(data, floor == 'basement')

agg.f <- with(ffloor, aggregate(x = value, by = list(OBJECTID), FUN = length))
agg.b <- with(basement, aggregate(x = value, by = list(OBJECTID), FUN = length))
IDs.f  <- agg.f$Group.1[which(agg.f$x >= 10)]   #only include small areas with >10 results
IDs.b  <- agg.b$Group.1[which(agg.b$x >= 10)]   #only include small areas with >10 results
d.f <- ffloor[ffloor$OBJECTID %in% IDs.f, ]
d.b <- basement[basement$OBJECTID %in% IDs.b, ]

data <- rbind(d.f, d.b)


#---------------#
#-- define UI --#
#---------------#
ui <- dashboardPage(
   dashboardHeader(title = "Radon level exceedence probability"),
   dashboardSidebar(
      sliderInput("radon_level", "Radon level (pCi/L):", 1, 20, 4),
      selectInput("floor", "Floor of Building",  c("1st Floor" = "1st", "Basement" = "basement"))
   ),
   dashboardBody(
      fluidRow(
         br(),
         h1("***NOTE: THESE ARE NOT REAL RADON DATA****", style="color:black;font-size:250%", align = "left"),
         h1("***THESE ARE ARTIFICIAL DATA GENERATED TO DESIGN THIS DASHBOARD****", style="color:black;font-size:250%", align = "left"),
         h2("Radon level exceedence probability", style="color:black;font-size:100%", align = "left"),
         h3("% of tested buildings exceeding a given level of radon by New Mexico small area", style="color:black;font-size:100%", align = "left"),
         box(leafletOutput("map", height = 600, width = 660))           
      )
   )
)


#-------------------#
#-- define server --#
#-------------------#
server <- function(input, output) {
 
filtered_data1 <- reactive({
   with(subset(data, floor == input$floor), 
        setNames(aggregate(x = value, by = list(OBJECTID), 
        FUN = function(value){(1 - ecdf(value)(input$radon_level)) * 100}), c('OBJECTID', 'value')))
   })

filtered_data2 <- reactive({
   geo_join(geom, filtered_data1(), by = 'OBJECTID')
   })

pal <- colorNumeric(
       palette = "YlGnBu",
       domain = seq(0,100,10))

output$map <- renderLeaflet({
   leaflet() %>% 
   addTiles() %>%
   addPolygons(data = geom) %>%
   addLegend(pal = pal, 
             values = seq(0,100,10), 
             position = "bottomright", 
             labFormat = labelFormat(suffix="%")) 	
})

observe({
   pal <- pal
   leafletProxy("map", data = filtered_data2()) %>%
      clearShapes() %>%
      addPolygons(
                  fillColor = ~pal(filtered_data2()$value), 
                  color = "#b2aeae",
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.2,
                  popup = paste(round(filtered_data2()$value), "%", sep=''))
  })
}


#-----------------#
#-- execute app --#
#-----------------#
shinyApp(ui, server)

