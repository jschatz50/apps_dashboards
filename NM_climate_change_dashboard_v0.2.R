#------------#
#-- author --#
#------------#
# Jason Schatz 
# created: 01/05/17
# last modified: 01/09/17


#-----------------#
#-- description --#
#-----------------#
# Builds an interactive dashboard that displays climate change projections for a 
# given part of NM, specified by the user entering an address, city, or zip code.


#--------------------#
#-- open libraries --#
#--------------------#
library(shiny)
library(shinydashboard)
library(rgdal)
library(tigris)
library(RColorBrewer)
library(leaflet)
library(ggplot2)
library(ncdf4)
library(ggmap)

#register_google(key = 'KEY STRING')


#----------------------#
#-- define functions --#
#----------------------#
#### returns position in matrix whose value is closest to a specified value
## @param coordinate:  individual coordinate
## @param coordinate_matrix:  matrix of coordinates to search for nearest value
find_nearest <- function(coordinate, coordinate_matrix){
   coordinate <- ifelse(coordinate < -100, coordinate + 360, coordinate)
   diffs <- coordinate - coordinate_matrix
   nearest <- which(abs(diffs) == min(abs(diffs)))
   return(nearest)
}


#--------------------#
#-- define globals --#
#--------------------#
filepath <- 'd90F_1986-2005_historical.nc'
nc <- nc_open(filepath)
lats = ncvar_get(nc, 'latitude')
lons = ncvar_get(nc, 'longitude')
head1 = "This tool compares New Mexico's current climate to projected future climates.  To use this tool, enter a New Mexico city, zip code, or street address."
head2 = 'These data are derived from the LOCA (localized constructed analogs) downscaled climate projections dataset (Pierce et al 2014). The LOCA method downscales CMIP5 climate projections to daily temporal resolution and 1/16th degree spatial resolution using local climate analogues.  The data displayed here are means of 28 GCMs for New Mexico in historical (1986-2005), mid-century (2040-2059), and late-century (2080-2099) for low-emissions (RCP 4.5) and high-emissions (RCP 8.5) scenarios.'
head3 = 'Pierce, DW, DR Cayan, and BL Thrasher 2014. Statistical downscaling using Localized Constructed Analogs (LOCA). Journal of Hydrometerology 15: 2558-2585.'

#---------------#
#-- define UI --#
#---------------#
ui <- dashboardPage(
   dashboardHeader(title = "New Mexico Climate"),
   dashboardSidebar(
      textInput(inputId = "address", 
                label = "Address",
                placeholder = "enter address"),
      actionButton("submit", "Submit", class = "btn-primary"),
      selectInput("metric", "Temperature measure",  c("Days/yr over 90F" = "d90F", 
                                                      "Days/yr over 100F" = "d100F"))
   ),
   dashboardBody(
      fluidRow(
         br(),
         h1(head1, style="color:black;font-size:100%", align = "left"),
         box(plotOutput("Plot", height = 600, width = 660),
         h1(head2, style="color:black;font-size:100%", align = "left"),
         h1(head3, style="color:black;font-size:100%", align = "left"))           
      )
   )
)


#-------------------#
#-- define server --#
#-------------------#
server <- function(input, output) {

   geocoded <- eventReactive(input$submit, {
      reply = geocode(input$address, output='all')
      reply
   })

   lat1 <- eventReactive(input$submit, {
      address_lat <- geocoded()$results[[1]]$geometry$location$lat
      y_coord <- find_nearest(address_lat, lats)
      y_coord
   })

   lon1 <- eventReactive(input$submit, {
      address_lon <- geocoded()$results[[1]]$geometry$viewport$southwest$lng
      x_coord <- find_nearest(address_lon, lons)
      x_coord
   })

   final_data <- reactive({
      hist_nc   <- nc_open(paste(input$metric, '_1986-2005_historical.nc', sep = ""))
      mid45_nc  <- nc_open(paste(input$metric, '_2040-2059_RCP45.nc', sep = ""))
      mid85_nc  <- nc_open(paste(input$metric, '_2040-2059_RCP85.nc', sep = ""))
      late45_nc <- nc_open(paste(input$metric, '_2080-2099_RCP45.nc', sep = ""))
      late85_nc <- nc_open(paste(input$metric, '_2080-2099_RCP85.nc', sep = ""))

      data_hist   <- ncvar_get(hist_nc, 'data')
      data_mid45  <- ncvar_get(mid45_nc, 'data')
      data_mid85  <- ncvar_get(mid85_nc, 'data')
      data_late45 <- ncvar_get(late45_nc, 'data')
      data_late85 <- ncvar_get(late85_nc, 'data')

      hist_val   <- apply(data_hist, c(1,2), mean)[lon1(), lat1()]
      mid45_val  <- apply(data_mid45, c(1,2), mean)[lon1(), lat1()]
      mid85_val  <- apply(data_mid85, c(1,2), mean)[lon1(), lat1()]
      late45_val <- apply(data_late45, c(1,2), mean)[lon1(), lat1()]
      late85_val <- apply(data_late85, c(1,2), mean)[lon1(), lat1()]

      df <- data.frame('period' = c(1, 1, 2, 2, 3, 3),
                       'scenario' = c('historical1', 'historical2', 'low-emissions', 'high-emissions', 'low-emissions', 'high-emissions'),
                       'value' = c(NA, hist_val, mid45_val, mid85_val, late45_val, late85_val))
      df$scenario <- factor(df$scenario, levels = c("historical1", "historical2", "low-emissions", "high-emissions"))
      df
   })

   ## y-axis label
   ylab1 <- reactive({
      label <- ifelse(input$metric %in% c('d90F','d95F','d100F','d110F'), 'Days/yr', 'Degrees C')
      label
   })

   ## plot data
   output$Plot <- renderPlot({
      labs <- factor(1:3, labels = c("1986-2005", 
                                     "2040-2059",
                                     "2080-2099"), 1:3)
      ggplot(data = final_data(), aes(x=period, y=value, fill=scenario, width=0.7, col=I("black")), ordered=T) + 
         geom_bar(stat = "identity", position = position_dodge(0.7)) +
         scale_x_discrete(limits=labs) +
         theme(axis.text=element_text(size=16),
               axis.title.x=element_blank(),
               axis.title.y=element_text(size=15),
               legend.title=element_blank(),
               legend.text=element_text(size=15),
               panel.background = element_blank(),
               panel.grid.major = element_blank(), 
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black")) +
         scale_fill_manual(breaks=c("historical", "low-emissions", "high-emissions"), 
                           values=c("#c6dbef", "#c6dbef", "#6baed6", "#084594")) +
         ylab(ylab1())
   })
}


#-----------------#
#-- execute app --#
#-----------------#
shinyApp(ui, server)



#-----------#
#-- to do --#
#-----------#
## I could make a table of lat/lon coordinates of every city in NM, which would
## bypass Google's API.  Could also add standard error bars.





