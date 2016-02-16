require(jsonlite)
require(leaflet)
require(lubridate)
require(shiny)
require(sp)

# Functions
getUSGSsites <- function(state){
  url <- paste0("http://waterservices.usgs.gov/nwis/iv/?period=PT1H&stateCd=",state,"&parameterCd=00060&format=json&siteStatus=active")
  dat <- jsonlite::fromJSON(url)
  sites <- dat$value$timeSeries$sourceInfo$geoLocation$geogLocation
  sites <- SpatialPoints(sites[,c("longitude","latitude")], CRS("+init=epsg:4326"))
  codes <- dat$value$timeSeries$sourceInfo$siteCode
  cdnm <- names(codes[[1]])
  codes <- data.frame(matrix(unlist(codes),ncol=6,byrow=T))
  codes <- data.frame(codes,dat$value$timeSeries$sourceInfo$siteName)
  colnames(codes) <- c(cdnm,"name")  
  list(sites=sites,codes=codes)
}
getUSGSdata <- function(days,vars,site,bounds){
  period <- paste0("P",days,"D")
  param <- paste(unlist(vars),collapse=",")
  if(!is.null(site)) url <- paste0("http://waterservices.usgs.gov/nwis/iv/?period=",period,"&sites=",site,"&parameterCd=",param,"&format=json")
  else url <- paste0("http://waterservices.usgs.gov/nwis/iv/?period=",period,"&bBox=",bounds,"&parameterCd=",param,"&format=json")
  print(url)
  dat <- jsonlite::fromJSON(url)
  list(data=dat$value$timeSeries$values[[1]]$value[[1]], 
       site=dat$value$timeSeries$sourceInfo$siteName[1]
  )
}
getdates <- function(date) ymd_hms(gsub("T", " ", substr(date,1,nchar(date)-6)))

#### Create basemap
  # m <- leaflet() %>% addTiles()

################################
# Shiny
ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    # Dropdown: choose state
      # hint: see state.abb
    # checkboxes for variables:
      # "00060" Discharge, cubic feet per second
      # "00065" Gage height, ft
      # "00070" Turbidity, Jackson Turbidity Units
      # "00010" Temperature, C
      # "00095" Specific conductance, uS/cm
    # choose time scale slider number of days (1 to 30)
    # action button
  ),
  mainPanel(
    # Text directions: 
      # 1. Choose a state to display stations
      # 2. Choose output variables and a time scale
      # 3. Click on a station or zoom to an area extent
      # 4. Click "View Data"
    # Output leaflet map
    # Output station name
    # Output plots
    # Output data summaries
  )
))

server <- function(input, output) {
  # eventReactive map data
    # eventExpr is state input
    # handlerExpr is getUSGSsite(state) with input, needs lowercase state input

  # create Leaflet map output
    # renderLeaflet
    # addMarkers or addCircleMarkers points to the basemap
    # data is in dat()$sites
    # popup site names dat()$codes$name
    # create layerId for station codes: dat()$codes$value

  # get current map bounds
#  mapbounds <- reactive(paste(rev(unlist( input$MAPID_bounds )),sep=","))
  # need to change MAPID to name of your map
  
  # get site selected from map
#   site <- reactive({
#     if(!is.null(input$MAPID_marker_click)) site <- as.character(input$MAPID_marker_click$id)
#     else NULL
#   })
  # need to change MAPID to name of your map
  
  # obs <- eventReactive stream data
    # eventExpr is actionButton
    # handlerExpr is getUSGSdata(days, vars, site) with inputs

  # Output station name
    # site name is in obs()$site
  # Output graph
    # data table is in obs()$data
    # values are in obs()$data$value
    # dates are obs()$data$dateTime
    # to convert dates: getdates(dates)
  # Output data summary
  
}

shinyApp(ui = ui, server = server)

