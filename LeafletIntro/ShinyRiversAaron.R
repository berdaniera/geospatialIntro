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
m <- leaflet() %>% addTiles()

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
    selectInput(inputId = "state",
                label = "State:",
                choices = state.abb,
                selected = "NC"),
#     checkboxGroupInput("variables", label = h3("Variables:"), 
#                        choices = list("Discharge" = "00060", 
#                                       "Turbidity" = "00070", 
#                                       "pH" = "00400"),
#                         selected = 1),
    radioButtons("variables", label = h3("Variables:"), 
                       choices = list("Discharge, ft3/s" = "00060",
                                      "Gage height, ft" = "00065",
                                      "Temperature, C" = "00010",
                                      "Turbidity" = "00070", 
                                      "Specific conductance, uS/cm" = "00095"),
                       selected = "00065"),
  #     checkboxInput(inputId = "discharge",label = "Discharge",value = TRUE),    
    #     checkboxInput(inputId = "turbidity",label = "Turbidity",value = FALSE),
    #     checkboxInput(inputId = "pH",label = "pH",value = FALSE),
    sliderInput("days", "Number of days back:",min=1, max=30, value=7),
    actionButton("getdat","View data")
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
    h3("Directions:"),
    p("1. Choose a state to display stations."),
    p("2. Choose output variables and a time scale."),
    p("3. Click on a station or zoom to an area extent."),
    p("4. Click 'View data'."),
    leafletOutput("leafmap"),
    plotOutput("plt"),
    verbatimTextOutput("tab")
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

  
  dat <- eventReactive( input$state, getUSGSsites(tolower(input$state)) )
  output$leafmap <- renderLeaflet({
    addCircleMarkers(m, data=dat()$sites, 
                     popup=dat()$codes$name, 
                     layerId=dat()$codes$value,
                     radius=3,fillOpacity=0.75,stroke=FALSE,color="#001A57")
  })
  

  site <- reactive({
    if(!is.null(input$leafmap_marker_click)) site <- as.character(input$leafmap_marker_click$id)
    else NULL
  })
  mapbounds <- reactive(paste(rev(unlist( input$leafmap_bounds )),collapse=","))

  obs <- eventReactive( input$getdat, getUSGSdata(input$days, input$variables, site(), mapbounds()) )  

  # data is in obs()$data
  # site name is in obs()$site
  #observeEvent(input$days, print(obs()))
  output$plt <- renderPlot({
    values <- obs()$data$value
    dates <- getdates(obs()$data$dateTime)
    plot(values~dates,type="b",pch=19)
  })
  output$tab <- renderPrint({
    if(!is.na(obs())) print(summary(as.numeric(obs()$data$value)))
    else print(NULL)
  })
  
}

shinyApp(ui = ui, server = server)

