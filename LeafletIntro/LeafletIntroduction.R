library(leaflet)
m <- leaflet() %>% addTiles()
m <- addMarkers(m, lng=c(-79.0943), lat=c(35.9836),
                popup="Aaron's research site")
m

library(shiny)
ui <- fluidPage(
  leafletOutput("amap")
)
server <- function(input, output, session){
  output$amap <- renderLeaflet({ m })
}
shinyApp(ui, server)