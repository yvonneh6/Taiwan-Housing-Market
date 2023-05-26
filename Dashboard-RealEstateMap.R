library(shiny)
library(leaflet)
library(readxl)

house = read_excel("Real estate valuation data set.xlsx")

ui <- fluidPage(
  leafletOutput("map")
)

# color palettes
PricePalette <- colorNumeric(c("blue", "red"), domain = house$`Y house price of unit area`)
AgePalette <- colorNumeric(c("blue", "red"), domain = house$`X2 house age`)
MRTPalette <- colorNumeric(c("blue", "red"), domain = house$`X3 distance to the nearest MRT station`)
StorePalette <- colorNumeric(c("blue", "red"), domain = house$`X4 number of convenience stores`)


server <- function(input, output, session) {
  
  # map
  output$map <- renderLeaflet({
    
    map = house %>%
      leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addCircles(lng = ~house$`X6 longitude`, lat = ~house$`X5 latitude`,color = ~PricePalette(house$`Y house price of unit area`), group = "House Price") %>%
      addCircles(lng = ~house$`X6 longitude`, lat = ~house$`X5 latitude`,color = ~AgePalette(house$`X2 house age`), group = "House Age") %>%
      addCircles(lng = ~house$`X6 longitude`, lat = ~house$`X5 latitude`,color = ~MRTPalette(house$`X3 distance to the nearest MRT station`), group = "Distance to the Nearest MRT Station") %>%
      addCircles(lng = ~house$`X6 longitude`, lat = ~house$`X5 latitude`,color = ~StorePalette(house$`X4 number of convenience stores`), group = "Number of Stores") %>%
      addLayersControl(
        baseGroups=c("House Price", "House Age", "Distance to the Nearest MRT Station", "Number of Stores"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        "topright",
        pal = PricePalette,
        values = house$`Y house price of unit area`,
        group = "House Price"
      ) 
    
    
  })
}

## update legend when the selected layer group changes
observeEvent(input$map_groups, {
  my_map <- leafletProxy("map") %>% clearControls()
  if (input$map_groups == 'House Price'){
    my_map <- my_map %>% addLegend(
      "topright",
      pal = PricePalette,
      values = house$`Y house price of unit area`, 
      group = "House Price", className = "Leaflet Price")
  } else if (input$map_groups == 'House Age'){
    my_map <- my_map %>% addLegend(
      "topright",
      pal = AgePalette,
      values = house$`X2 house age`,
      group = "House Age")
  } else if (input$map_groups == 'Distance to the Nearest MRT Station'){
    my_map <- my_map %>% addLegend(
      "topright",
      pal = MRTPalette,
      values = house$`X3 distance to the nearest MRT station`,
      group = "Distance to the Nearest MRT Station")
  } else{
    my_map <- my_map %>%
      addLegend(
        "topright",
        pal = StorePalette,
        values = house$`X4 number of convenience stores`)
  }
})

shinyApp(ui, server)