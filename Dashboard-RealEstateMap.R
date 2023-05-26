library(shiny)
library(leaflet)
library(readxl)

house <- read_excel("Real estate valuation data set.xlsx")

ui <- fluidPage(
  leafletOutput("map")
)

# Define color palettes based on the range of values
PricePalette <- colorNumeric(
  palette = c("blue", "red"),
  domain = house$`Y house price of unit area`
)
AgePalette <- colorNumeric(
  palette = c("blue", "red"),
  domain = house$`X2 house age`
)
MRTPalette <- colorNumeric(
  palette = c("blue", "red"),
  domain = house$`X3 distance to the nearest MRT station`
)
StorePalette <- colorNumeric(
  palette = c("blue", "red"),
  domain = house$`X4 number of convenience stores`
)


server <- function(input, output, session) {
  
  # Render the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addCircles(
        data = house,
        lng = ~ `X6 longitude`,
        lat = ~ `X5 latitude`,
        color = ~ PricePalette(`Y house price of unit area`),
        group = "House Price"
      ) %>%
      addCircles(
        data = house,
        lng = ~ `X6 longitude`,
        lat = ~ `X5 latitude`,
        color = ~ AgePalette(`X2 house age`),
        group = "House Age"
      ) %>%
      addCircles(
        data = house,
        lng = ~ `X6 longitude`,
        lat = ~ `X5 latitude`,
        color = ~ MRTPalette(`X3 distance to the nearest MRT station`),
        group = "Distance to the Nearest MRT Station"
      ) %>%
      addCircles(
        data = house,
        lng = ~ `X6 longitude`,
        lat = ~ `X5 latitude`,
        color = ~ StorePalette(`X4 number of convenience stores`),
        group = "Number of Stores"
      ) %>%
      addLayersControl(
        baseGroups = c(
          "House Price",
          "House Age",
          "Distance to the Nearest MRT Station",
          "Number of Stores"
        ),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        "topright",
        pal = PricePalette,
        values = house$`Y house price of unit area`,
        group = "House Price",
        title = "House Price (unit)"
      )
  })
  
  # Update legend when the selected layer group changes
  observeEvent(input$map_groups, {
    leafletProxy("map") %>% clearControls() %>%
      addLegend(
        "topright",
        pal = switch(
          input$map_groups,
          "House Price" = PricePalette,
          "House Age" = AgePalette,
          "Distance to the Nearest MRT Station" = MRTPalette,
          StorePalette
        ),
        values = switch(
          input$map_groups,
          "House Price" = house$`Y house price of unit area`,
          "House Age" = house$`X2 house age`,
          "Distance to the Nearest MRT Station" = house$`X3 distance to the nearest MRT station`,
          house$`X4 number of convenience stores`
        ),
        group = input$map_groups,
        title = switch(
          input$map_groups,
          "House Price" = "House Price per Unit Area",
          "House Age" = "Year",
          "Distance to the Nearest MRT Station" = "Meters",
          "Total Number"
        )
      )
  })
}

shinyApp(ui, server)
