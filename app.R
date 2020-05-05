#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(leaflet)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)


# Load data
provinces_latlong <- read_csv('data/raw/provinces_latlong.csv') %>% 
  janitor::clean_names()

ev_data <- read_csv('data/raw/ev_registrations.csv') %>% 
  janitor::clean_names() %>% 
  filter(str_detect(vehicle_type, '^Total')) %>% 
  filter(geo != 'Canada') %>% 
  select(year = ref_date, geo, fuel_type, amount = value) %>%
  replace_na(list(amount = 0)) %>% 
  mutate(geo = recode(geo, `British Columbia and the Territories` = 'British Columbia')) %>% # Fix later
  left_join(provinces_latlong, by = c('geo' = 'province'))

fuel_types = ev_data$fuel_type %>% unique()

ev_data <- ev_data %>% pivot_wider(names_from = fuel_type, values_from = amount)


# Plotting parameters for map
bins <- c(0, 10, 1000, 10000, 30000, Inf)
ev_pal <- colorBin('Blues', domain = ev_data['All fuel types'], bins = bins)


# Create basemap
basemap <- leaflet() %>% 
  addTiles() %>% 
  addLayersControl(
    position = 'bottomright',
    overlayGroups = fuel_types,
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(fuel_types[-1]) %>% 
  fitBounds(-140, 74, -44, 42) %>% 
  addLegend('bottomright', pal = ev_pal, values = ev_data['All fuel types'],
            title = '<small>Amount</small>')

# Add circle markers for each group
basemap <- basemap %>% 
  addCircleMarkers(
    data = ev_data, 
    lat = ~ latitude, 
    lng = ~ longitude,
    weight = 1, 
    radius = ~(`All fuel types`)^(1/4), 
    fillOpacity = 0.2, 
    group = 'All fuel types',
    label = sprintf("<strong>%s</strong><br/>Amount: %g", ev_data$geo, ev_data %>% pull('All fuel types')) %>% lapply(htmltools::HTML),
    labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px", direction = "auto"))



ui <- bootstrapPage(
  
  div(class='outer',
      tags$head(includeCSS('styles.css')),
      leafletOutput('mymap', width='100%', height='100%')
      )
  
)


server <- function(input, output) {
  
  output$mymap <- renderLeaflet({basemap})
  
}


# Run the application 
shinyApp(ui = ui, server = server)
