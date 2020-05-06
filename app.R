#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Based off of https://github.com/eparker12/nCoV_tracker


library(leaflet)
library(shiny)
library(tidyverse)


# Load data and preprocess
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

fuel_types <- ev_data$fuel_type %>% unique()
ev_fuel_types <- c('Battery electric', 'Plug-in hybrid electric') 

ev_data <- ev_data %>% pivot_wider(names_from = fuel_type, values_from = amount)

min_year <- ev_data$year %>% min()
max_year <- ev_data$year %>% max()


# Plotting parameters for map
bins <- c(0, 100, 10000, 100000, 500000, Inf)
max_value = ev_data %>% select(all_of(fuel_types)) %>% max()
ev_pal <- colorBin('Blues', domain = c(0, max_value), bins = bins)


# Create basemap
basemap <- leaflet() %>% 
  addTiles() %>% 
  addLayersControl(
    position = 'topright',
    overlayGroups = fuel_types,
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(fuel_types[-1]) %>% 
  setView(-95, 55, zoom = 5) %>% 
  addLegend('topright', pal = ev_pal, values = c(0, max_value),
            title = '<small>Amount</small>')


ui <- bootstrapPage(
  div(class='outer',
    tags$head(includeCSS('styles.css')),
    
    leafletOutput('mymap', width='100%', height='100%'),
    
    absolutePanel(
      id = 'controls', class = 'panel panel-default', top = 80, left = 20, width = 250, fixed = TRUE,
      draggable = TRUE, height = 'auto',
      
      h3(textOutput('reactive_total_new_vehicles'), align = 'right'),
      span(h4(textOutput('reactive_total_new_gv'), align = 'right'), style="color:#cc4c02"),
      span(h4(textOutput('reactive_total_new_zev'), align = 'right'), style="color:#006d2c"),
      
      sliderInput(
        'plot_date',
        label = 'Year',
        value = max_year,
        min = min_year,
        max = max_year,
        step = 1,
        sep = '',
        animate = animationOptions(interval = 2000, loop = FALSE)
      )
      
    )
  )
)


server <- function(input, output) {
  
  reactive_ev_data <- reactive({
    ev_data %>% filter(year == input$plot_date)
  })
  
  reactive_total_new_vehicles <- reactive({
    reactive_ev_data() %>% pull('All fuel types') %>% sum()
  })
  
  reactive_total_new_zev <- reactive({
    reactive_ev_data() %>% select(all_of(ev_fuel_types)) %>% sum()
  })
  
  reactive_total_new_gv <- reactive({
    reactive_total_new_vehicles() - reactive_total_new_zev()
  })
  
  
  output$reactive_total_new_vehicles <- renderText({
    paste0(prettyNum(reactive_total_new_vehicles(), big.mark=','), ' new vehicles')
  })
  
  output$reactive_total_new_gv <- renderText({
    paste0(prettyNum(reactive_total_new_gv(), big.mark=','), ' new gas vehicles')
  })
  
  output$reactive_total_new_zev <- renderText({
    paste0(prettyNum(reactive_total_new_zev(), big.mark=','), ' new electric vehicles')
  })
  
  
  output$mymap <- renderLeaflet({basemap})
  
  
  # Update map circle markers when date changes
  observeEvent(input$plot_date, {
    leafletProxy('mymap') %>% 
    clearMarkers()
      
    ev_data_filtered <- reactive_ev_data()
    # Add circle markers for each group to the basemap
    for (fuel_type in fuel_types) {
      amount <- ev_data_filtered %>% pull(fuel_type)
      leafletProxy('mymap') %>% 
        addCircleMarkers(
          data = ev_data, 
          lat = ~ latitude, 
          lng = ~ longitude,
          weight = 1, 
          radius = ~8*(amount)^(1/5),
          fillOpacity = 0.2, 
          fillColor = ~ev_pal(amount),
          group = fuel_type,
          label = sprintf("<strong>%s</strong><br/>Amount: %g", ev_data_filtered$geo, amount) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px", direction = "auto"))
    }
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)





