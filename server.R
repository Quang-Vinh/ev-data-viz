library(leaflet)
library(plotly)
library(RColorBrewer)
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)

# Get updated data with STC API
get_nmvr_data <- function() {
  
  # Province latitude and longitude data
  provinces_latlong <- read_csv('data/raw/provinces_latlong.csv') %>% 
    janitor::clean_names()
  
  # Download nmvs data using STC API
  file_path <- './data/raw/nmvs_data.zip'
  url <- 'https://www150.statcan.gc.ca/n1/tbl/csv/20100021-eng.zip'
  
  download.file(url, file_path)
  nmvr_file <- unz(file_path, '20100021.csv')
  
  data <- read_csv(nmvr_file) %>% 
    janitor::clean_names() %>% 
    filter(str_detect(vehicle_type, '^Total')) %>% 
    filter(geo != 'Canada') %>% 
    select(year = ref_date, geo, fuel_type, amount = value) %>%
    replace_na(list(amount = 0)) %>% 
    mutate(geo = recode(geo, `British Columbia and the Territories` = 'British Columbia')) %>% # Fix later
    left_join(provinces_latlong, by = c('geo' = 'province')) %>% 
    group_by(geo, fuel_type) %>% 
    mutate(cumsum = cumsum(amount))
  
  return (data)
}

nmvr_data <- get_nmvr_data()

# Extract some properties
fuel_types <- nmvr_data$fuel_type %>% unique()
ev_fuel_types <- c('Battery electric', 'Plug-in hybrid electric') 
provinces <- nmvr_data$geo %>% unique() %>% sort()

min_year <- nmvr_data$year %>% min()
max_year <- nmvr_data$year %>% max()


# Plotting parameters for map
bins <- c(0, 100, 10000, 100000, 500000, Inf)
max_value = nmvr_data$amount %>% max()
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
            title = '<small>Amount of vehicles</small>')



server <- function(input, output, session) {
  
  reactive_nmvr_data <- reactive({
    nmvr_data %>% filter(year == input$plot_date)
  })
  
  
  
  # Map View ----------------------------------------------------------------
  
  reactive_total_new_vehicles <- reactive({
    reactive_nmvr_data() %>% filter(fuel_type == 'All fuel types') %>% pull(amount) %>% sum()
  })
  
  reactive_total_new_zev <- reactive({
    reactive_nmvr_data() %>% filter(fuel_type %in% ev_fuel_types) %>% pull(amount) %>% sum()
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
  
  output$slider_input_plot_date <- renderUI({
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
  })
  
  output$select_input_province <- renderUI({
    selectInput(
      'province_select', 'Province',
      choices = provinces,
      selected = 'Ontario'
    )
  })
  
  output$select_input_fuel_type <- renderUI({
    selectInput(
      'fuel_type_select', 'Fuel type',
      choices = fuel_types
    )
  })

  
  # Update map circle markers when date changes
  observeEvent(input$plot_date, {
    leafletProxy('mymap') %>% 
      clearMarkers()
    
    # Add circle markers for each group to the basemap
    for (fuel_typ in fuel_types) {
      nmvr_data_filtered <- reactive_nmvr_data() %>% filter(fuel_type == fuel_typ)
      amount <- nmvr_data_filtered$amount
      
      leafletProxy('mymap') %>% 
        addCircleMarkers(
          data = nmvr_data_filtered, 
          lat = ~ latitude, 
          lng = ~ longitude,
          weight = 1, 
          radius = ~8*(amount)^(1/5),
          fillOpacity = 0.6, 
          fillColor = ~ev_pal(amount),
          group = fuel_typ,
          label = sprintf("<strong>%s</strong><br/>Amount: %g", nmvr_data_filtered$geo, amount) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px", direction = "auto"))
    }
  })
  
  
  
  
  # Growth Tab --------------------------------------------------------------
  
  
  # Set color ramp 
  my_colors <- colorRampPalette(brewer.pal(8, 'Set2'))(15)
  
  reactive_nmvr_data_fuel_type <- reactive({
    nmvr_data %>% filter(fuel_type == input$fuel_type_select)
  })
  
  reactive_nmvr_data_province <- reactive({
    nmvr_data %>% filter(geo == input$province_select)
  })
  
  # Disable/Enable select options based on Group select and set color ramp
  observeEvent(input$group_select, {
    if (input$group_select == 'Province') {
      shinyjs::disable('province_select')
      shinyjs::enable('fuel_type_select')
    } 
    else if (input$group_select == 'Fuel type') {
      shinyjs::enable('province_select')
      shinyjs::disable('fuel_type_select')
    }
  })
  
  # TODO: Switch to plotly proxy to update plots 
  output$time_series_plot <- renderPlotly({
    if (input$group_select == 'Province') {
      nmvr_data_plot <- reactive_nmvr_data_fuel_type()
      nmvr_data_plot %>% 
        plot_ly(x = ~year, y = ~cumsum, color = ~geo, colors = my_colors, type = 'scatter', mode = 'lines+markers') %>% 
        layout(
          xaxis = list(title = 'Year'),
          yaxis = list(title = 'Number of vehicles', range=c(0, 1.2 * max(nmvr_data_plot$cumsum))),
          title = paste0('Total number of ', input$fuel_type_select, ' vehicles over time')
        )
    } 
    else if (input$group_select == 'Fuel type') {
      nmvr_data_plot <- reactive_nmvr_data_province()
      nmvr_data_plot %>% 
        plot_ly(x = ~year, y = ~cumsum, color = ~fuel_type, type = 'scatter', mode = 'lines+markers') %>% 
        layout(
          xaxis = list(title = 'Year'),
          yaxis = list(title = 'Number of vehicles', range=c(0, 1.2 * max(nmvr_data_plot$cumsum))),
          title = paste0('Total number of vehicles in ', input$province_select,' over time')
        )
    }
  })
  
  output$bar_chart_plot <- renderPlotly({
    if (input$group_select == 'Province') {
      nmvr_data_plot <- reactive_nmvr_data_fuel_type()
      nmvr_data_plot %>% 
        plot_ly(x = ~year, y = ~amount, color = ~geo, colors = my_colors, type = 'bar') %>% 
        layout(
          yaxis = list(title = 'Number of new vehicles'),
          title = paste0('Number of new ', input$fuel_type_select, ' vehicles'),
          barmode = 'stack')
    }
    else if (input$group_select == 'Fuel type') {
      nmvr_data_plot <- reactive_nmvr_data_province() %>% filter(fuel_type != 'All fuel types')
      nmvr_data_plot %>% 
        plot_ly(x = ~year, y = ~amount, color = ~fuel_type, type = 'bar') %>% 
        layout(
          yaxis = list(title = 'Number of new vehicles'),
          title = paste0('Number of new vehicles per fuel type in ', input$province_select),
          barmode = 'stack')
    }
  })
  
  
  # output$sunburst_plot <- renderPlotly({
  #   nmvr_data_plot <- nmvr_data %>%
  #     filter(year == 2018) %>% 
  #     filter(fuel_type != 'All fuel types', fuel_type != 'Gasoline') %>% 
  #     arrange(geo)
  #   labels <- c('Canada', provinces, nmvr_data_plot$fuel_type)
  #   parents <- c('', rep('Canada', length(provinces)), nmvr_data_plot$geo)
  #   values <- c(
  #     nmvr_data_plot %>% pull(amount) %>% sum(),
  #     nmvr_data_plot %>% group_by(geo) %>% summarise(total=sum(amount)) %>% pull(total),
  #     nmvr_data_plot %>% pull(amount)
  #   )
  #   plot_ly(labels = labels, parents = parents, values = values, type = 'sunburst')
  # })  
  
}
