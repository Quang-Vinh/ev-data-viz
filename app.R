#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Based off of https://github.com/eparker12/nCoV_tracker


library(leaflet)
library(plotly)
library(RColorBrewer)
library(shiny)
library(shinyjs)
library(shinythemes)
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
  left_join(provinces_latlong, by = c('geo' = 'province')) %>% 
  group_by(geo, fuel_type) %>% 
  mutate(cumsum = cumsum(amount))

fuel_types <- ev_data$fuel_type %>% unique()
ev_fuel_types <- c('Battery electric', 'Plug-in hybrid electric') 
provinces <- ev_data$geo %>% unique() %>% sort()

min_year <- ev_data$year %>% min()
max_year <- ev_data$year %>% max()


# Plotting parameters for map
bins <- c(0, 100, 10000, 100000, 500000, Inf)
max_value = ev_data$amount %>% max()
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


ui <- bootstrapPage(
  shinyjs::useShinyjs(),
  
  navbarPage(theme = shinytheme('flatly'), collapsible = TRUE, 'VRF Data Visualisation', id='nav',
            
    tabPanel('Map View',
      div(class='outer', tags$head(includeCSS('styles.css')),
        leafletOutput('mymap', width = '100%', height = '100%'),
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
        ) # absolute Panel
      ) # div outer
    ), # Tab panel
    
    tabPanel('Growth view',
      sidebarLayout(
        sidebarPanel(
          selectInput(
            'group_select', 'Group by',
            choices = c('Province', 'Fuel type')),
          selectInput(
            'province_select', 'Province',
            choices = provinces,
            selected = 'Ontario'
          ),
          selectInput(
            'fuel_type_select', 'Fuel type',
            choices = fuel_types
          ),
          width = 2
        ),
        mainPanel(
          plotlyOutput('time_series_plot'),
          plotlyOutput('bar_chart_plot')
        )
      ) # sidebar layout
    ) # tab panel
  ) # navbar page
)


server <- function(input, output) {
  
  reactive_ev_data <- reactive({
    ev_data %>% filter(year == input$plot_date)
  })
  
  reactive_total_new_vehicles <- reactive({
    reactive_ev_data() %>% filter(fuel_type == 'All fuel types') %>% pull(amount) %>% sum()
  })
  
  reactive_total_new_zev <- reactive({
    reactive_ev_data() %>% filter(fuel_type %in% ev_fuel_types) %>% pull(amount) %>% sum()
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
  
  ### Map view tab
  # Update map circle markers when date changes
  observeEvent(input$plot_date, {
    leafletProxy('mymap') %>% 
    clearMarkers()
    
    # Add circle markers for each group to the basemap
    for (fuel_typ in fuel_types) {
      ev_data_filtered <- reactive_ev_data() %>% filter(fuel_type == fuel_typ)
      amount <- ev_data_filtered$amount
      
      leafletProxy('mymap') %>% 
        addCircleMarkers(
          data = ev_data_filtered, 
          lat = ~ latitude, 
          lng = ~ longitude,
          weight = 1, 
          radius = ~8*(amount)^(1/5),
          fillOpacity = 0.6, 
          fillColor = ~ev_pal(amount),
          group = fuel_typ,
          label = sprintf("<strong>%s</strong><br/>Amount: %g", ev_data_filtered$geo, amount) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px", direction = "auto"))
    }
  })
  
  
  ### Growth tab
  
  # Set color ramp 
  my_colors <- colorRampPalette(brewer.pal(8, 'Set2'))(15)
  
  reactive_ev_data_fuel_type <- reactive({
    ev_data %>% filter(fuel_type == input$fuel_type_select)
  })
  
  reactive_ev_data_province <- reactive({
    ev_data %>% filter(geo == input$province_select)
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
      ev_data_plot <- reactive_ev_data_fuel_type()
      ev_data_plot %>% 
        plot_ly(x = ~year, y = ~cumsum, color = ~geo, colors = my_colors, type = 'scatter', mode = 'lines+markers') %>% 
        layout(
          xaxis = list(title = 'Year'),
          yaxis = list(title = 'Number of vehicles', range=c(0, 1.2 * max(ev_data_plot$cumsum))),
          title = paste0('Total number of ', input$fuel_type_select, ' vehicles over time')
        )
    } 
    else if (input$group_select == 'Fuel type') {
      ev_data_plot <- reactive_ev_data_province()
      ev_data_plot %>% 
        plot_ly(x = ~year, y = ~cumsum, color = ~fuel_type, type = 'scatter', mode = 'lines+markers') %>% 
        layout(
          xaxis = list(title = 'Year'),
          yaxis = list(title = 'Number of vehicles', range=c(0, 1.2 * max(ev_data_plot$cumsum))),
          title = paste0('Total number of vehicles in ', input$province_select,' over time')
        )
    }
  })

  output$bar_chart_plot <- renderPlotly({
    if (input$group_select == 'Province') {
      ev_data_plot <- reactive_ev_data_fuel_type()
      ev_data_plot %>% 
        plot_ly(x = ~year, y = ~amount, color = ~geo, colors = my_colors, type = 'bar') %>% 
        layout(
          yaxis = list(title = 'Number of new vehicles'),
          title = paste0('Number of new ', input$fuel_type_select, ' vehicles'),
          barmode = 'stack')
    }
    else if (input$group_select == 'Fuel type') {
      ev_data_plot <- reactive_ev_data_province()
      ev_data_plot %>% 
        plot_ly(x = ~year, y = ~amount, color = ~fuel_type, type = 'bar') %>% 
        layout(
          yaxis = list(title = 'Number of new vehicles'),
          title = paste0('Number of new vehicles per fuel type in ', input$province_select),
          barmode = 'stack')
    }
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)





