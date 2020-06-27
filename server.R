library(formatR)
library(leaflet)
library(plotly)
library(RColorBrewer)
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)

source('load_data.R')


# TODO: Pass nmvr and nmvs data as parameters to rmd file. Downloading is fast enough for now



# Set color ramp 
my_colors <- colorRampPalette(brewer.pal(8, 'Set2'))(15)



server <- function(input, output, session) {

  # Datasets. Updated daily ------------------------------------------------
  
  reactive_nmvr_data <- reactive({
    invalidateLater(8.64e7, session)
    return (get_nmvr_data())
  })
  
  reactive_nmvs_data <- reactive({
    invalidateLater(8.64e7, session)
    return (get_nmvs_data())
  })
  
  
  
  
  # NMVR Map View ----------------------------------------------------------------

  # Extract some properties from datasets
  fuel_types <- reactive({
    reactive_nmvr_data()$fuel_type %>% unique()
  })
  
  ev_fuel_types <- c('Battery electric', 'Plug-in hybrid electric') 
  
  provinces <- reactive({
    reactive_nmvr_data()$geo %>% unique() %>% sort()
  })
  
  min_year <- reactive({
    reactive_nmvr_data()$year %>% min()
  })
  
  max_year <- reactive({
    reactive_nmvr_data()$year %>% max()
  })
  
  # Plotting parameters for map
  bins <- c(0, 100, 10000, 100000, 500000, Inf)
  
  max_value <- reactive({
    reactive_nmvr_data()$amount %>% max()
  })
  
  ev_pal <- reactive({
    colorBin('Blues', domain = c(0, max_value()), bins = bins)
  })
    
  
  reactive_nmvr_data_year <- reactive({
    reactive_nmvr_data() %>% filter(year == input$plot_date)
  })
   
  reactive_total_new_vehicles <- reactive({
    reactive_nmvr_data_year() %>% filter(fuel_type == 'All fuel types') %>% pull(amount) %>% sum()
  })
  
  reactive_total_new_zev <- reactive({
    reactive_nmvr_data_year() %>% filter(fuel_type %in% ev_fuel_types) %>% pull(amount) %>% sum()
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
  
  # Basemap
  output$mymap <- renderLeaflet({
    leaflet() %>% 
        addTiles() %>% 
        addLayersControl(
          position = 'topright',
          overlayGroups = fuel_types(),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        hideGroup(fuel_types()[-1]) %>% 
        setView(-95, 55, zoom = 5) %>% 
        addLegend('topright', pal = ev_pal(), values = c(0, max_value()),
                  title = '<small>Amount of vehicles</small>')
  })
  
  output$slider_input_plot_date <- renderUI({
    sliderInput(
      'plot_date',
      label = 'Year',
      value = max_year(),
      min = min_year(),
      max = max_year(),
      step = 1,
      sep = '',
      animate = animationOptions(interval = 2000, loop = FALSE)
    )    
  })

  # Update map circle markers when date changes
  observeEvent(input$plot_date, {
    leafletProxy('mymap') %>% 
      clearMarkers()
    
    # Add circle markers for each group to the basemap
    for (fuel_typ in fuel_types()) {
      nmvr_data_filtered <- reactive_nmvr_data_year() %>% filter(fuel_type == fuel_typ)
      amount <- nmvr_data_filtered$amount
      
      leafletProxy('mymap') %>% 
        addCircleMarkers(
          data = nmvr_data_filtered, 
          lat = ~ latitude, 
          lng = ~ longitude,
          weight = 1, 
          radius = ~8*(amount)^(1/5),
          fillOpacity = 0.6, 
          fillColor = ~ev_pal()(amount),
          group = fuel_typ,
          label = sprintf("<strong>%s</strong><br/>Amount: %g", nmvr_data_filtered$geo, amount) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px", direction = "auto"))
    }
  })
  
  
  
  
  
  
  # NMVR Growth Tab --------------------------------------------------------------
  
  # UI Components
  output$select_input_province <- renderUI({
    selectInput(
      'province_select', 'Province',
      choices = provinces(),
      selected = 'Ontario'
    )
  })
  
  output$select_input_fuel_type <- renderUI({
    selectInput(
      'fuel_type_select', 'Fuel type',
      choices = fuel_types()
    )
  })
  
  
  reactive_nmvr_data_fuel_type <- reactive({
    reactive_nmvr_data() %>% filter(fuel_type == input$fuel_type_select)
  })
  
  reactive_nmvr_data_province <- reactive({
    reactive_nmvr_data() %>% filter(geo == input$province_select)
  })
  
  
  # Disable/Enable select options based on Group select and set color ramp
  observeEvent(input$nmvr_group_select, {
    if (input$nmvr_group_select == 'Province') {
      shinyjs::disable('province_select')
      shinyjs::enable('fuel_type_select')
    } 
    else if (input$nmvr_group_select == 'Fuel type') {
      shinyjs::enable('province_select')
      shinyjs::disable('fuel_type_select')
    }
  })
  
  # Output plots
  # TODO: Switch to plotly proxy to update plots 
  output$time_series_plot <- renderPlotly({
    if (input$nmvr_group_select == 'Province') {
      nmvr_data_plot <- reactive_nmvr_data_fuel_type()
      nmvr_data_plot %>% 
        plot_ly(x = ~year, y = ~cumsum, color = ~geo, colors = my_colors, type = 'scatter', mode = 'lines+markers') %>% 
        layout(
          xaxis = list(title = 'Year'),
          yaxis = list(title = 'Number of vehicles', range=c(0, 1.2 * max(nmvr_data_plot$cumsum))),
          title = paste0('Total number of ', input$fuel_type_select, ' vehicles over time')
        )
    } 
    else if (input$nmvr_group_select == 'Fuel type') {
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
    if (input$nmvr_group_select == 'Province') {
      nmvr_data_plot <- reactive_nmvr_data_fuel_type()
      nmvr_data_plot %>% 
        plot_ly(x = ~year, y = ~amount, color = ~geo, colors = my_colors, type = 'bar') %>% 
        layout(
          yaxis = list(title = 'Number of new vehicles'),
          title = paste0('Number of new ', input$fuel_type_select, ' vehicles'),
          barmode = 'stack')
    }
    else if (input$nmvr_group_select == 'Fuel type') {
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
  
  
  
  

  # NMVS Time View ----------------------------------------------------------
  
  sale_types <- c('Dollars', 'Units')
  
  origins_manufacture <- reactive({
    reactive_nmvs_data()$origin_of_manufacture %>%
      unique() %>% 
      sort()
  })
  
  vehicle_types <- reactive({
    reactive_nmvs_data()$vehicle_type %>% unique()
  })
  
  
  # UI components
  output$nmvs_select_sale_type <- renderUI({
    selectInput(
      inputId = 'nmvs_select_sale_type',
      label = 'Sale type',
      choices = sale_types,
      selected = sale_types[1]
    )
  })
  
  output$nmvs_select_origin_manufacture <- renderUI({
    selectInput(
      inputId = 'nmvs_select_origin_manufacture',
      label = 'Origin of manufacture',
      choices = origins_manufacture(),
      selected = 'Total, country of manufacture'
    )
  })
  
  output$nmvs_select_vehicle_type <- renderUI({
    selectInput(
      inputId = 'nmvs_select_vehicle_type',
      label = 'Vehicle Type',
      choices = vehicle_types()
    )
  })
  
  
  # Filtered data based on select inputs
  reactive_nmvs_data_filtered <- reactive({
    reactive_nmvs_data() %>% 
      filter(
        sales == input$nmvs_select_sale_type,
        origin_of_manufacture == input$nmvs_select_origin_manufacture,
        vehicle_type == input$nmvs_select_vehicle_type
      )
  })
  
  
  # Output plots
  output$nmvs_time_series_plot <- renderPlotly({
    reactive_nmvs_data_filtered() %>% 
      plot_ly(x = ~year, y = ~value, color = ~geo, colors = my_colors,
              type = 'scatter', mode = 'lines+markers', height=600, width=1550) %>% 
      layout(
        xaxis = list(title = 'Year'),
        yaxis = list(title = input$nmvs_select_sale_type),
        title = paste0('Number of new vehicle sales (', input$nmvs_select_sale_type, ')')
      )
  })
  
  
  

  # Generating Report -------------------------------------------------------
  
  output$btn_download_report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(nmvr_data = reactive_nmvr_data(), 
                     nmvs_data = reactive_nmvs_data())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
}
