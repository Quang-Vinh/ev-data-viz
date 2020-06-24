#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Based off of https://github.com/eparker12/nCoV_tracker


# TODO: Rename some label IDs to specify NMVR


library(leaflet)
library(plotly)
library(RColorBrewer)
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)


ui <- bootstrapPage(
  shinyjs::useShinyjs(),
  
  navbarPage(theme = shinytheme('flatly'), collapsible = TRUE, 'VRF Data Visualisation', id='nav',
             
             tabPanel('NMVR Map View',
                      div(class='outer', tags$head(includeCSS('styles.css')),
                          leafletOutput('mymap', width = '100%', height = '100%'),
                          absolutePanel(
                            id = 'controls', class = 'panel panel-default', top = 80, left = 20, width = 250, fixed = TRUE,
                            draggable = TRUE, height = 'auto',
                            
                            h3(textOutput('reactive_total_new_vehicles'), align = 'right'),
                            span(h4(textOutput('reactive_total_new_gv'), align = 'right'), style="color:#cc4c02"),
                            span(h4(textOutput('reactive_total_new_zev'), align = 'right'), style="color:#006d2c"),
                            uiOutput('slider_input_plot_date')
                          ) # absolute Panel
                      ) # div outer
             ), # Tab panel
             
             tabPanel('NMVR Time view',
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            'nmvr_group_select', 'Group by',
                            choices = c('Province', 'Fuel type')),
                          uiOutput('select_input_province'),
                          uiOutput('select_input_fuel_type'),
                          width = 2
                        ),
                        mainPanel(
                          plotlyOutput('time_series_plot'),
                          plotlyOutput('bar_chart_plot'),
                          plotlyOutput('sunburst_plot')
                        )
                      ) # sidebar layout
             ), # tab panel
             
             tabPanel('NMVS Time View',
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput('nmvs_select_sale_type'),
                          uiOutput('nmvs_select_vehicle_type'),
                          uiOutput('nmvs_select_origin_manufacture'),
                          width = 2
                        ),
                        mainPanel(
                          plotlyOutput('nmvs_time_series_plot')
                        )
                      ) # side bar layout
             ) #tab panel
             
  ) # navbar page
) # bootstrap page
