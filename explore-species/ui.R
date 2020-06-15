#######################################################
##### -- California Coast Explorer R Shiny App -- #####
#######################################################
################################
##### -- User Interface -- #####
################################

##### Load packages
library(shiny)
library(leaflet)
library(shinydashboard)
library(plotly)
library(shinycssloaders)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(highcharter)
library(shinybusy)

options(shiny.suppressMissingContextError = TRUE)

source("withSpinner_edit.R")

page <- navbarPage(title = HTML("<a href='https://calcoast.dob.bio'><img src = 'DOBCalCoast_logo.png', height = '50'></a>"), 
           windowTitle = "DOB Cal Coast - Explore Species", 
           selected = "Explore Species",
           id="nav", theme = "style.css",
           
           tabPanel("Explore Places"),
           
           tabPanel("Explore Species", height = "100%", div(class="outer", 
                                                            
                                                          useShinyjs(),     ## Call to use shinyJS
                                                          
                                                          tags$head(
                                                            # Include custom CSS
                                                            includeScript("gomap.js"),
                                                            HTML("<link href='https://fonts.googleapis.com/css?family=Nunito:400,800&display=swap' rel='stylesheet'>")
                                                          ),
                                                          
                                                          tags$style(type="text/css",
                                                                     ".shiny-output-error { visibility: hidden; }",
                                                                     ".shiny-output-error:before { visibility: hidden; }"),
                                                          
                                                          tags$footer(img(src = "sponsor-logos.png", height = "35", align = "right")),
                                                          
                                                          fluidRow(height = "100%",
                                                                   column(width = 2, style = "padding-right: 0; margin-right: 0;", height = "100%",
                                                                          h3('Biodiversity Indicators', style = "padding-left: 10px;"),
                                                                          
                                                                          bsCollapse(id = "species_menu", open = "Population Trend", 
                                                                                     bsCollapsePanel("Population Trend", style = "primary", 
                                                                                                     selectizeInput("trend_species", "Select species", choices = ""),
                                                                                                     selectizeInput("trend_variable", "Add physical variable", choices = list(
                                                                                                       None = "none",
                                                                                                       Temperature = c("Minimum" = "temp_min", "Median" = "temp_median", "Maximum" = "temp_max"),
                                                                                                       Salinity = c("Minimum" = "salt_min", "Median" = "salt_median", "Maximum" = "salt_max"),
                                                                                                       Sea_level = c("Minimum" = "zeta_min", "Median" = "zeta_median", "Maximum" = "zeta_max")
                                                                                                       )
                                                                                                       ),
                                                                                                     bsButton(inputId = "plot_trend", label = "Show trend")
                                                                                     ),
                                                                                     bsCollapsePanel("Geographic Range", style = "primary", 
                                                                                                     selectizeInput("range_species", "Select Species", choices = ""),
                                                                                                     bsButton(inputId = "plot_range", label = "Show range")
                                                                                     ),
                                                                                     bsCollapsePanel("Range Change", style = "primary",
                                                                                                     selectizeInput("change_species", "Select Species", choices = ""),
                                                                                                     bsButton(inputId = "plot_change", label = "Show range change")
                                                                                     )
                                                                          )
                                                                          
                                                                   ),
                                                                   
                                                                   column(width = 10, 
                                                                          box(width = NULL, solidHeader = TRUE, 
                                                                          style = "background-color: white;
                                                                                   padding: 0 20px 80px 28px;
                                                                                   margin: 0;
                                                                                   position: relative;",
                                                                          div(id = "trend_output",
                                                                              fluidRow(h2(textOutput("trend_species_common_name_display"), style = "display: inline-block; float: left; padding-right:5px;"), h2(textOutput("selected_trend_species"), style = "display: inline-block; float: left; font-style: italic;")),
                                                                              fluidRow(  
                                                                               h3("Proportional rate of observation on iNaturalist", style = "display: inline-block; float: left;"),
                                                                                h4("[", style='display: inline-block; float: left; padding: 10px 0 0 5px; cursor: pointer;'),
                                                                                popify(a("what's this?", style='display: inline-block; float: left; padding: 17px 0 0 0;'), title = "",
                                                                                       content = "The proportional rate of observation on iNaturalist provides an estimate of change in the population size of a species across years for the corresponding region. It is calculated as the total number of observations of the focal species relative to the background rate of observation of a reference set of species frequently observed in the same places and times and by the same observers. (see <b><a>Methodology</a></b> for more details).", 
                                                                                       placement = "right", trigger = "click",  
                                                                                       options = list(container = "body")),
                                                                                h4("]", style='display: inline-block; float: left; padding: 10px 0 2px 0;')
                                                                              ),
                                                                              fluidRow(
                                                                                column(width = 5, 
                                                                                       fluidRow(style = "padding-right: 5px",
                                                                                                plotlyOutput("trend_plot_state", height = 300)
                                                                                       ),
                                                                                       fluidRow(style = "display: block; margin-left: auto; margin-right: auto; padding-top: 30px;",
                                                                                         htmlOutput("species_data", height = 150), 
                                                                                       )                                                                                                                                                                            
                                                                                       ),
                                                                                column(width = 4, style = "padding-left: 10px;",
                                                                                       plotlyOutput("trend_plot_north", height = 150),
                                                                                       plotlyOutput("trend_plot_central", height = 150),
                                                                                       plotlyOutput("trend_plot_south", height = 150)
                                                                                       ),
                                                                                column(width = 3, 
                                                                                       style = "padding-bottom:0; margin-bottom:0",
                                                                                       leafletOutput("region_map", height = 420)
                                                                                       )
                                                                              )
                                                                                ),
                                                                          hidden(div(id = "range_output", style = "max-height: 520px",
                                                                                     fluidRow(h2(textOutput("range_species_common_name_display"), style = "display: inline-block; float: left; padding-right:5px;"), h2(textOutput("selected_range_species"), style = "display: inline-block; float: left; font-style: italic;")),
                                                                                     fluidRow(  
                                                                                       h3("Geographic range and population density", style = "display: inline-block; float: left;"),
                                                                                       h4("[", style='display: inline-block; float: left; padding: 10px 0 0 5px;'),
                                                                                       popify(a("what's this?", style='display: inline-block; float: left; padding: 17px 0 0 0; cursor: pointer;'), title = "",
                                                                                              content = "Geographic ranges and population densities throughout the coast are jointly estimated by integrating species occurrence data from iNaturalist with Multi-agency rocky intertidal network (MARINe) survey data from 2012-2019. Maps show estimates of population density in terms of the number of individuals/colonies per 10 kilometers squared (see <b><a>Methodology</a></b> for more details).", 
                                                                                              placement = "right", trigger = "click",  
                                                                                              options = list(container = "body")),
                                                                                       h4("]", style='display: inline-block; float: left; padding: 10px 0 2px 0;')
                                                                                     ),
                                                                                     fluidRow(
                                                                                       column(7, tags$style(type = "text/css"),
                                                                                              leafletOutput("range_map", height = 500)
                                                                                              ),
                                                                                       column(3, 
                                                                                              selectInput("response_curve_predictor", "Modeled response to", choices = c("Minimum temperature" = "temp_min", 
                                                                                                                                                      "Median salinity" = "salt_median",
                                                                                                                                                      "Bathymetry" = "h_median",
                                                                                                                                                      "Maximum sea level" = "zeta_max")),
                                                                                              plotlyOutput("response_curve", height = 420)
                                                                                       )
                                                                                     )
                                                                                     # allow to binarize maps
                                                                          )
                                                                          ),
                                                                          hidden(div(id = "change_output", 
                                                                              fluidRow(h2(textOutput("change_species_common_name_display"), style = "display: inline-block; float: left; padding-right:5px;"), h2(textOutput("selected_change_species"), style = "display: inline-block; float: left; font-style: italic;")),
                                                                              fluidRow(  
                                                                                h3("Change in geographic range and population density across years", style = "display: inline-block; float: left;"),
                                                                                h4("[", style='display: inline-block; float: left; padding: 10px 0 0 5px;'),
                                                                                popify(a("what's this?", style='display: inline-block; float: left; padding: 17px 0 0 0; cursor: pointer;'), title = "",
                                                                                       content = "Geographic ranges and population densities each year are jointly estimated by integrating species occurrence data from iNaturalist with Multi-agency rocky intertidal network (MARINe) survey data for a given year. Maps show estimates of population density in terms of the number of individuals/colonies per 10 kilometers squared (see <b><a>Methodology</a></b> for more details).", 
                                                                                       placement = "right", trigger = "click",  
                                                                                       options = list(container = "body")),
                                                                                h4("]", style='display: inline-block; float: left; padding: 10px 0 2px 0;')
                                                                              ),
                                                                              fluidRow(
                                                                                column(width = 3, style = "padding-top: 10px;",
                                                                                       fluidRow(leafletOutput("predictions_map1"), style = "padding-right: 20px;"),
                                                                                       fluidRow(selectInput("selected_period1", label = "Select year", choices = "", width = "90%", selectize = FALSE), style = "padding-top: 20px;")
                                                                                ),
                                                                                column(width = 3, style = "padding-top: 10px;", 
                                                                                       fluidRow(leafletOutput("predictions_map2"), style = "padding-right: 20px;"),
                                                                                       fluidRow(selectInput("selected_period2", label = "Select year", choices = "", selected = "2019", width = "90%", selectize = FALSE), style = "padding-top: 20px;")
                                                                                ),
                                                                                column(width = 3,
                                                                                       fluidRow(leafletOutput("comparison_map"), style = "padding-top: 10px;"),
                                                                                       fluidRow(h4('Change in density between ', style = "display: inline-block; float: left;"), h4(textOutput("display_period"), style = "display: inline-block; float: left; padding-left: 5px;"), style = "padding: 15px 0 0 10px;")
                                                                                       )
                                                                                )
                                                                              # allow to binarize maps
                                                                              )
                                                                          )
                                                                          )
                                                                              )
                                                                          )

           )
           ),
           
           tabPanel("Methodology")

)


page[[3]][[1]]$children[[1]]$children[[2]]$children[[1]][[1]] <- tags$li(tags$a(href = "https://grap.shinyapps.io/dob-calcoast-places/", "Explore Places"))
page[[3]][[1]]$children[[1]]$children[[2]]$children[[1]][[3]] <- tags$li(tags$a(href = "http://calcoast.dob.bio/methodology.html", "Methodology", target = "_blank"))

page