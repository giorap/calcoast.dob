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
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(highcharter)
library(shinybusy)
options(shiny.suppressMissingContextError = TRUE)

source("withSpinner_edit.R")

page <- navbarPage(title = HTML("<a href='https://calcoast.dob.bio'><img src = 'DOBCalCoast_logo.png', height = '50'></a>"), 
           windowTitle = "DOB Cal Coast - Places", 
           id="nav", theme = "style.css",
           
           tabPanel("Explore Places", height = "100%", div(class="outer", 
                                                          
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
                                                                   column(width = 2, style = "padding-right: 0; margin-right: 0", height = "100%",
                                                                          h3(HTML('Biodiversity Indicators <br /> for the Intertidal Zone'), style = "padding-left: 10px;"),
                                                                          
                                                                          bsCollapse(id = "report_menu", open = "Report menu",
                                                                                     bsCollapsePanel("Richness", style = "primary",
                                                                                                     selectInput("richness_taxon", "Select Group", c("All species" = "all", 
                                                                                                                                                        "Mollusks" = "Mollusca", 
                                                                                                                                                        "Arthropods" = "Arthropoda", 
                                                                                                                                                        "Echinoderms" = "Echinodermata", 
                                                                                                                                                        "Anemones, jellies and corals" = "Cnidaria", 
                                                                                                                                                        "Sea stars" = "Asteroidea", 
                                                                                                                                                        "Kelps and seaweeds" = "Chromista",
                                                                                                                                                        "Nudibranchs" = "Nudibranchia")),
                                                                                                     prettySwitch(inputId = "map_richness", label = HTML("Add to map"), value = FALSE, status = "primary")
                                                                                     ),
                                                                                     bsCollapsePanel("Uniqueness", style = "primary",
                                                                                                     selectInput("uniqueness_taxon", "Select Group", c("All species" = "all", 
                                                                                                                                                          "Mollusks" = "Mollusca", 
                                                                                                                                                          "Arthropods" = "Arthropoda", 
                                                                                                                                                          "Echinoderms" = "Echinodermata", 
                                                                                                                                                          "Anemones, jellies and corals" = "Cnidaria", 
                                                                                                                                                          "Sea stars" = "Asteroidea", 
                                                                                                                                                          "Kelps and seaweeds" = "Chromista",
                                                                                                                                                          "Nudibranchs" = "Nudibranchia")),
                                                                                                     prettySwitch(inputId = "map_uniqueness", label = HTML("Add to map"), value = FALSE, status = "primary")
                                                                                     ),
                                                                                     bsCollapsePanel("Rarity", style = "primary",
                                                                                                     prettySwitch(inputId = "map_rarity", label = HTML("Add to map"), value = FALSE, status = "primary")
                                                                                     ),
                                                                                     bsCollapsePanel("Irreplaceability", style = "primary",
                                                                                                     prettySwitch(inputId = "map_priority", label = HTML("Add to map"), value = FALSE, status = "primary")
                                                                                     ),
                                                                                     bsCollapsePanel("Change", style = "primary",
                                                                                                     prettySwitch(inputId = "map_change", label = HTML("Add to map"), value = FALSE, status = "primary")
                                                                                     ),
                                                                                     bsCollapsePanel("Create a Report", style = "info",
                                                                                                     h5("Click on a boundary on the map, then hit the 'Create a report' button"),
                                                                                                     bsButton(inputId = "generate_report", label = "Create a report")
                                                                                     )
                                                                                     ),

                                                                          bsButton(inputId = "clear_map", label = "  Clear Map", icon = icon("close"), block = TRUE, style = "primary")
                                                                          
                                                                          ),
                                                                   column(width = 10,
                                                                          box(width = NULL, solidHeader = TRUE, tags$style(type = "text/css", "#cal_coast_map {height: calc(97vh - 80px) !important;}"),
                                                                              leafletOutput("cal_coast_map")                                                                              )
                                                                          ),

                                                                   
                                                                   hidden(
                                                                     div(id = "richnessPanel",
                                                                         fluidRow(
                                                                           absolutePanel(id = "cond_panel",
                                                                                         class = "panel panel-default",
                                                                                         fixed = TRUE,
                                                                                         draggable = TRUE, top = 125, left = "auto", right = 0, bottom = 25,
                                                                                         width = 500, 
                                                                                         style = "overflow-y:scroll; 
                                                                                                  max-height: 600px;
                                                                                                  padding: 10px 40px 10px 40px;
                                                                                                  background-color: rgba(255, 255, 255, 0.9);
                                                                                                  ",
                                                                                         fluidRow(
                                                                                           h3("Species Richness"),
                                                                                           column(width = 4, 
                                                                                                  h5("Percentage of all species observed network-wide that have been observed in this polygon."), 
                                                                                                  style = "padding: 0;"),
                                                                                           column(width = 8, highchartOutput("richness_donut", height = 250, width = 250), style = "padding: 0;"),
                                                                                         ),
                                                                                         fluidRow(
                                                                                         column(width = 4, 
                                                                                                         fluidRow(valueBoxOutput("richness_estimate")),
                                                                                                         fluidRow(valueBoxOutput("richness_confidence")), 
                                                                                                style = "padding: 0;"
                                                                                                ),
                                                                                         column(width = 8,
                                                                                                         plotlyOutput("richness_plot", height = 300)
                                                                                                ),
                                                                                         )
                                                                                                
                                                                           )
                                                                         )
                                                                     ) 
                                                                   ),
                                                                   hidden(
                                                                     div(id = "uniquenessPanel",
                                                                         fluidRow(
                                                                           absolutePanel(id = "cond_panel2",
                                                                                         class = "panel panel-default", fixed = TRUE,
                                                                                         draggable = TRUE, top = 125, left = "auto", right = 0, bottom = 25,
                                                                                         style = "overflow-y:scroll;
                                                                                                  max-height: 600px;
                                                                                                  padding: 10px 40px 10px 40px;
                                                                                                  background-color: rgba(255, 255, 255, 0.9);
                                                                                                  ",
                                                                                         width = 500, 
                                                                                           fluidRow(
                                                                                             h3("Biodiversity Uniqueness"),
                                                                                             column(width = 4, 
                                                                                                    h5("Biodiversity Uniqueness measures the distinctiveness of the ecological community in this polygon compared with all others."), 
                                                                                                    h5("Values range from 100% for communities completely distinct from all others to 0% for communities identical to all others."),
                                                                                                    style = "padding: 0;"),
                                                                                             column(width = 8, highchartOutput("uniqueness_donut", height = 250, width = 250), style = "padding: 0;"),
                                                                                           )
                                                                         )
                                                                     )
                                                                   )
                                                                   ),
                                                                   hidden(
                                                                     div(id = "rarityPanel",
                                                                         fluidRow(
                                                                           absolutePanel(id = "cond_panel3",
                                                                                         class = "panel panel-default", fixed = TRUE,
                                                                                         draggable = TRUE, top = 125, left = "auto", right = 0, bottom = 25,
                                                                                         style = "overflow-y:scroll; 
                                                                                                  max-height: 600px;
                                                                                                  padding: 10px 40px 10px 40px;
                                                                                                  background-color: rgba(255, 255, 255, 0.9);
                                                                                                  ",
                                                                                         width = 500, 
                                                                                         fluidRow(
                                                                                           h3("Species Rarity"),
                                                                                           column(width = 4, 
                                                                                                  h5("Species Rarity measures the richness of species weighted by how rare they are."),
                                                                                                  h5("Values indicate the polygon's species rarity rank expressed as a percentile, with 100% being the polygon with the most rare species."),
                                                                                                  style = "padding: 0;"),
                                                                                           column(width = 8, highchartOutput("rarity_donut", height = 250, width = 250), style = "padding: 0;"),
                                                                                         )
                                                                           )
                                                                         )
                                                                     )
                                                                   ),
                                                                   hidden(
                                                                     div(id = "priorityPanel",
                                                                         fluidRow(
                                                                           absolutePanel(id = "cond_panel4",
                                                                                         class = "panel panel-default",
                                                                                         fixed = TRUE,
                                                                                         draggable = TRUE, top = 125, left = "auto", right = 0, bottom = 25,
                                                                                         style = "overflow-y:scroll; 
                                                                                                  max-height: 600px;
                                                                                                  padding: 10px 40px 10px 40px;
                                                                                                  background-color: rgba(255, 255, 255, 0.9);
                                                                                                  ",
                                                                                         width = 500,  
                                                                                         fluidRow(
                                                                                           h3("Biodiversity Irreplaceability"),
                                                                                           column(width = 4, 
                                                                                                  h5("Biodiversity Irreplaceability measures how key this polygon is - as a complement to all others - to ensure that all species across the network are sufficiently protected."), 
                                                                                                  h5("Values indicate the polygon's irreplaceability rank expressed as a percentile, with 100% being the most and 0% the least important."),
                                                                                                  style = "padding: 0;"),
                                                                                           column(width = 8, highchartOutput("priority_donut", height = 250, width = 250), style = "padding: 0;"),
                                                                                         )
                                                                           )
                                                                         )
                                                                     )
                                                                   ),
                                                                   hidden(
                                                                     div(id = "changePanel",
                                                                         fluidRow(
                                                                           absolutePanel(id = "cond_panel5",
                                                                                         class = "panel panel-default",
                                                                                         fixed = TRUE,
                                                                                         draggable = TRUE, top = 125, left = "auto", right = 0, bottom = 25,
                                                                                         style = "overflow-y:scroll; 
                                                                                                  max-height: 600px;
                                                                                                  padding: 10px 40px 10px 40px;
                                                                                                  background-color: rgba(255, 255, 255, 0.9);
                                                                                                  ",
                                                                                         width = 500, 
                                                                                         fluidRow(
                                                                                           h3("Ecological Community Change"),
                                                                                           h5("Ecological Community Change measures the percentage change in ecological community composition and evenness from year to year."),
                                                                                           h5("Negative values indicate less change between years than the average background rate."), 
                                                                                           fluidRow(plotlyOutput("change_plot", height = 300), style = "padding: 10px 0 0 0;"),
                                                                                           style = "padding: 10px 40px 10px 40px;"
                                                                                         )
                                                                           )
                                                                         )
                                                                     )
                                                                   ),
                                                                   
                                                                   hidden(
                                                                     div(id = "reportPanel",
                                                                         fluidRow(
                                                                           absolutePanel(id = "cond_panel6",
                                                                                         class = "panel panel-default",
                                                                                         fixed = TRUE,
                                                                                         draggable = FALSE, top = 125, left = "auto", right = 0, bottom = 25,
                                                                                         style = "overflow-y:scroll;
                                                                                                  max-height: 600px;
                                                                                                  background-color: rgba(255, 255, 255, 0.9);
                                                                                                  padding: 10px 40px 10px 40px;",
                                                                                         width = 500, 
                                                                                         fluidRow(
                                                                                           h2("Summary Report", style = "padding: 0;"),
                                                                                         ),
                                                                                         fluidRow(
                                                                                           h3("Species Richness"),
                                                                                           column(width = 3, 
                                                                                                  h5("Percentage of all species observed network-wide that have been observed in this polygon"), 
                                                                                                  style = "padding: 0;"),
                                                                                           column(width = 9, highchartOutput("richness_donut_forReport", height = 250), style = "padding: 0;"),
                                                                                         ),
                                                                                         fluidRow(
                                                                                           h3("Biodiversity Rarity"),
                                                                                           column(width = 3, 
                                                                                                  h5("Species Rarity measures the richness of species weighted by how rare they are."),
                                                                                                  h5("Values indicate the polygon's species rarity rank expressed as a percentile, with 100% being the polygon with the most rare species."),                                                                                                  style = "padding: 0;"),
                                                                                           column(width = 9, highchartOutput("rarity_donut_forReport", height = 250), style = "padding: 0;"),
                                                                                         ),
                                                                                         fluidRow(
                                                                                           h3("Biodiversity Uniqueness"),
                                                                                           column(width = 3, 
                                                                                                  h5("Biodiversity Uniqueness measures the distinctiveness of the ecological community in this polygon compared with all others."), 
                                                                                                  h5("Values range from 100% for communities completely distinct from all others to 0% for communities identical to all others."),                                                                                                  style = "padding: 0;"),
                                                                                           column(width = 9, highchartOutput("uniqueness_donut_forReport", height = 250), style = "padding: 0;"),
                                                                                         ),
                                                                                         fluidRow(
                                                                                           h3("Biodiversity Irreplaceability"),
                                                                                           column(width = 3, 
                                                                                                  h5("Biodiversity Irreplaceability measures how key this polygon is - compared with all others - to ensure that all species across the network are sufficiently protected."), 
                                                                                                  h5("Values indicate the polygon's irreplaceability rank expressed as a percentile, with 100% being the most and 0% the least important."),                                                                                                  style = "padding: 0;"),
                                                                                           column(width = 9, highchartOutput("priority_donut_forReport", height = 250), style = "padding: 0;"),
                                                                                         ),
                                                                                         fluidRow(
                                                                                           h3("Ecological Community Change"),
                                                                                           column(width = 12, style = "padding: 0;",
                                                                                                  h5("Ecological Community Change measures the percentage change in ecological community composition and evenness from year to year."),
                                                                                                  h5("Negative values indicate less change between years than the average background rate."), 
                                                                                           plotlyOutput("change_plot_forReport", height = 300)
                                                                                           )
                                                                                         )
                                                                                         )
                                                                           )
                                                                         )
                                                                     )
                                                                   )
                                                          )
                                                          
                                                          
                                              
           ),
           
           tabPanel("Explore Species"),
           
           tabPanel("Methodology")
           
)

page[[3]][[1]]$children[[1]]$children[[2]]$children[[1]][[2]] <- tags$li(tags$a(href = "https://grap.shinyapps.io/dob-calcoast-species/", "Explore Species"))
page[[3]][[1]]$children[[1]]$children[[2]]$children[[1]][[3]] <- tags$li(tags$a(href = "http://calcoast.dob.bio/methodology.html", "Methodology", target = "_blank"))

page


