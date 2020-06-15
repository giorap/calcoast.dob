#######################################################
##### -- California Coast Explorer R Shiny App -- #####
#######################################################
########################
##### -- Server -- #####
########################

##### Load packages
library(shiny)
library(leaflet)
library(plotly)
library(ggplot2)
library(sf)
library(dplyr)
library(htmltools)
library(htmlwidgets)
library(shinyWidgets)
library(pagedown)
library(raster)
library(rgdal)
library(rinat)
library(scales)
library(shinybusy)
library(splines)

##### -- Load data -- #####

#### Custom functions
source("functions.R")

#### Species predictions
species_predictions <- readRDS("data/sdm_predictions_byYear.rds")
species_trends <- readRDS("data/species_trends.rds")
# Remove unwanted species
species_trends <- species_trends[setdiff(names(species_trends), c("Ariolimax", "Harpaphe_haydeniana", "Oxidus_gracilis"))]
region_polygons <- readRDS("data/region_polygons.rds")
trend_species_common_names <- readRDS("data/trend_species_common_names.rds")
trend_species_common_names <- trend_species_common_names[-c(17, 74, 127)]
change_species_common_names <- readRDS("data/change_species_common_names.rds")
inverts_sdm <- readRDS("data/inverts_sdm.rds")
algae_sdm <- readRDS("data/algae_sdm.rds")
temp_means_byRegion <- readRDS("data/temp_means_byRegion.rds")
salt_means_byRegion <- readRDS("data/salt_means_byRegion.rds")
zeta_means_byRegion <- readRDS("data/zeta_means_byRegion.rds")

#### Shiny app body
function(input, output, session) {
  
  show_modal_spinner("circle", color = "#024b6c") # show the modal window
  
  trend_species_list <- reactiveValues()

  observe ({
    trend_species_list <- as.list(names(species_trends))
    names(trend_species_list) <- paste0(trend_species_common_names, " (", gsub("_", " ", names(species_trends)), ")")
    updateSelectizeInput(session,
                      inputId = "trend_species",
                      choices = trend_species_list, 
                      options = list(create = TRUE)
    )
  })

  output$trend_species_common_name_display <- renderText({
    trend_species_common_names[which(names(species_trends) == input$trend_species)]
    })
  
  range_species_list <- reactiveValues()
  
  observe ({
    range_species_list <- as.list(names(species_predictions))
    names(range_species_list) <- change_species_common_names
    updateSelectizeInput(session,
                         inputId = "range_species",
                         choices = range_species_list, 
                         options = list(create = TRUE)
    )
  })
  
  output$range_species_common_name_display <- renderText({
    change_species_common_names[which(names(species_predictions) == input$range_species)]
  })
  
  change_species_list <- reactiveValues()
  
  observe ({
    change_species_list <- as.list(names(species_predictions))
    names(change_species_list) <- change_species_common_names
    updateSelectizeInput(session,
                         inputId = "change_species",
                         choices = change_species_list, 
                         options = list(create = TRUE)
    )
  })
  
  output$change_species_common_name_display <- renderText({
    change_species_common_names[which(names(species_predictions) == input$change_species)]
  })
  
  observeEvent(input$plot_trend, {
    shinyjs::show(id = "trend_output")
    shinyjs::hide(id = "change_output")
    shinyjs::hide(id = "range_output")
  })
  
  observeEvent(input$plot_range, {
    shinyjs::show(id = "range_output")
    shinyjs::hide(id = "trend_output")
    shinyjs::hide(id = "change_output")
  })
  
  observeEvent(input$plot_change, {
    shinyjs::show(id = "change_output")
    shinyjs::hide(id = "trend_output")
    shinyjs::hide(id = "range_output")
  })
  
  output$selected_trend_species <- renderText({
    gsub("_", " ", input$trend_species) %>% as.character() 
  })
  
  output$selected_range_species <- renderText({
    gsub("_", " ", input$range_species) %>% as.character() 
  })
  
  output$selected_change_species <- renderText({
    gsub("_", " ", input$change_species) %>% as.character() 
  })
  
  last_species_records <- reactive({
    get_inat_obs(query=gsub("_", " ", input$trend_species), maxresults = 6, quality = "research")
  })
  
  output$species_data <- renderText({
    sp_res <- last_species_records()
    species_name <- sp_res$scientific_name[1]
    species_id <- sp_res$taxon_id[1]
    sp_images <- sp_res[c("url", "image_url")]
    
    paste0(
      "<style type='text/css' media='screen'>
      .inat-widget { font-family: Georgia, serif; padding: 0; line-height: 1; width: 100%; display: block; margin-left: auto; margin-right: auto;} 
      .inat-widget-header {margin-bottom: 10px;}
      .inat-widget td {vertical-align: top; padding-bottom: 10px;}
      .inat-label { color: #888; }
      .inat-meta { font-size: smaller; margin-top: 3px; line-height: 1.2;}
      .inat-observation-body, .inat-user-body { padding-left: 10px; }
      .inat-observation-image {text-align: center; height:55px;}
      .inat-observation-image, .inat-user-image { width: 55px; display: inline-block; height:55px;}
      .inat-observation-image img, .inat-user-image img { width: 55px; height: 55px;}
      .inat-observation-image img { vertical-align: middle; }
      .inat-widget-small, .inat-observation-image { display:block; float: left; margin: 0 6px 6px 0; }
      .inat-label, .inat-value, .inat-user { font-family: 'Trebuchet MS', Arial, sans-serif; }
      .inat-user-body {vertical-align: middle;}
      .inat-widget td.inat-user-body {vertical-align: middle;}
      .inat-widget .inat-footer td.inat-value {vertical-align: middle; padding-left: 10px;}
      </style>
       <div class='inat-widget'>
        <div class='inat-widget-header'> 
        <p style='clear:left; font-size:12px; font-family: 'Nunito', sans-serif; color: #024b6c;'>
        <a href='https://www.inaturalist.org/observations?order_by=observed_on&quality_grade=research&place_id=96687&taxon_id=", species_id, "', target = 'blank'>View all <i>", species_name, "</i> observations on <img alt='iNaturalist' src='https://www.inaturalist.org/assets/logo-small-c1caecf8d38ed3d4ddeb7a1da076ec97.png' height='18' style='padding-bottom:5px;'/></a>
        </p>
        </div>
        <div class='inat-widget-small'>
         <a class='inat-observation-image' href='", sp_images$url[1], "' target = '_blank'><img border='0' src='", sp_images$image_url[1], "' alt='Square'/></a>
         <a class='inat-observation-image' href='", sp_images$url[2], "' target = '_blank'><img border='0' src='", sp_images$image_url[2], "' alt='Square' /></a>
         <a class='inat-observation-image' href='", sp_images$url[3], "' target = '_blank'><img border='0' src='", sp_images$image_url[3], "' alt='Square' /></a>
         <a class='inat-observation-image' href='", sp_images$url[4], "' target = '_blank'><img border='0' src='", sp_images$image_url[4], "' alt='Square' /></a>
         <a class='inat-observation-image' href='", sp_images$url[5], "' target = '_blank'><img border='0' src='", sp_images$image_url[5], "' alt='Square' /></a>
         <a class='inat-observation-image' href='", sp_images$url[6], "' target = '_blank'><img border='0' src='", sp_images$image_url[6], "' alt='Square' /></a>
        <div style='visibility:hidden; clear:both; height:0;'>&nbsp;</div>
        </div>"
      )
    
  })
  
  #### Generate range map
  output$range_map <- renderLeaflet(map_predictions(species_predictions[[input$range_species]]$'overall'))
  
  output$response_curve <- renderPlotly({
    if (input$range_species %in% names(inverts_sdm$data$PO_data)) sdm <- inverts_sdm
    if (input$range_species %in% names(algae_sdm$data$PO_data)) sdm <- algae_sdm
    p <- plot_response_curve(sdm, focal_species_name = input$range_species, predictor = input$response_curve_predictor, xlabel = input$response_curve_predictor)
    gg <- plotly_build(p) %>% config(displayModeBar = F) %>%
      config(displayModeBar = FALSE) %>%
      layout(font = list(family = "Helvetica", size = 9), 
             xaxis = list(titlefont = list(size = 9),
                          tickfont = list(size = 9)),
             yaxis = list(titlefont = list(size = 9),
                          tickfont = list(size = 9))
             )
    gg$x$layout$margin <- list(t = 0, r = 0, b = 0, l = 0)
    gg$x$data[[1]]$hoverinfo <- "none"
    gg$x$data[[2]]$hoverinfo <- "none"
    gg$x$layout$xaxis$title$font <- 5
    gg$x$layout$yaxis$title$font <- 5
    gg
  })
  
  observe ({
    updateSelectInput(session,
                         inputId = "selected_period1",
                         choices = setdiff(names(species_predictions[[input$change_species]]), "overall"), 
                         selected = setdiff(names(species_predictions[[input$change_species]]), "overall")[1]
    )
  })
  
  observe ({
    updateSelectInput(session,
                         inputId = "selected_period2",
                         choices = setdiff(names(species_predictions[[input$change_species]]), "overall"), 
                         selected = setdiff(names(species_predictions[[input$change_species]]), "overall")[length(setdiff(names(species_predictions[[input$change_species]]), "overall"))]
    )
  })
  
  output$display_period <- renderText({
    paste0(input$selected_period1, " and ", input$selected_period2)
  })
  
  output$display_period2 <- renderText({
  })
  
  predictions1 <- reactiveValues(r = NULL)
  predictions2 <- reactiveValues(r = NULL)
  predictions_comparison <- reactiveValues(r = NULL)
  pred_data <- reactiveValues()

  observeEvent({input$change_species
               input$selected_period1
               input$selected_period2}, {
     predictions1$r <- species_predictions[[input$change_species]][[as.character(input$selected_period1)]]
     predictions1$r[which(predictions1$r[] < quantile(predictions1$r[], 0.1, na.rm = TRUE))] <- NA
     predictions2$r <- species_predictions[[input$change_species]][[as.character(input$selected_period2)]]
     predictions2$r[which(predictions2$r[] < quantile(predictions2$r[], 0.1, na.rm = TRUE))] <- NA
     predictions_comparison$r <- predictions2$r - predictions1$r
     predictions_comparison$r[which(predictions_comparison$r[] < 0.01 & predictions_comparison$r[] > -0.01)] <- NA
   })
  
  #### Generate predictions map 1
  output$predictions_map1 <- renderLeaflet(map_predictions(predictions1$r))
  
  #### Generate predictions map 2
  output$predictions_map2 <- renderLeaflet(map_predictions(predictions2$r))
  
  #### Generate map comparing prediction maps 1 and 2
  output$comparison_map <- renderLeaflet({
    
    pal <- colorBin(palette = colorRamp(c("firebrick3", rgb(1, 1, 1, 0.1), "#024b6c"), interpolate = "spline"), domain = c(max(abs(min(raster::values(predictions_comparison$r), na.rm = TRUE)), 
                                               abs(max(raster::values(predictions_comparison$r), na.rm = TRUE)))*-1,
                                           max(abs(min(raster::values(predictions_comparison$r), na.rm = TRUE)), 
                                               abs(max(raster::values(predictions_comparison$r), na.rm = TRUE)))),
                                           bins = 5,
                        na.color = "transparent")
    
    leaflet(options = leafletOptions(zoomDelta = 0.5, zoomSnap = 0, attributionControl = FALSE)) %>% 
      fitBounds(-124.5351, 32.1330, -115.89794, 42.42072) %>%
      addProviderTiles(providers$Esri.WorldShadedRelief) %>%
      addRasterImage(predictions_comparison$r,
                     colors = pal
      ) %>%
      addLegend(pal = pal, values = raster::values(predictions_comparison$r),
                title = "Density <br> change")
    
  })

  output$trend_plot_state <- renderPlotly({
    if (input$trend_variable == "none"){
      p <- plot_trends_data(species_name = input$trend_species, region = "Statewide", dat = species_trends)
    } else {
      if (input$trend_variable %in% c("temp_min", "temp_median", "temp_max")) pred_data <- temp_means_byRegion
      if (input$trend_variable %in% c("salt_min", "salt_median", "salt_max")) pred_data <- salt_means_byRegion
      if (input$trend_variable %in% c("zeta_min", "zeta_median", "zeta_max")) pred_data <- zeta_means_byRegion 
      p <- plot_trends_data(species_name = input$trend_species, region = "Statewide", dat = species_trends, predictor_data = pred_data[["Statewide"]][, input$trend_variable], predictor_label = input$trend_variable)
    }
    remove_modal_spinner() # show the modal window
    p
  })
  
  output$trend_plot_north <- renderPlotly({
    if (input$trend_variable == "none"){
      p <- plot_trends_data(species_name = input$trend_species, region = "North", dat = species_trends)
    } else {
      if (input$trend_variable %in% c("temp_min", "temp_median", "temp_max")) pred_data <- temp_means_byRegion
      if (input$trend_variable %in% c("salt_min", "salt_median", "salt_max")) pred_data <- salt_means_byRegion
      if (input$trend_variable %in% c("zeta_min", "zeta_median", "zeta_max")) pred_data <- zeta_means_byRegion 
      p <- plot_trends_data(species_name = input$trend_species, region = "North", dat = species_trends, predictor_data = pred_data[["Statewide"]][, input$trend_variable])
    }
    p  
    })
  
  output$trend_plot_central <- renderPlotly({
    if (input$trend_variable == "none"){
      p <- plot_trends_data(species_name = input$trend_species, region = "Central", dat = species_trends)
    } else {
      if (input$trend_variable %in% c("temp_min", "temp_median", "temp_max")) pred_data <- temp_means_byRegion
      if (input$trend_variable %in% c("salt_min", "salt_median", "salt_max")) pred_data <- salt_means_byRegion
      if (input$trend_variable %in% c("zeta_min", "zeta_median", "zeta_max")) pred_data <- zeta_means_byRegion 
      p <- plot_trends_data(species_name = input$trend_species, region = "Central", dat = species_trends, predictor_data = pred_data[["Statewide"]][, input$trend_variable])
    }
    p  
  })
  
  output$trend_plot_south <- renderPlotly({
    if (input$trend_variable == "none"){
      p <- plot_trends_data(species_name = input$trend_species, region = "South", dat = species_trends)
    } else {
      if (input$trend_variable %in% c("temp_min", "temp_median", "temp_max")) pred_data <- temp_means_byRegion
      if (input$trend_variable %in% c("salt_min", "salt_median", "salt_max")) pred_data <- salt_means_byRegion
      if (input$trend_variable %in% c("zeta_min", "zeta_median", "zeta_max")) pred_data <- zeta_means_byRegion 
      p <- plot_trends_data(species_name = input$trend_species, region = "South", dat = species_trends, predictor_data = pred_data[["Statewide"]][, input$trend_variable])
    }
    p 
  })
  
  output$region_map <- renderLeaflet({
    #### Color palette for MPA polygons
    col_pal <- colorFactor("Dark2", factor(region_polygons@data$Region, levels = c("North", "Central", "South")))
    
    leaflet(options = leafletOptions(attributionControl = FALSE, zoomControl = FALSE)) %>% 
      setView(-120.8282,
              37.41643,
              zoom=5 # set to 10 as 9 is a bit too zoomed out
      ) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = region_polygons,
                  color = "#333333",
                  label = region_polygons$Region,
                  fillColor = ~col_pal(factor(region_polygons@data$Region, levels = c("North", "Central", "South"))),
                  fillOpacity = 0.5,
                  weight = 1
      ) %>%
      addLegend("topright",
                pal = col_pal, values = factor(region_polygons@data$Region, levels = c("North", "Central", "South")),
                opacity = 1, title = "Coastal Regions"
      )
  })
  
}

