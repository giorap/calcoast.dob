#' ---
#' title: DOB Cal Coast <br> <strong> Explore Places Shiny App - Server.R
#' ---
#' 
#' ### Load packages
library(shiny)
library(leaflet)
library(plotly)
library(ggplot2)
library(sf)
library(dplyr)
library(htmltools)
library(htmlwidgets)
library(shinyWidgets)
library(highcharter)
library(pagedown)
library(scales)
#'
#' ### Load data
#' #### Custom functions
source("functions.R")
#' #### Places
#' ##### Watersheds
Watersheds <- readRDS("data/watershed_polygons.rds")
#' ##### MPAs
MPAs <- readRDS("data/MPA_polygons.rds")
# MPAs@data <- MPAs@data %>% dplyr::distinct(.keep_all = TRUE)
#' ##### Counties
Counties <- readRDS("data/county_polygons.rds")
#' ##### Hexagons
Hexagons <- readRDS("data/hexagon_polygons.rds")
#' ##### Coastal access points
coastal_access_df_overlap <- readRDS("data/coastal_access_df_overlap.rds")
#' Temporal change data
temporal_change <- purrr::map(list.files("data", pattern = "temporal_change", full.names = TRUE), readRDS)
names(temporal_change) <- c("Counties", "Hexagons", "Marine Protected Areas", "Watersheds")
#' Add species rarity proportion field
Watersheds@data <- Watersheds@data %>% 
  dplyr::mutate(species_rarity_proportion = species_rarity/max(species_rarity, na.rm = TRUE)) %>% 
  dplyr::left_join(temporal_change$Watersheds$place_stability %>% group_by(place) %>% summarise(mean_change = mean(mean)) %>% dplyr::mutate(mean_change = mean_change - temporal_change$Watersheds$global_stability["mean"]), by = c("ID" = "place")) %>% 
  distinct(.keep_all = TRUE)
MPAs@data <- MPAs@data %>% 
  dplyr::mutate(species_rarity_proportion = species_rarity/max(species_rarity, na.rm = TRUE)) %>% 
  dplyr::left_join(temporal_change$'Marine Protected Areas'$place_stability %>% group_by(place) %>% summarise(mean_change = mean(mean)) %>% dplyr::mutate(mean_change = mean_change - temporal_change$'Marine Protected Areas'$global_stability["mean"]), by = c("ID" = "place")) %>% 
  distinct(.keep_all = TRUE)
Counties@data <- Counties@data %>% 
  dplyr::mutate(species_rarity_proportion = species_rarity/max(species_rarity, na.rm = TRUE)) %>%  
  dplyr::left_join(temporal_change$Counties$place_stability %>% group_by(place) %>% summarise(mean_change = mean(mean)) %>% dplyr::mutate(mean_change = mean_change - temporal_change$Counties$global_stability["mean"]), by = c("ID" = "place")) %>% 
  distinct(.keep_all = TRUE)
Hexagons@data <- Hexagons@data %>% 
  dplyr::mutate(species_rarity_proportion = species_rarity/max(species_rarity, na.rm = TRUE)) %>% 
  dplyr::left_join(temporal_change$Hexagons$place_stability %>% group_by(place) %>% summarise(mean_change = mean(mean)) %>% dplyr::mutate(mean_change = mean_change - temporal_change$Hexagons$global_stability["mean"]), by = c("ID" = "place")) %>% 
  distinct(.keep_all = TRUE)
#'
#' #### Shiny app body
function(input, output, session) {

  ##### -- Compare Counties tab -- #####

  #### Generate counties map
  output$cal_coast_map <- renderLeaflet({
    show_modal_spinner("circle", color = "#024b6c") # show the modal window
    leaflet(options = leafletOptions(zoomDelta = 0.5, zoomSnap = 0, attributionControl = FALSE)) %>% 
      fitBounds(-124.5351, 32.1330, -115.89794, 42.42072) %>%
      clearControls() %>%
      clearShapes() %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Ocean Basemap") %>%
      addPolygons(data = Watersheds,
                  layerId = ~ID,
                  color = "black",
                  fillColor = grey(0.5),
                  fillOpacity = 0.5,
                  smoothFactor = 0.5,
                  weight = 0.6,
                  highlightOptions = highlightOptions(color = "white", fillColor = "tomato", weight = 2, bringToFront = TRUE, fillOpacity = 0.4),
                  label = Watersheds$ID, labelOptions = labelOptions(textOnly = FALSE, direction = "right", textsize = "12px", sticky = TRUE, style = list("color" = "black"), offset = c(-5, 0)),
                  group = "Watersheds"
      ) %>%
      addPolygons(data = MPAs,
                  layerId = ~ID,
                  color = "black",
                  fillColor = grey(0.5),
                  fillOpacity = 0.5,
                  smoothFactor = 0.5,
                  weight = 0.6,
                  highlightOptions = highlightOptions(color = "white", fillColor = "tomato", weight = 2, bringToFront = TRUE, fillOpacity = 0.4),
                  label = MPAs$ID, labelOptions = labelOptions(textOnly = FALSE, direction = "right", textsize = "12px", sticky = TRUE, style = list("color" = "black"), offset = c(-5, 0)),
                  group = "Marine Protected Areas"
      ) %>%
      addPolygons(data = Counties,
                  layerId = ~ID,
                  color = "black",
                  fillColor = grey(0.5),
                  fillOpacity = 0.5,
                  smoothFactor = 0.5,
                  weight = 0.6,
                  highlightOptions = highlightOptions(color = "white", fillColor = "tomato", weight = 2, bringToFront = TRUE, fillOpacity = 0.4),
                  label = Counties$ID, labelOptions = labelOptions(textOnly = FALSE, direction = "right", textsize = "12px", sticky = TRUE, style = list("color" = "black"), offset = c(-5, 0)),
                  group = "Counties"
      ) %>%
      addPolygons(data = Hexagons,
                  layerId = ~ID,
                  color = "black",
                  fillColor = grey(0.5),
                  fillOpacity = 0.5,
                  smoothFactor = 0.5,
                  weight = 0.6,
                  highlightOptions = highlightOptions(color = "white", fillColor = "tomato", weight = 2, bringToFront = TRUE, fillOpacity = 0.4),
                  label = Hexagons$ID, labelOptions = labelOptions(textOnly = FALSE, direction = "right", textsize = "12px", sticky = TRUE, style = list("color" = "black"), offset = c(-5, 0)),
                  group = "Hexagons"
      ) %>%
      addLayersControl(
        baseGroups = c("Watersheds", "Marine Protected Areas", "Counties", "Hexagons"),
        overlayGroups = "Show Coastal Access <br/> &nbsp; &nbsp; on High Zoom",
        position = "topleft",
        options = layersControlOptions(collapsed = FALSE, title = "Polygons")
      ) %>% 
      hideGroup("Show Coastal Access <br/> &nbsp; &nbsp; on High Zoom") %>%
      htmlwidgets::onRender("
        function(el, x) {
        var myMap = this;
        myMap.on('baselayerchange',
        function(){
            myMap.removeControl(poly_name);
            }
            )}") 
  })

  observeEvent(input$cal_coast_map_zoom, {

    icon_access <- icons(
      iconUrl = "https://www.coastal.ca.gov/publiced/newsletter/summer12/access_logo.jpg",
      iconWidth = 32, iconHeight = 32
    )

    if (input$cal_coast_map_zoom > 10) {
      leafletProxy("cal_coast_map") %>%
        addMarkers(coastal_access_df_overlap$LONGITUDE, coastal_access_df_overlap$LATITUDE,
                   icon = icon_access,
                   popup = paste0("<strong>", coastal_access_df_overlap$NameMobileWeb, "</strong>",
                                  "<br>",
                                  coastal_access_df_overlap$DescriptionMobileWeb,
                                  "<br>",
                                  "<a target = '_blank' href = 'https://www.coastal.ca.gov/YourCoast/#/map/location/id/", coastal_access_df_overlap$ID, "'>", "Click for more information</a>"),
                   popupOptions = popupOptions(maxWidth = 250),
                   # label = coastal_access_df_overlap$NameMobileWeb, labelOptions = labelOptions(textOnly = FALSE, direction = "right", textsize = "13px", sticky = FALSE, offset = c(20, 0)),
                   group = "Show Coastal Access <br/> &nbsp; &nbsp; on High Zoom"
        )
    } else {
        leafletProxy("cal_coast_map") %>%
          clearMarkerClusters() %>%
          clearMarkers()
    }
  })
  
  clicks <- reactiveValues(IDs = vector(mode = "character"))
  base_groups <- reactiveValues(IDs = "Watersheds")
  shape_polys <- reactiveValues(polys = NULL)
  richness_polys <- reactiveValues(polys = NULL)
  uniqueness_polys <- reactiveValues(polys = NULL)
  rarity_polys <- reactiveValues(polys = NULL)
  priority_polys <- reactiveValues(polys = NULL)
  change_polys <- reactiveValues(polys = NULL)
  selected_poly <- reactiveValues(poly = NULL)
  selected_polys <- reactiveValues()

  observeEvent(input$cal_coast_map_groups, {
    remove_modal_spinner() # remove the modal window
    if ("Watersheds" %in% input$cal_coast_map_groups) shape_polys$polys <- Watersheds
    if ("Marine Protected Areas" %in% input$cal_coast_map_groups) shape_polys$polys <- MPAs
    if ("Counties" %in% input$cal_coast_map_groups) shape_polys$polys <- Counties  
    if ("Hexagons" %in% input$cal_coast_map_groups) shape_polys$polys <- Hexagons
  })
  
  observeEvent(input$cal_coast_map_groups, {
    
    if (!identical(intersect(input$cal_coast_map_groups, c("Watersheds", "Marine Protected Areas", "Counties", "Hexagons")), base_groups$IDs[length(base_groups$IDs)])){
    
    base_groups$IDs <- c(base_groups$IDs, intersect(input$cal_coast_map_groups, c("Watersheds", "Marine Protected Areas", "Counties", "Hexagons")))
    
      leafletProxy("cal_coast_map") %>%
        clearGroup("richness_values") %>%
        clearGroup("uniqueness_values") %>%
        clearGroup("rarity_values") %>%
        clearGroup("priority_values") %>%
        clearGroup("change_values") %>%
        removeControl("legend_richness") %>%
        removeControl("legend_uniqueness") %>%
        removeControl("legend_rarity") %>%
        removeControl("legend_priority") %>%
        removeControl("legend_change") %>%
        clearControls() 

      shinyjs::hide(id = "richnessPanel")
      shinyjs::hide(id = "uniquenessPanel")
      shinyjs::hide(id = "rarityPanel")
      shinyjs::hide(id = "priorityPanel")
      shinyjs::hide(id = "changePanel")
      shinyjs::hide(id = "reportPanel")

      updateSwitchInput(session = session, inputId = "map_richness", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_uniqueness", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_rarity", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_priority", value = FALSE)
      
      richness_polys$polys <- NULL
      uniqueness_polys$polys <- NULL
      rarity_polys$polys <- NULL
      priority_polys$polys <- NULL
       
      clicks$IDs <- NULL
    }

  })
  
  observeEvent(input$map_richness, {
    
    if(isTRUE(input$map_richness)){
    
      updateSwitchInput(session = session, inputId = "map_uniqueness", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_rarity", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_priority", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_change", value = FALSE)
      
      observeEvent(input$richness_taxon, {
      
        showNotification(HTML("This may take <br/> a few moments!"), duration = 3, type = "message")
        
        richness_polys$polys <- shape_polys$polys
        richness_polys$polys$ID <- paste0(richness_polys$polys$ID, " richness")
    
        richness_polys$polys@data <- richness_polys$polys@data %>% 
          dplyr::select("ID", contains(input$richness_taxon))
    
        col_pal <- colorNumeric("Reds", richness_polys$polys@data[, paste0("chao1_estimate_", input$richness_taxon)], na.color = grey(.7))
      
        leafletProxy("cal_coast_map") %>%
          addPolygons(data = richness_polys$polys,
                layerId = ~ID,
                group = "richness_values",
                color = "#444444",
                fillColor = ~col_pal(richness_polys$polys@data[, paste0("chao1_estimate_", input$richness_taxon)]),
                smoothFactor = 0.5,
                opacity = 1.0,
                fillOpacity = 0.75,
                weight = 1,
                highlightOptions = highlightOptions(color = "#222222", weight = 2, bringToFront = TRUE, fillOpacity = 0.25),
                label = richness_polys$polys$ID, labelOptions = labelOptions(textOnly = FALSE, direction = "right", textsize = "12px", sticky = TRUE, style = list("color" = "black"), offset = c(-5, 0)),
                ) %>%
          addLegend("topleft",
                layerId = "legend_richness",
                pal = col_pal,
                values = richness_polys$polys@data[, paste0("chao1_estimate_", input$richness_taxon)],
                title = "Species richness",
                labFormat = function(type = "numeric", cuts){
                  c("low", rep("", 3), "high")
                },
                labels = c("lowest", "", "", "", "highest")
      ) 
    
    if (!is.null(selected_poly$poly)) shinyjs::show(id = "richnessPanel")
      })
      }
    })

  observeEvent(input$map_uniqueness, {
    
    if(isTRUE(input$map_uniqueness)){
      
      updateSwitchInput(session = session, inputId = "map_richness", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_rarity", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_priority", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_change", value = FALSE)
      
      observeEvent(input$uniqueness_taxon, {
        
        showNotification(HTML("This may take <br/> a few moments!"), duration = 3, type = "message")
        
      uniqueness_polys$polys <- shape_polys$polys
      uniqueness_polys$polys$ID <- paste0(uniqueness_polys$polys$ID, " uniqueness")
      
      uniqueness_polys$polys@data <- uniqueness_polys$polys@data %>% 
        dplyr::select("ID", contains(input$uniqueness_taxon))
      
      col_pal <- colorNumeric("Reds", uniqueness_polys$polys@data[, paste0("uniqueness_median_", input$uniqueness_taxon)], na.color = grey(.7))
      
      leafletProxy("cal_coast_map") %>%
        addPolygons(data = uniqueness_polys$polys,
                    layerId = ~ID,
                    group = "uniqueness_values",
                    color = "#444444",
                    fillColor = ~col_pal(uniqueness_polys$polys@data[, paste0("uniqueness_median_", input$uniqueness_taxon)]),
                    smoothFactor = 0.5,
                    opacity = 1.0,
                    fillOpacity = 0.75,
                    weight = 1,
                    highlightOptions = highlightOptions(color = "#222222", weight = 2, bringToFront = TRUE, fillOpacity = 0.25)
        ) %>%
        addLegend("topleft",
                  layerId = "legend_uniqueness",
                  pal = col_pal,
                  values = uniqueness_polys$polys@data[, paste0("uniqueness_median_", input$uniqueness_taxon)],
                  title = "Biodiversity uniqueness",
                  labFormat = function(type = "numeric", cuts){
                    c("low", rep("", 3), "high")
                  },
                  labels = c("lowest", "", "", "", "highest")
        )
      
      if (!is.null(selected_poly$poly)) shinyjs::show(id = "uniquenessPanel")
      })
    }
  })
  
  observeEvent(input$map_rarity, {
    
    if(isTRUE(input$map_rarity)){
      
      updateSwitchInput(session = session, inputId = "map_richness", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_uniqueness", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_priority", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_change", value = FALSE)
      
      showNotification(HTML("This may take <br/> a few moments!"), duration = 3, type = "message")
      
      rarity_polys$polys <- shape_polys$polys
      rarity_polys$polys$ID <- paste0(rarity_polys$polys$ID, " rarity")
      
      col_pal <- colorNumeric("Reds", rarity_polys$polys@data$species_rarity, na.color = grey(.7))
      
      leafletProxy("cal_coast_map") %>%
        removeControl("legend") %>%
        addPolygons(data = rarity_polys$polys,
                    layerId = ~ID,
                    group = "rarity_values",
                    color = "#444444",
                    fillColor = ~col_pal(rarity_polys$polys@data$species_rarity),
                    smoothFactor = 0.5,
                    opacity = 1.0,
                    fillOpacity = 0.75,
                    weight = 1,
                    highlightOptions = highlightOptions(color = "#222222", weight = 2, bringToFront = TRUE, fillOpacity = 0.25)
        ) %>%
        addLegend("topleft",
                  layerId = "legend_rarity",
                  pal = col_pal,
                  values = rarity_polys$polys@data$species_rarity,
                  title = "Species rarity",
                  labFormat = function(type = "numeric", cuts){
                    c("low", rep("", 3), "high")
                  },
                  labels = c("lowest", "", "", "", "highest")
        )
      
      if (!is.null(selected_poly$poly)) shinyjs::show(id = "rarityPanel")
      
    }
  })
  
  observeEvent(input$map_priority, {
    
    if(isTRUE(input$map_priority) & length(intersect("Counties", input$cal_coast_map_groups)) == 0){
      
      updateSwitchInput(session = session, inputId = "map_richness", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_uniqueness", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_rarity", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_change", value = FALSE)

      showNotification(HTML("This may take <br/> a few moments!"), duration = 5, type = "message")
      
      priority_polys$polys <- shape_polys$polys
      priority_polys$polys$ID <- paste0(priority_polys$polys$ID, " priority")
      
      col_pal <- colorNumeric("Reds", priority_polys$polys@data$rank_priority, na.color = grey(.7))
      
      leafletProxy("cal_coast_map") %>%
        addPolygons(data = priority_polys$polys,
                    layerId = ~ID,
                    group = "priority_values",
                    color = "#444444",
                    fillColor = ~col_pal(priority_polys$polys@data$rank_priority),
                    smoothFactor = 0.5,
                    opacity = 1.0,
                    fillOpacity = 0.75,
                    weight = 1,
                    highlightOptions = highlightOptions(color = "#222222", weight = 2, bringToFront = TRUE, fillOpacity = 0.25)
        ) %>%
        addLegend("topleft",
                  layerId = "legend_priority",
                  pal = col_pal,
                  values = priority_polys$polys@data$rank_priority,
                  title = "Biodiversity <br> irreplaceability",
                  labFormat = function(type = "numeric", cuts){
                    c("low", rep("", 3), "high")
                  },
                  labels = c("lowest", "", "", "", "highest")
        )
      
      if (!is.null(selected_poly$poly)) shinyjs::show(id = "priorityPanel")

    } 
    
    if(isTRUE(input$map_priority) & length(intersect("Counties", input$cal_coast_map_groups)) > 0){
      
      showNotification("This map is not available", duration = 3, type = "message")
    }
  })
  
  observeEvent(input$map_change, {
    
    if(isTRUE(input$map_change)){
      
      updateSwitchInput(session = session, inputId = "map_richness", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_uniqueness", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_priority", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_rarity", value = FALSE)
      
      showNotification(HTML("This may take <br/> a few moments!"), duration = 3, type = "message")
      
      change_polys$polys <- shape_polys$polys
      change_polys$polys$ID <- paste0(change_polys$polys$ID, " change")
      
      col_pal <- colorNumeric("Reds", change_polys$polys@data$mean_change * -1, na.color = grey(.7))
      
      leafletProxy("cal_coast_map") %>%
        addPolygons(data = change_polys$polys,
                    layerId = ~ID,
                    group = "change_values",
                    color = "#444444",
                    fillColor = ~col_pal(change_polys$polys@data$mean_change * -1),
                    smoothFactor = 0.5,
                    opacity = 1.0,
                    fillOpacity = 0.75,
                    weight = 1,
                    highlightOptions = highlightOptions(color = "#222222", weight = 2, bringToFront = TRUE, fillOpacity = 0.25)
        ) %>%
        addLegend("topleft",
                  layerId = "legend_change",
                  pal = col_pal,
                  values = change_polys$polys@data$mean_change * -1,
                  title = "Community change",
                  labFormat = function(type = "numeric", cuts){
                    c("low", rep("", 3), "high")
                  },
                  labels = c("lowest", "", "", "", "highest")
        )
      
      if (!is.null(selected_poly$poly)) shinyjs::show(id = "changePanel")
      
    }
  })
  
  #### Observe click event and zoom in on clicked MPA
  observeEvent(input$cal_coast_map_shape_click, {
    
    clicks$IDs <- c(clicks$IDs, input$cal_coast_map_shape_click$id)
    
    click <- as.character(clicks$IDs[(length(clicks$IDs))])
    
    if (isTRUE(input$map_richness)){
      selected_polys <- richness_polys$polys
      shinyjs::show(id = "richnessPanel")
      shinyjs::hide(id = "uniquenessPanel")
      shinyjs::hide(id = "priorityPanel")
      shinyjs::hide(id = "rarityPanel")
      shinyjs::hide(id = "changePanel")
      shinyjs::hide(id = "reportPanel")
      updateSwitchInput(session = session, inputId = "map_uniqueness", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_rarity", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_priority", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_change", value = FALSE)
    }
    
    if (isTRUE(input$map_uniqueness)){
      selected_polys <- uniqueness_polys$polys
      shinyjs::show(id = "uniquenessPanel")
      shinyjs::hide(id = "richnessPanel")
      shinyjs::hide(id = "priorityPanel")
      shinyjs::hide(id = "rarityPanel")
      shinyjs::hide(id = "changePanel")
      shinyjs::hide(id = "reportPanel")
      updateSwitchInput(session = session, inputId = "map_richness", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_rarity", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_priority", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_change", value = FALSE)
    }
    
    if (isTRUE(input$map_rarity)){
      selected_polys <- rarity_polys$polys
      shinyjs::show(id = "rarityPanel")
      shinyjs::hide(id = "uniquenessPanel")
      shinyjs::hide(id = "priorityPanel")
      shinyjs::hide(id = "richnessPanel")
      shinyjs::hide(id = "changePanel")
      shinyjs::hide(id = "reportPanel")
      updateSwitchInput(session = session, inputId = "map_richness", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_uniqueness", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_priority", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_change", value = FALSE)
    }
    
    if (isTRUE(input$map_priority)){
      selected_polys <- priority_polys$polys
      shinyjs::show(id = "priorityPanel")
      shinyjs::hide(id = "uniquenessPanel")
      shinyjs::hide(id = "richnessPanel")
      shinyjs::hide(id = "rarityPanel")
      shinyjs::hide(id = "changePanel")
      shinyjs::hide(id = "reportPanel")
      updateSwitchInput(session = session, inputId = "map_richness", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_uniqueness", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_rarity", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_change", value = FALSE)
    }
    
    if (isTRUE(input$map_change)){
      selected_polys <- change_polys$polys
      shinyjs::show(id = "changePanel")
      shinyjs::hide(id = "priorityPanel")
      shinyjs::hide(id = "uniquenessPanel")
      shinyjs::hide(id = "richnessPanel")
      shinyjs::hide(id = "rarityPanel")
      shinyjs::hide(id = "reportPanel")
      updateSwitchInput(session = session, inputId = "map_richness", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_uniqueness", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_rarity", value = FALSE)
      updateSwitchInput(session = session, inputId = "map_priority", value = FALSE)
    }
    
    if (isFALSE(input$map_richness) & isFALSE(input$map_uniqueness) & isFALSE(input$map_rarity) & isFALSE(input$map_priority) & isFALSE(input$map_change)) {
      selected_polys <- shape_polys$polys
    }
    
    selected_poly$poly <- selected_polys[selected_polys$ID == click, ]
    selected_poly$poly@data$selected <- "selected" # paste0(click, "_selected_on_click_", length(clicks$IDs))
    
    poly_bbox <- bbox(selected_poly$poly)
    
    html_selected_poly <- paste0("<strong>", paste(strsplit(as.character(selected_poly$poly$ID), " ")[[1]][-length(strsplit(as.character(selected_poly$poly$ID), " ")[[1]])], collapse = " "), "</strong>")
    
    lon_range <- poly_bbox[3] - poly_bbox[1]
    right_padding <- lon_range * 0.3
    lat_range <- poly_bbox[4] - poly_bbox[2]
    top_padding <- lat_range * 0.3
    
    leafletProxy("cal_coast_map") %>%
      flyToBounds(poly_bbox[1] + right_padding, poly_bbox[2] - top_padding, poly_bbox[3] + right_padding, poly_bbox[4] + top_padding, options = list(animate = TRUE, duration = 1, easeLinearity = 0.1, noMoveStart = TRUE)) %>%
      addControl(html = html_selected_poly, position = "topright", layerId = "poly_name", className = "html_control")
    
  }) 
  
  observeEvent(input$clear_map, {
    
    leafletProxy("cal_coast_map") %>%
      clearGroup("richness_values") %>%
      clearGroup("uniqueness_values") %>%
      clearGroup("rarity_values") %>%
      clearGroup("priority_values") %>%
      clearGroup("change_values") %>%
      removeControl("legend_richness") %>%
      removeControl("legend_uniqueness") %>%
      removeControl("legend_rarity") %>%
      removeControl("legend_priority") %>%
      removeControl("legend_change") %>%
      clearControls() %>% 
      fitBounds(-124.5351, 32.1330, -115.89794, 42.42072)
    
    shinyjs::hide(id = "richnessPanel")
    shinyjs::hide(id = "uniquenessPanel")
    shinyjs::hide(id = "rarityPanel")
    shinyjs::hide(id = "priorityPanel")
    shinyjs::hide(id = "changePanel")
    shinyjs::hide(id = "reportPanel")
    
    updateSwitchInput(session = session, inputId = "map_richness", value = FALSE)
    updateSwitchInput(session = session, inputId = "map_uniqueness", value = FALSE)
    updateSwitchInput(session = session, inputId = "map_rarity", value = FALSE)
    updateSwitchInput(session = session, inputId = "map_priority", value = FALSE)
    updateSwitchInput(session = session, inputId = "map_change", value = FALSE)
    
    richness_polys$polys <- NULL
    uniqueness_polys$polys <- NULL
    rarity_polys$polys <- NULL
    priority_polys$polys <- NULL
    
    clicks$IDs <- NULL
  })
  
  observeEvent(input$map_richness, {
    
    shinyjs::hide(id = "uniquenessPanel")
    shinyjs::hide(id = "priorityPanel")
    shinyjs::hide(id = "rarityPanel")
    shinyjs::hide(id = "changePanel")
    shinyjs::hide(id = "reportPanel")
    
    if(isFALSE(input$map_richness)){
      leafletProxy("cal_coast_map") %>%
        clearGroup("richness_values") %>%
        removeControl("legend_richness")
      shinyjs::hide(id = "richnessPanel")
      richness_polys$polys <- NULL
      clicks$IDs <- NULL
    }
  })
  
  output$richness_donut <- renderHighchart({
    out <- selected_poly$poly@data %>% dplyr::select(paste0("obs_species_proportion_", input$richness_taxon)) %>% round(2) %>% pull()
    plot_single_number_donut(out)
  })
  
  #### Estimated species richness box
  output$richness_estimate <- renderValueBox({
    out <- selected_poly$poly@data %>% dplyr::select(paste0("chao1_estimate_", input$richness_taxon)) %>% round() %>% pull()
    out <- ifelse(!is.na(out), out, "Not available")
    valueBox(out, "Estimated species")
  })

  #### Estimated richness confidence box
  output$richness_confidence <- renderValueBox({
    out <- selected_poly$poly@data %>% dplyr::select(paste0("chao1_confidence_", input$richness_taxon)) %>% pull() %>% as.character()
    out <- ifelse(!is.na(out), out, "Not available")
    out <- paste0(toupper(substr(out, 1, 1)), substr(out, 2, nchar(out)))
    valueBox(as.character(out), "Confidence \n on estimate")
  })
  
  #### Plot of richness inside vs outside MPA
  output$richness_plot <- renderPlotly({
    
    plot_dat <- selected_poly$poly@data
    
    names(plot_dat) <- gsub(paste0("_", input$richness_taxon), "", names(plot_dat))
    
    plot_dat <- plot_dat %>%
      dplyr::select(obs_species, chao1_estimate, chao1_lower, chao1_upper) %>%
      dplyr::mutate(x1 = 1, x2 = 2) 
    
    p <- ggplot() + 
      geom_rect(data=plot_dat, 
                mapping=aes(xmin = x1, xmax = x2, 
                            ymin = chao1_lower, ymax = chao1_upper), color="black", alpha=0.5) +
      ylim(0, max(plot_dat$chao1_upper)) +
      xlim(1, 4) +
      geom_segment(aes(x = 1, xend = 2, y = plot_dat$obs_species[1], yend = plot_dat$obs_species[1]), size = 1.2, col = "tomato") + 
      geom_segment(aes(x = 1, xend = 2, y = plot_dat$chao1_estimate[1], yend = plot_dat$chao1_estimate[1]), size = 1.8) +
      geom_segment(aes(x = 1, xend = 2, y = plot_dat$chao1_lower[1], yend = plot_dat$chao1_lower[1]), size = 0.7) +
      geom_segment(aes(x = 1, xend = 2, y = plot_dat$chao1_upper[1], yend = plot_dat$chao1_upper[1]), size = 0.7) +
      ylab("Number of species") +
      theme_bw() +
      theme(
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = grey(0.95)), # bg of the panel
        plot.background = element_blank(), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.position = "none",
        legend.title = element_blank()
      ) + 
      annotate("text", x = 2.75, y = mean(plot_dat$obs_species), label = "Observed", col = "tomato", size = 4) +
      annotate("text", x = 2.75, y = mean(plot_dat$chao1_estimate), label = "Estimated", col = "black", size = 4) 
    

    gg <- plotly_build(p) %>% config(displayModeBar = F) %>%
      config(displayModeBar = FALSE) %>%
      layout(font = list(family = "Helvetica", size = 14), 
             xaxis = list(titlefont = list(size = 13),
                          tickfont = list(size = 13)),
             yaxis = list(titlefont = list(size = 13),
                          tickfont = list(size = 13))
      )
    
    gg$x$data[[2]]$text = paste0(plot_dat$obs_species, " observed species richness")
    gg$x$data[[3]]$text = paste0(plot_dat$chao1_estimate, " estimated species richness median")
    gg$x$data[[4]]$text = paste0(plot_dat$chao1_lower, " estimated species richness minimum")
    gg$x$data[[5]]$text = paste0(plot_dat$chao1_upper, " estimated species richness maximum")
    gg$x$data[[6]]$hovertext = ""
    gg$x$data[[7]]$hovertext = ""

    gg
    
  })
  
  observeEvent(input$map_uniqueness, {
    
    shinyjs::hide(id = "richnessPanel")
    shinyjs::hide(id = "rarityPanel")
    shinyjs::hide(id = "priorityPanel")
    shinyjs::hide(id = "changePanel")
    shinyjs::hide(id = "reportPanel")
    
    if(isFALSE(input$map_uniqueness)){
      leafletProxy("cal_coast_map") %>%
        clearGroup("uniqueness_values") %>%
        removeControl("legend_uniqueness")
      shinyjs::hide(id = "uniquenessPanel")
      uniqueness_polys$polys <- NULL
      clicks$IDs <- NULL
    }
  })
  
  output$uniqueness_donut <- renderHighchart({
    out <- selected_poly$poly@data %>% dplyr::select(paste0("uniqueness_median_", input$uniqueness_taxon)) %>% round(2) %>% pull()
    plot_single_number_donut(out)
  })
  
  observeEvent(input$map_rarity, {
    
    shinyjs::hide(id = "richnessPanel")
    shinyjs::hide(id = "uniquenessPanel")
    shinyjs::hide(id = "priorityPanel")
    shinyjs::hide(id = "changePanel")
    shinyjs::hide(id = "reportPanel")
    
    if(isFALSE(input$map_rarity)){
      leafletProxy("cal_coast_map") %>%
        clearGroup("rarity_values") %>%
        removeControl("legend_rarity")
      shinyjs::hide(id = "rarityPanel")
      rarity_polys$polys <- NULL
      clicks$IDs <- NULL
    }
  })
  
  output$rarity_donut <- renderHighchart({
    out <- selected_poly$poly@data %>% dplyr::select(species_rarity_proportion) %>% round(2) %>% pull()
    plot_single_number_donut(out)
  })
  
  observeEvent(input$map_priority, {
    
    shinyjs::hide(id = "richnessPanel")
    shinyjs::hide(id = "uniquenessPanel")
    shinyjs::hide(id = "rarityPanel")
    shinyjs::hide(id = "changePanel")
    shinyjs::hide(id = "reportPanel")
    
    if(isFALSE(input$map_priority)){
      leafletProxy("cal_coast_map") %>%
        clearGroup("priority_values") %>%
        removeControl("legend_priority")
      shinyjs::hide(id = "priorityPanel")
      priority_polys$polys <- NULL
      clicks$IDs <- NULL
    }
  })

  output$priority_donut <- renderHighchart({
    out <- selected_poly$poly@data %>% dplyr::select(rank_priority) %>% round(2) %>% pull()
    plot_single_number_donut(out)
  })
  
  output$protected_area_donut <- renderHighchart({
    out <- selected_poly$poly@data %>% dplyr::select(percentage_protected) %>% round(2) %>% pull()
    plot_single_number_donut(out)
  })
  
  observeEvent(input$map_change, {
    
    shinyjs::hide(id = "richnessPanel")
    shinyjs::hide(id = "uniquenessPanel")
    shinyjs::hide(id = "rarityPanel")
    shinyjs::hide(id = "priorityPanel")
    shinyjs::hide(id = "reportPanel")
    
    if(isFALSE(input$map_change)){
      leafletProxy("cal_coast_map") %>%
        clearGroup("change_values") %>%
        removeControl("legend_change")
      shinyjs::hide(id = "changePanel")
      change_polys$polys <- NULL
      clicks$IDs <- NULL
    }
  })
  
  output$change_plot <- renderPlotly({
    p <- plot_change(selected_place = gsub(" change", "", selected_poly$poly$ID),
                   change_data = temporal_change[[intersect(names(temporal_change), input$cal_coast_map_groups)]])
    
    gg <- plotly_build(p) %>% config(displayModeBar = F) %>%
      config(displayModeBar = FALSE) %>%
      layout(font = list(family = "Helvetica", size = 10), 
             xaxis = list(titlefont = list(size = 10),
                          tickfont = list(size = 10)),
             yaxis = list(title = paste0(c(rep("&nbsp;", 50),
                                           "% change",
                                           rep("&nbsp;", 50),
                                           rep("\n&nbsp;", 3)),
                                         collapse = ""),
                          titlefont = list(size = 11),
                          tickfont = list(size = 10),
                          hoverlabel = list(bgcolor = "white"))
      )
    gg$x$layout$yaxis$title$font$size <- 12
    gg$x$data[[1]]$text <- paste0(substr(gg$x$data[[1]]$text, 13, 17), " community change: ", round(as.numeric(substr(gg$x$data[[1]]$text, 29, nchar(gg$x$data[[1]]$text))), 4), "%")
    gg$x$data[[2]]$text <- ""
    gg$x$data[[3]]$text <- ""
    gg$x$data[[4]]$text <- "Background rate of change"
    gg
  })
  
  observeEvent(input$generate_report, {

    # updateSwitchInput(session = session, inputId = "map_priority", value = TRUE)
    
    shinyjs::hide(id = "richnessPanel")
    shinyjs::hide(id = "uniquenessPanel")
    shinyjs::hide(id = "rarityPanel")
    shinyjs::hide(id = "priorityPanel")
    
    if (!is.null(selected_poly$poly)){
      shinyjs::show(id = "reportPanel") 
    } else {
      showNotification(HTML("Select a map <br/> boundary first!"), duration = 2, type = "message")
    }
    
  })
  
  output$richness_donut_forReport <- renderHighchart({
    out <- selected_poly$poly@data %>% dplyr::select(paste0("obs_species_proportion_", input$richness_taxon)) %>% round(2) %>% pull()
    plot_single_number_donut(out)
  })

  output$uniqueness_donut_forReport <- renderHighchart({
    out <- selected_poly$poly@data %>% dplyr::select(paste0("uniqueness_median_", input$uniqueness_taxon)) %>% round(2) %>% pull()
    plot_single_number_donut(out)
  })
  
  output$rarity_donut_forReport <- renderHighchart({
    out <- selected_poly$poly@data %>% dplyr::select(species_rarity_proportion) %>% round(2) %>% pull()
    plot_single_number_donut(out)
  })
  
  output$priority_donut_forReport <- renderHighchart({
    out <- selected_poly$poly@data %>% dplyr::select(rank_priority) %>% round(2) %>% pull()
    plot_single_number_donut(out)
  })
  
  output$protected_area_donut_forReport <- renderHighchart({
    out <- selected_poly$poly@data %>% dplyr::select(percentage_protected) %>% round(2) %>% pull()
    plot_single_number_donut(out)
  })

  output$change_plot_forReport <- renderPlotly({
    p <- plot_change(selected_place = gsub(" change", "", selected_poly$poly$ID),
                     change_data = temporal_change[[intersect(names(temporal_change), input$cal_coast_map_groups)]])
    
    gg <- plotly_build(p) %>% config(displayModeBar = F) %>%
      config(displayModeBar = FALSE) %>%
      layout(font = list(family = "Helvetica", size = 10), 
             xaxis = list(titlefont = list(size = 10),
                          tickfont = list(size = 10)),
             yaxis = list(title = paste0(c(rep("&nbsp;", 50),
                                           "% change",
                                           rep("&nbsp;", 50),
                                           rep("\n&nbsp;", 3)),
                                         collapse = ""),
                          titlefont = list(size = 11),
                          tickfont = list(size = 10),
                          hoverlabel = list(bgcolor = "white"))
      )
    gg$x$layout$yaxis$title$font$size <- 12
    gg$x$data[[1]]$text <- paste0(substr(gg$x$data[[1]]$text, 13, 17), " community change: ", round(as.numeric(substr(gg$x$data[[1]]$text, 29, nchar(gg$x$data[[1]]$text))), 4), "%")
    gg$x$data[[2]]$text <- ""
    gg$x$data[[3]]$text <- ""
    gg$x$data[[4]]$text <- "Background rate of change"
    gg
  })  

  # output$download_report = downloadHandler(
  #   filename = function() {"report.png"},
  #   content = function(file) {
  #     png(file)
  #     plot_single_number_donut(selected_poly$poly@data %>% dplyr::select(rank_priority) %>% round(2) %>% pull()) 
  #     dev.off()
  #   }
  # )
  
}

#### Features
### Include a measure of conservation deficit (high priority, low protected area)
### Include Report download
### Extract species list
### Enable uploading/drawing polygon
### Add most common species feature 
