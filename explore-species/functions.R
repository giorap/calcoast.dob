###################################################################################
###################################################################################
##### -- Place-based biodiversity indicators from iNaturalist Observations -- #####
###################################################################################
###################################################################################
################################ SHINY FUNCTIONS ##################################
#' plot_trends_data()
plot_trends_data <- function(species_name = "Pisaster_ochraceus", 
                             region = "Statewide", 
                             dat = species_trends, 
                             anomaly = FALSE, 
                             predictor_data = NULL, 
                             predictor_label = "Temperature minimum"){
  
  plot_dat <- dat[[species_name]][[region]] 

  if (isTRUE(anomaly)){
    plot_dat <- plot_dat %>% dplyr::mutate(reporting_rate = (reporting_rate - mean(reporting_rate, na.rm = TRUE))/mean(reporting_rate, na.rm = TRUE))
  }
  
  line_col <- ifelse(region == "Statewide", "black", 
              ifelse(region == "North", "#1b9e77",
              ifelse(region == "Central", "#d95f02",
              ifelse(region == "South", "#7570b3", NA
                     )
              )
              )
  )
  
  if (is.null(predictor_data)){
    p <- plot_dat %>% ggplot(aes(x = year %>% as.integer())) + 
      geom_line(aes(y = reporting_rate, colour = "Observation rate"), size = 0.7) +
      geom_point(aes(y = reporting_rate, colour = "Observation rate"), size = 1.3) +
      scale_color_manual(values = line_col) +
      scale_y_continuous(labels = scales::percent_format(accuracy = .01)) 
       
  } else {
    plot_dat <- data.frame(plot_dat, predictor_data[-1])
    names(plot_dat)[ncol(plot_dat)] <- "predictor"
    plot_dat <- plot_dat %>% 
      dplyr::mutate(predictor = scales::rescale(predictor, to = c(min(reporting_rate, na.rm = TRUE), max(reporting_rate, na.rm = TRUE))))
    p <- plot_dat %>% ggplot(aes(x = year %>% as.integer())) + 
      geom_line(aes(y = reporting_rate, colour = "Observation rate"), size = 0.7) +
      geom_point(aes(y = reporting_rate, colour = "Observation rate"), size = 1.3) +
      geom_line(aes(y = predictor, colour = predictor_label), size = 0.7) +
      geom_point(aes(y = predictor, colour = predictor_label), size = 1.3) +
      scale_y_continuous(sec.axis = sec_axis(~., name = predictor_label), 
                         labels = scales::percent_format(accuracy = .01)) +
      scale_color_manual(values = c(line_col, grey(.75))) 
  }

  p <- p + 
    labs(y = "Proportional rate of \n observation on iNaturalist", x = "") +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"), 
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.text = element_text(size = 13),
          legend.title = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(angle = 0),
          ) 
  
  if (region == "Statewide") p <- p + annotate("text", label = "Statewide", x = 2018.5, y = max(plot_dat$reporting_rate), hjust = -1)
  if (region == "Statewide" & !is.null(predictor_data)) p <- p + theme(legend.position = "bottom")
  else p <- p + theme(legend.position = "none")
  
  gg <- plotly_build(p) %>% config(displayModeBar = F) %>%
    config(displayModeBar = FALSE) %>%
    layout(font = list(family = "Helvetica", size = 11), 
           title = list(family = "Helvetica", size = 6),
           xaxis = list(titlefont = list(size = 11),
                        tickfont = list(size = 11)),
           yaxis = list(titlefont = list(size = 11),
                        tickfont = list(size = 11)),
           hoverlabel=list(bgcolor="white"),
           legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.1)
           )
  gg$x$layout$margin <- list(t = 0, r = 0, b = 0, l = 0)
  gg$x$layout$title$font$size <- 11
  gg$x$layout$legend$font$size <- 11
  gg$x$data[[1]]$text <- paste0(2012:2019, " Observation rate: ", round(plot_dat$reporting_rate*100, 2), "%")
  if (!is.null(predictor_data)) gg$x$data[[2]]$text <- paste0(2012:2019, " ", predictor_label, ": ", round(as.numeric(predictor_data[-1]), 2))
  gg
}

#' map_predictions()
map_predictions <- function(prediction_raster){

  full_pal <- colorNumeric(colorRamp(c(rgb(1, 1, 1, 0.1), "red"), interpolate = "spline"), raster::values(prediction_raster),
                           na.color = "transparent")
  prediction_raster_top <- prediction_raster
  prediction_raster_top[raster::values(prediction_raster) < quantile(raster::values(prediction_raster), .9, na.rm = TRUE)] <- NA
  top_pal <- colorNumeric(colorRamp(c(full_pal(quantile(raster::values(prediction_raster), .9, na.rm = TRUE)), 
                                      full_pal(max(raster::values(prediction_raster), na.rm = TRUE))), 
                                    interpolate = "spline"), 
                          raster::values(prediction_raster_top),
                          na.color = "transparent")
  
  leaflet(options = leafletOptions(zoomDelta = 0.5, zoomSnap = 0, attributionControl = FALSE)) %>% 
    fitBounds(-124.5351, 32.1330, -115.89794, 42.42072) %>%
    addProviderTiles(providers$Esri.WorldShadedRelief) %>%
    addRasterImage(prediction_raster,
                   colors = full_pal,
                   opacity = 0.5
    ) %>%
    addRasterImage(prediction_raster_top,
                   colors = top_pal,
                   opacity = 1
    ) %>%
    addLegend(pal = full_pal, values = raster::values(prediction_raster),
              title = "Density <br> (1/10km2)",
              bins = 4)  
}

predict.multispeciesPP <- function (object, newdata, sdm = TRUE, bias = FALSE, species = colnames(object$fit.PA), 
          dispersion = NULL, terms = NULL, na.action = na.pass, ...) 
{
  na.act <- object$na.action
  object$na.action <- NULL
  pred <- matrix(0, nrow(newdata), length(species), dimnames = list(rownames(newdata), 
                                                                    species))
  if (sdm) {
    sdm.mat <- model.matrix(object$sdm.formula, newdata)
    good.rows <- row.names(sdm.mat)
    pred[!(row.names(pred) %in% good.rows), ] <- NA
    pred[good.rows, ] <- sdm.mat %*% object$species.coef[-nrow(object$species.coef), 
                                                         species]
  }
  if (bias) {
    bias.mat <- model.matrix(object$bias.formula, newdata)
    good.rows <- rownames(bias.mat)
    pred[!(row.names(pred) %in% good.rows), ] <- NA
    pred[good.rows, ] <- pred[good.rows, ] + bias.mat[good.rows, 
                                                      ] %*% object$bias.coef
    pred <- pred + rep(object$species.coef["isPO", 
                                           species], each = nrow(pred))
  }
  pred
}

#' plot_response_curve()
plot_response_curve <- function(sdm, focal_species_name, predictor = "temp_min", xlabel = "Mean temperature"){
  
  bg <- sdm$data$background
  new_var <- seq(round(min(bg[, predictor]), 3), round(max(bg[, predictor]), 3), (round(max(bg[, predictor]), 3)-round(min(bg[, predictor]), 3))/100)
  bg_means <- sdm$data$background %>% dplyr::select(-x, -y, -habitat, -predictor) %>% summarise_all(mean)
  main_habitat <- data.frame(habitat = sdm$data$background$habitat, species_density = sdm$sdm$fit.BG[, focal_species_name]) %>% 
    dplyr::group_by(habitat) %>% 
    dplyr::summarise(species_density = median(species_density)) %>%
    arrange(desc(species_density)) %>% 
    pull(habitat) %>% 
    head(1)
  new_bg <- data.frame(new_var, bg_means[rep(1, length(new_var)), setdiff(names(bg_means), "year")], habitat = main_habitat, year = 2019) 
  names(new_bg)[1] <- predictor
  sdm_predictions_new <- predict.multispeciesPP(sdm$sdm, newdata = new_bg, type = "response") %>% exp()
  sdm_predictions_new <- data.frame(cbind(new_bg %>% dplyr::select(predictor), sdm_predictions_new[, focal_species_name]))
  names(sdm_predictions_new)[2] <- "species_density"
  median_response <- sdm_predictions_new %>% group_by_at(vars(one_of(predictor))) %>% dplyr::summarise(species_density = median(species_density)) %>% as.data.frame()
  p <- ggplot(median_response, aes_string(x = predictor, y = "species_density")) +
    geom_line(size = 1.5, color = "firebrick3") + 
    geom_ribbon(aes(ymax = species_density), ymin = 0, fill = "firebrick3", alpha=0.3) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"), 
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.text = element_text(size = 13),
          legend.title = element_blank(),
          legend.position = "none"
    ) +
    xlab(xlabel) +
    ylab("Population density (1/10km2)") 
  
  print(p)
  return(p)
  
}

plot_density_by_predictor <- function(sdm, focal_species_name, predictor = "temp_min", xlabel = "Mean temperature"){

  df <- data.frame(predictor = sdm$data$background[, predictor], 
                   species_density = sdm$sdm$fit.BG[, focal_species_name]) 
  response <- lm(species_density ~ splines::bs(predictor, 6), data = df)
  df <- df %>% 
    dplyr::mutate(fitted_species_density = fitted(response)) %>% 
    dplyr::mutate(fitted_species_density = ifelse(fitted_species_density >= 0, fitted_species_density, 0))
  p <- ggplot(df, aes(x = predictor, y = fitted_species_density)) +
    geom_line(size = 1.5, color = "firebrick3") + 
    geom_ribbon(aes(ymax = fitted_species_density), ymin = 0, fill = "firebrick3", alpha=0.3) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"), 
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.text = element_text(size = 13),
          legend.title = element_blank(),
          legend.position = "none"
    ) +
    ylim(0, max(df$fitted_species_density)) +
    xlab(xlabel) +
    ylab("Species density (1/10km2)") 
  
  print(p)
  return(p)
}

