###################################################################################
###################################################################################
##### -- Place-based biodiversity indicators from iNaturalist Observations -- #####
###################################################################################
###################################################################################
################################ SHINY FUNCTIONS ##################################
plot_single_number_donut <- function(value_to_plot, percentage = TRUE, suffix = "%"){
  
  if (!is.na(value_to_plot)){
    
    plot_values <- c(value_to_plot, 1 - value_to_plot)
    
    if (isTRUE(percentage)) plot_values <- plot_values * 100
    
    donut_plot <- highchart() %>%
      hc_title(text = paste0(plot_values[1], suffix),
               verticalAlign = "middle",
               floating = TRUE,
               y = 0,
               x = 0,
               style = list(color = "#33A1DE", fontSize = "25px", fontFamily = "Helvetica", fontWeight = "bold", useHTML = TRUE)) %>%
      hc_chart(type = "pie", height = 250) %>%
      hc_add_series(plot_values, innerSize = "70%", center = c('50%', '50%'), dataLabels = list(enabled = FALSE, connectorWidth = 0)) %>%
      hc_colors(c("#33A1DE", grey(.9))) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_size(height = 250, width = 200)
    } else {
      donut_plot <- highchart() %>%
        hc_title(text = "NA",
               verticalAlign = "middle",
               floating = TRUE,
               y = 0,
               style = list(color = grey(.9), fontSize = "25px", fontFamily = "Helvetica", fontWeight = "bold", useHTML = TRUE)) %>%
      hc_chart(type = "pie") %>%
      hc_add_series(c(1, 0), innerSize = "70%", center = c('50%', '50%'), dataLabels = list(enabled = FALSE, connectorWidth = 0)) %>%
      hc_colors(c(grey(.9), grey(.9))) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_size(height = 250) 
      }

  return(donut_plot)
  
}

plot_change <- function(selected_place, change_data){
  
  plot_dat <- change_data$place_stability %>%
    dplyr::filter(place == selected_place, time_period > 2010 & time_period < 2020) %>% 
    dplyr::select(time_period, upper, mean, lower)
  
  plot_dat[c("upper", "mean", "lower")] <- round(-1 * (100 * (plot_dat[c("upper", "mean", "lower")] - change_data$global_stability["mean"])/change_data$global_stability["mean"]), 6)
  change_data$global_stability <- round(-1 * (100 * (change_data$global_stability - change_data$global_stability["mean"])/change_data$global_stability["mean"]), 6)
  
  plot_dat <- plot_dat %>% 
    add_row(time_period = min(plot_dat$time_period) - 1, 
            upper = change_data$global_stability["mean"], mean = change_data$global_stability["mean"], lower = change_data$global_stability["mean"], .before = 1) 

  p <- plot_dat %>%
    ggplot(aes(x = time_period, y = mean)) +
    geom_line(color = "deepskyblue3", size = 0.7) +
    geom_point(color = "deepskyblue3", size = 1.3) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "deepskyblue3", alpha = 0.2) +
    geom_hline(yintercept = change_data$global_stability["mean"], linetype = "dashed") +
    geom_ribbon(aes(ymin = change_data$global_stability["lower"], ymax = change_data$global_stability["upper"]), fill = "grey70", alpha = 0.5) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "transparent"), 
          plot.background = element_rect(fill = "transparent", color = NA)) +
    scale_x_continuous(breaks = plot_dat$time_period) +
    ylab("% change") +
    xlab("")

  if (nrow(plot_dat) == 1){
    p <- p + 
      annotate("text", y = 0, x = 0.5, label = "NA", color = grey(.7), size = 15) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  }
  
  p
}
