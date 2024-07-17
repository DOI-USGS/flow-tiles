#' @description Cowplot national data image with explainer annotations
#' @param plot_nat National data plot
#' @param date_start first day of focal month
#' @param flow_label legend explainer text
#' @param source_label data source text
#' @param legend restyled legend
#' @param explainer_label top annotation for how to read the plot
#' @param file_png file path for final png
#' @param width width of final png
#' @param height height of final png
#' @param font_legend font used for legend text
#' @param text_color color used for viz text
cowplot_national_explainer <- function(explainer_label, file_png, 
                                       width, height, font_legend, text_color, blue_label, orange_label){

  plot_margin <- 0.025
  # background
  canvas <- grid::rectGrob(
    x = 0, y = 0, 
    width = 16, height = 9,
    gp = grid::gpar(fill = color_bknd, alpha = 1, col = color_bknd)
  )
  
  #arrows
  (normal_range_arrow <- ggplot() + 
      theme_void()+
      # add arrow using `geom_curve()`
      geom_curve(aes(x = 13, y = 3,
                     xend = 11, yend = 3),
                 arrow = grid::arrow(length = unit(0.2, 'lines')), 
                 curvature = 0, angle = 100, ncp = 10,
                 color = text_color, linewidth = 0.2))
  
  (low_range_arrow <- ggplot() + 
      theme_void()+
      # add arrow using `geom_curve()`
      geom_curve(aes(x = 13, y = 3,
                     xend = 11, yend = 3),
                 arrow = grid::arrow(length = unit(0.2, 'lines')), 
                 curvature = 0, angle = 100, ncp = 10,
                 color = "#A84E0B", linewidth = 0.2))
  
  (high_range_arrow <- ggplot() + 
      theme_void()+
      # add arrow using `geom_curve()`
      geom_curve(aes(x = 13, y = 3,
                     xend = 11, yend = 3),
                 arrow = grid::arrow(length = unit(0.2, 'lines')), 
                 curvature = 0, angle = 100, ncp = 10,
                 color = "#002D5E", linewidth = 0.2))
  
  national_plot_png <- magick::image_read("out/flow_national_ig.png")
  # compose final plot
  ggdraw(ylim = c(0,1), 
         xlim = c(0,1)) +
    # a white background
    draw_grob(canvas,
              x = 0, y = 1,
              height = 1, width = 1,
              hjust = 0, vjust = 1) +
    draw_image(national_plot_png, x = 0, y = 0, width = 1, hjust = 0, vjust = 0, halign = 0, valign = 0)+
    draw_label(explainer_label, 
               x = 0.5, y = 0.96, 
               size = 5.5, 
               hjust = 0, vjust = 1,
               fontfamily = font_legend,
               color = "#000000")+ #text_color) +
    draw_label(blue_label, 
               x = 0.5, y = 0.91, 
               size = 5.5, 
               hjust = 0, vjust = 1,
               fontfamily = font_legend,
               color = "#002D5E")+
    draw_label(orange_label, 
               x = 0.5, y = 0.88, 
               size = 5.5, 
               hjust = 0, vjust = 1,
               fontfamily = font_legend,
               color = "#A84E0B")+
    draw_label("Low\nStreamflow",
               x = 0.84, y = 0.355, 
               size = 5.5, 
               hjust = 0.5, vjust = 1,
               fontfamily = font_legend,
               color = "#A84E0B") +
    draw_label("High\nStreamflow",
               x = 0.84, y = 0.76, 
               size = 5.5, 
               hjust = 0.5, vjust = 1,
               fontfamily = font_legend,
               color = "#002D5E") +
    draw_label("Typical\nStreamflow",
               x = 0.855, y = 0.5, 
               size = 5.5, 
               hjust = 0.5, vjust = 1,
               fontfamily = font_legend,
               color = text_color) +
    draw_plot(high_range_arrow, # for high streamflow
              x = 0.755, 
              y = 0.755,
              height = 0.035, 
              width = 0.05,
              hjust = 0,
              vjust = 0.5)+
    draw_plot(normal_range_arrow, # for typical streamflow
              x = 0.755, 
              y = 0.495,
              height = 0.035, 
              width = 0.05,
              hjust = 0,
              vjust = 0.5)+
    draw_plot(low_range_arrow, # for low streamflow
              x = 0.755, 
              y = 0.35,
              height = 0.035, 
              width = 0.055,
              hjust = 0,
              vjust = 0.5)
  
  # Save and convert file
  ggsave(file_png, width = width, height = height, dpi = 300, units = c("px"))
  
}

#' @description Plot national data for intro question background
#' @param national_data The proportion of sites in each flow condition, daily
#' @param percentile_bin bins used to define the percentiles
#' @param pal color palette for each bin level
intro_background <- function(national_data, percentile_bin, pal){ 
  
  (plot_nat_clean <- national_data %>% 
     ggplot(aes(date, prop)) +
     geom_area(aes(fill = percentile_bin), alpha = 0.5) +
     scale_fill_manual(values = rev(pal)) +
     scale_y_continuous(trans = "reverse",
                        breaks = rev(c(0, 0.25, 0.5, 0.75, 1)), 
                        labels = c("0%", "25%", "50%", "75%", "100%")) +
     coord_fixed(ratio = 28, clip = "off")+
     theme_void()+
     theme(legend.position = "none"))
  
}

#' @description Cowplot intro question image
#' @param plot_nat_clean Plot of national data used for background
#' @param date_start first day of focal month
#' @param font_legend font used for legend text
#' @param width width of final png
#' @param height height of final png
#' @param file_png file path for final png
intro_image <- function(plot_nat_clean, date_start, font_legend, width, height, file_png){
  
  plot_month <- lubridate::month(date_start, label = TRUE, abbr = FALSE)
  plot_year <- lubridate::year(date_start)
  
  plot_margin <- 0.025
  
  title_flow_dark <- magick::image_read('in/streamflow.png') |> magick::image_scale('800x') %>%
    magick::image_colorize(100, "#222222") |> magick::image_scale('800x')
  
  # background
  canvas <- grid::rectGrob(
    x = 0, y = 0, 
    width = 16, height = 9,
    gp = grid::gpar(fill = color_bknd, alpha = 1, col = color_bknd)
  )
  
  ggdraw(ylim = c(0,1), 
         xlim = c(0,1)) +
    # a white background
    draw_grob(canvas,
              x = 0, y = 1,
              height = 0.37, width = 0.37,
              hjust = 0, vjust = 1) +
    # national-level plot
    draw_plot(plot_nat_clean,
              x = -0.055,
              y = -0.09,
              height = 1.18,
              width = 1.18)+
    # draw title
    draw_label(sprintf('%s %s', plot_month, plot_year),
               x = plot_margin*2, y = 1-plot_margin*1.2,
               size = 16,
               hjust = 0,
               vjust = 1,
               fontfamily = font_legend,
               color = "#222222",
               lineheight = 1)  +
    # stylized streamflow title
    draw_image(title_flow_dark ,
               x = plot_margin*2,
               y = 1-(1.5*plot_margin),
               height = 0.16,
               width = 0.74,
               hjust = 0,
               vjust = 1) +
    draw_label(sprintf("How did %s's\nstreamflow\ncompare to the\npast?", plot_month),
               x = 0.05,
               y = 0.5,
               size = 26,
               hjust = 0,
               vjust = 0.5,
               fontfamily = font_legend,
               color = "#222222")
  
  ggsave(file_png, width = width, height = height, dpi = 300, units = c("px"))
  
}