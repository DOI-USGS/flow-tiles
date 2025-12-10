#' @description Cowplot national data image with explainer annotations
#' @param file_png file path for final png
#' @param ig_grid_lines file path for instagram square layout for grid in 2025
#' @param width width of final png
#' @param height height of final png
#' @param font_legend font used for legend text
#' @param text_color color used for viz text
#' @param check_ig_safezone check safe zone guide to check instagram layout
#' @param date_start First day of focal month
#' @param flow_label Flow percentile label placed above legend
#' @param source_label Source label placed in bottom right of plot
#' @param restyle_legend re-stylizing legend national flow timeseries plot
cowplot_national_explainer <- function(file_png, national_plot_png,
                                       ig_grid_lines,
                                       width, height, font_legend, text_color, 
                                       low_col, high_col, low_lab, high_lab, typ_lab, 
                                       typ_lab_ypos, typ_arr_ypos, check_ig_safezone,
                                       date_start, flow_label, source_label, 
                                       restyle_legend){
  
  plot_month <- lubridate::month(date_start, label = TRUE, abbr = FALSE)
  plot_year <- lubridate::year(date_start)
  
  # usgs logo
  usgs_logo <- magick::image_read('in/usgs_logo.png') %>%
    magick::image_colorize(100, text_color) |> magick::image_scale('250x')
  
  # streamflow title
  title_flow <- magick::image_read('in/streamflow.png') |> magick::image_scale('800x')

    plot_margin <- 0.025
  # background
  canvas <- grid::rectGrob(
    x = 0, y = 0, 
    width = 16, height = 9,
    gp = grid::gpar(fill = color_bknd, alpha = 1, col = color_bknd)
  )
  
  # arrows
  (dry_arrow <- ggplot() +
      theme_void() +
      # add arrow using `geom_curve()`
      geom_curve(aes(x = -13, y = 3,
                     xend = -11, yend = 2),
                 arrow = grid::arrow(length = unit(1.25, 'lines')),
                 curvature = 0.3, angle = 80, ncp = 10,
                 color = low_col, linewidth = 1.25))

  (wet_arrow <- ggplot() +
      theme_void() +
      # add arrow using `geom_curve()`
      geom_curve(aes(x = 13, y = 3,
                     xend = 11, yend = 2),
                 arrow = grid::arrow(length = unit(1.25, 'lines')),
                 curvature = -0.3, angle = 80, ncp = 10,
                 color = high_col, linewidth = 1.25))

  (normal_range_arrow <- ggplot() +
      theme_void()+
      # add arrow using `geom_curve()`
      geom_curve(aes(x = 13, y = 3,
                     xend = 11, yend = 3),
                 arrow = grid::arrow(length = unit(1.25, 'lines')),
                 curvature = 0, angle = 100, ncp = 10,
                 color = text_color, linewidth = 1.25))

  (low_range_arrow <- ggplot() +
      theme_void()+
      # add arrow using `geom_curve()`
      geom_curve(aes(x = 13, y = 3,
                     xend = 11, yend = 3),
                 arrow = grid::arrow(length = unit(1.25, 'lines')),
                 curvature = 0, angle = 100, ncp = 10,
                 color = low_col, linewidth = 1.25))

  (high_range_arrow <- ggplot() +
      theme_void()+
      # add arrow using `geom_curve()`
      geom_curve(aes(x = 13, y = 3,
                     xend = 11, yend = 3),
                 arrow = grid::arrow(length = unit(1.25, 'lines')),
                 curvature = 0, angle = 100, ncp = 10,
                 color = high_col, linewidth = 1.25))
  
  og_plot_png <- magick::image_read(national_plot_png)
  ig_grid_png <- magick::image_read(ig_grid_lines)
  
  if (check_ig_safezone) {
  # convert IG grid to rasterGrob for overlay
  ig_grid_raster <- grid::rasterGrob(
    as.raster(ig_grid_png),
    width = unit(1, "npc"),
    height = unit(1, "npc"),
    interpolate = TRUE, 
    gp = grid::gpar(alpha = 0.4)
  )
  }
  
  # compose final plot
  g <- ggdraw(ylim = c(0,1), 
         xlim = c(0,1)) +
    # a white background
    draw_grob(canvas,
              x = 0, y = 1,
              height = 1, width = 1,
              hjust = 0, vjust = 1) +
    draw_image(og_plot_png, 
               x = 0.09, y = 0.13, 
               width = 0.75, 
               hjust = 0, vjust = 0, 
               halign = 0, valign = 0) +
    draw_label(
      high_lab,
      x = 0.757,
      y = 0.672,
      size = 22,
      hjust = 0.5,
      vjust = 0,
      fontfamily = font_legend,
      color = high_col
    ) +
    draw_plot(
      high_range_arrow,
      x = 0.72,
      y = 0.699,
      height = 0.06,
      width = 0.06,
      hjust = 1,
      vjust = 0.5
    ) +
    draw_label(
      typ_lab,
      x = 0.771,
      y = typ_lab_ypos,
      size = 22,
      hjust = 0.5,
      vjust = 0,
      fontfamily = font_legend,
      color = text_color
    ) +
    draw_plot(
      normal_range_arrow,
      x = 0.72,
      y = typ_arr_ypos,
      height = 0.06,
      width = 0.06,
      hjust = 1,
      vjust = 0.5
    ) +
    draw_label(
      low_lab,
      x = 0.754,
      y = 0.37,
      size = 22,
      hjust = 0.5,
      vjust = 0,
      fontfamily = font_legend,
      color = low_col
    ) +
    draw_plot(
      low_range_arrow,
      x = 0.72,
      y = 0.395,
      height = 0.06,
      width = 0.06,
      hjust = 1,
      vjust = 0.5
    ) + 
  # add legend
  draw_plot(restyle_legend,
            x = (1-plot_margin)*0.5,
            y = 0.133,
            height = 0.12 ,
            width = 0.02-plot_margin) +
    # draw title
    draw_label(sprintf('%s %s', plot_month, plot_year),
               x = plot_margin*7.5, y = 1-plot_margin*2.5,
               size = 72,
               hjust = 0,
               vjust = 1,
               fontfamily = font_legend,
               color = text_color,
               lineheight = 1)  +
    # stylized streamflow title
    draw_image(title_flow,
               x = plot_margin*7.6,
               y = 1-(2.4*plot_margin),
               height = 0.2,
               width = 0.76,
               hjust = 0,
               vjust = 1) +
    # percentile info
    draw_label(flow_label,
               x = (1-plot_margin)*0.246,
               y = 0.27,
               hjust = 0,
               vjust = 1,
               fontfamily = font_legend,
               color = text_color,
               size = 24) +
    # add data source
    draw_label(source_label, 
               x = plot_margin + 0.8, y = plot_margin*2, 
               fontface = "italic", 
               size = 20, 
               hjust = 1, vjust = 0,
               fontfamily = font_legend,
               color = text_color,
               lineheight = 1.1) +
    # add logo
    draw_image(usgs_logo, x = plot_margin + 0.14, y = plot_margin*1.6,
               width = 0.135, hjust = 0, vjust = 0, halign = 0, valign = 0)
  
  # overlay IG grid 
  if (check_ig_safezone) {
    g <- g + draw_grob(ig_grid_raster)
  }
  
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
#' @param text_size size of text
intro_image <- function(plot_nat_clean, date_start, font_legend, width, height, file_png, text_size){
  
  plot_month <- lubridate::month(date_start, label = TRUE, abbr = FALSE)
  plot_margin <- 0.025
  
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
              x = -0.075, y = -0.09,
              height = 1.18, width = 1.18) +
    draw_label(sprintf("A look at\n%s's\nstreamflow\nacross the U.S.", plot_month),
               x = 0.15, y = 0.5,
               size = text_size,
               hjust = 0, vjust = 0.5,
               fontfamily = font_legend,
               color = "#222222")
  
  ggsave(file_png, width = width, height = height, dpi = 300, units = c("px"))
  
}