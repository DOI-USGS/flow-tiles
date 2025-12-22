#' @description Cowplot national data image with source label + logo
#' and check if content fall within 2025 IG safezone for home landing img.
#' @param file_png_list file path for final png based off check_ig_safezone parameter 
#' @param ig_grid_lines file path for instagram square layout for grid in 2025
#' @param width width of final png
#' @param height height of final png
#' @param font_legend font used for legend text
#' @param text_color color used for viz text
#' @param check_ig_safezone check safe zone guide to check instagram layout
#' @param source_label Source label placed in bottom right of plot
cowplot_national_explainer <- function(file_png_list, national_plot_png,
                                       ig_grid_lines, width, height, font_legend,
                                       text_color, check_ig_safezone,
                                       source_label){
  # Selecting output file based off check_ig_safezone
  file_png <- if (check_ig_safezone) {
    file_png_list[["check_ig_safezone"]]
  } else {
    file_png_list[["dont_check_ig_safezone"]]
  }
  
  # usgs logo
  usgs_logo <- magick::image_read('in/usgs_logo.png') |> 
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
               x = -0.02, y = -0.01, 
               width = 1, height = 1.2,
               hjust = 0, vjust = 0, 
               halign = 0, valign = 0) + 
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