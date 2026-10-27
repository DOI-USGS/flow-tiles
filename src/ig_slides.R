#' @description Format focal month as "[Month] [Year]" title
#' @param date_start first day of focal month
ig_month_year <- function(date_start){
  sprintf('%s %s',
          lubridate::month(date_start, label = TRUE, abbr = FALSE),
          lubridate::year(date_start))
}

#' @description Stylized STREAMFLOW title as a grob that keeps its aspect ratio
streamflow_title_grob <- function(){
  # trim transparent padding so text centers in its box
  title_flow <- magick::image_read('in/streamflow.png') |>
    magick::image_trim() |>
    magick::image_repage()
  grid::rasterGrob(as.raster(title_flow), width = unit(1, "npc"), interpolate = TRUE)
}

#' @description Save a cowplot composition on the IG background
#' @param plot cowplot composition
#' @param file_png file path for final png
#' @param width width of final png in pixels
#' @param height height of final png in pixels
#' @param color_bknd background color
save_ig_slide <- function(plot, file_png, width, height, color_bknd){
  ggsave(file_png,
         plot + theme(plot.background = element_rect(fill = color_bknd, color = NA)),
         width = width, height = height, dpi = 300, units = "px", bg = color_bknd)
  return(file_png)
}

#' @description Plot national flow as a full-bleed area for title slide background
#' @param national_data The proportion of sites in each flow condition, daily
#' @param pal color palette for each bin level
#' @param alpha transparency of area fill
plot_national_bkgd_ig <- function(national_data, pal, alpha){
  national_data %>%
    ggplot(aes(date, prop)) +
    geom_area(aes(fill = percentile_bin), alpha = alpha) +
    scale_fill_manual(values = rev(pal)) +
    scale_x_date(expand = c(0, 0)) +
    scale_y_continuous(trans = "reverse", expand = c(0, 0)) +
    theme_void() +
    theme(legend.position = "none",
          plot.margin = margin(0, 0, 0, 0))
}

#' @description Plot national flow with % and wet/dry labels for IG
#' @param national_data The proportion of sites in each flow condition, daily
#' @param pal color palette for each bin level
#' @param text_color Color of text in plot
#' @param font_legend font styling
#' @param text_size size of axis and title text
#' @param low_lab label for dry end of y axis
#' @param high_lab label for wet end of y axis
plot_national_ig <- function(national_data, pal, text_color, font_legend, text_size,
                             low_lab, high_lab){
  national_data %>%
    ggplot(aes(date, prop)) +
    geom_area(aes(fill = percentile_bin)) +
    scale_fill_manual(values = rev(pal)) +
    scale_x_date(expand = c(0, 0)) +
    # reversed so 1 (0% of gages) sits at bottom
    scale_y_continuous(trans = "reverse", expand = c(0, 0),
                       breaks = c(1, 0.5),
                       labels = c("0%", "50%"),
                       sec.axis = dup_axis(breaks = c(0.96, 0.04),
                                           labels = c(low_lab, high_lab))) +
    labs(title = "100% of streamgages") +
    theme_void() +
    theme(legend.position = "none",
          text = element_text(family = font_legend, color = text_color, size = text_size),
          plot.title = element_text(hjust = 0, size = text_size * 1.1, margin = margin(b = 6)),
          plot.title.position = "plot",
          axis.text.y.left = element_text(hjust = 1, margin = margin(r = 3)),
          axis.text.y.right = element_text(hjust = 0, margin = margin(l = 3)),
          plot.margin = margin(0, 0, 0, 0))
}

#' @description IG slide 1: national flow background with month title
#' @param file_png file path for final png
#' @param plot_bkgd full-bleed national plot
#' @param date_start first day of focal month
#' @param width width of final png in pixels
#' @param height height of final png in pixels
#' @param color_bknd background color
#' @param text_color Color of text in plot
#' @param font_legend font styling
ig_title_slide <- function(file_png, plot_bkgd, date_start, width, height,
                           color_bknd, text_color, font_legend){
  p <- ggdraw() +
    draw_plot(plot_bkgd, x = 0, y = 0, width = 1, height = 1) +
    draw_label(ig_month_year(date_start),
               x = 0.5, y = 0.445,
               size = 26,
               fontfamily = font_legend,
               color = text_color) +
    draw_grob(streamflow_title_grob(),
              x = 0.17, y = 0.31,
              width = 0.66, height = 0.08)

  save_ig_slide(p, file_png, width, height, color_bknd)
}

#' @description IG slide 2: national flow chart with legend
#' @param file_png file path for final png
#' @param plot_nat_ig national plot styled for IG
#' @param legend legend grob from `restyle_legend()`
#' @param date_start first day of focal month
#' @param width width of final png in pixels
#' @param height height of final png in pixels
#' @param color_bknd background color
#' @param text_color Color of text in plot
#' @param font_legend font styling
#' @param flow_label Flow percentile label placed above legend
ig_national_slide <- function(file_png, plot_nat_ig, legend, date_start, width, height,
                              color_bknd, text_color, font_legend, flow_label){
  p <- ggdraw() +
    draw_label(ig_month_year(date_start),
               x = 0.5, y = 0.95,
               size = 18,
               fontfamily = font_legend,
               color = text_color) +
    draw_plot(plot_nat_ig,
              x = 0.06, y = 0.29,
              width = 0.88, height = 0.6) +
    draw_label(flow_label,
               x = 0.13, y = 0.235,
               size = 8,
               hjust = 0,
               fontfamily = font_legend,
               color = text_color) +
    draw_grob(legend,
              x = 0, y = 0.09,
              width = 1, height = 0.13) +
    draw_grob(streamflow_title_grob(),
              x = 0.17, y = 0.002,
              width = 0.66, height = 0.08)

  save_ig_slide(p, file_png, width, height, color_bknd)
}

#' @description IG slide 3: state cartogram tiles
#' @param file_png file path for final png
#' @param plot_cart state tiles styled for IG
#' @param date_start first day of focal month
#' @param width width of final png in pixels
#' @param height height of final png in pixels
#' @param color_bknd background color
#' @param text_color Color of text in plot
#' @param font_legend font styling
ig_cartogram_slide <- function(file_png, plot_cart, date_start, width, height,
                               color_bknd, text_color, font_legend){
  # pre-render tiles so shadows stay crisp
  tiles <- render_plot_hires(plot_cart +
                               theme(text = element_text(family = font_legend, color = text_color),
                                     strip.text = element_text(size = 6, vjust = -3)),
                             width_px = round(1.25 * 1450),
                             height_px = round(1.4 * 1450 * height / width))

  p <- ggdraw() +
    draw_label(ig_month_year(date_start),
               x = 0.5, y = 0.95,
               size = 18,
               fontfamily = font_legend,
               color = text_color) +
    draw_image(tiles,
               x = 1.12, y = -0.18,
               width = 1.25, height = 1.4,
               hjust = 1, vjust = 0) +
    draw_grob(streamflow_title_grob(),
              x = 0.17, y = 0.002,
              width = 0.66, height = 0.08)

  save_ig_slide(p, file_png, width, height, color_bknd)
}
