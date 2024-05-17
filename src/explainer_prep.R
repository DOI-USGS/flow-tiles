#' @description Prep and plot national data
#' @param national_data The proportion of sites in each flow condition, daily
#' @param date_start first day of focal month
#' @param date_end last day of focal month
#' @param color_bknd Plot background color
#' @param text_color Color of the text
#' @param axis_text_size manual adjustmet of axis text sizing in theming
#' @param axis_title_bottom_size manual adjustment of axis title size in theming 
#' @param axis_title_top_size manual adjustment of top x axis title size in theming 
#' @param pal color palette for each bin level
prep_and_plot_national_data <- function(national_data, date_start, date_end, 
                                        color_bknd, text_color, axis_text_size, 
                                        axis_title_bottom_size, axis_title_top_size,
                                        pal){
  
  # to label flow categories
  sec_labels <- national_data  %>%
    filter(date == max(national_data$date)) %>%
    distinct(percentile_cond, prop) %>%
    mutate(prop = cumsum(prop))
  
  plot_nat <- national_data %>% 
    ggplot(aes(date, prop)) +
    geom_area(aes(fill = percentile_bin)) +
    theme_classic() +
    labs(x = lubridate::month(date_end - 30, label = TRUE, abbr = FALSE),
         y="% of Streamgages") +
    scale_fill_manual(values = rev(pal)) +
    scale_y_continuous(trans = "reverse",
                       breaks = rev(c(0, 0.25, 0.5, 0.75, 1)), 
                       labels = c("0%", "25%", "50%", "75%", "100%")
    ) +
    theme_flowfacet(base = 12, color_bknd, text_color) +
    theme(axis.text.y = 
            element_text(size = axis_text_size,
                         vjust = 0.5, #c(1, 0), 
                         hjust = 1),
          axis.title.y = element_text(size = axis_title_bottom_size,
                                      margin = margin(r = 5)),
          axis.title.x.bottom = element_text(size = axis_title_bottom_size,
                                             vjust = -1,
                                             margin = margin(t = 5)),
          axis.title.x.top = element_text(size = axis_title_top_size,
                                          vjust = 0,
                                          margin = margin(b = -5)),
          axis.text.x.bottom = element_text(size = axis_text_size,
                                            vjust = 1,
                                            # nudge labels up closer to bottom
                                            margin = margin(t = -7))) +
    scale_x_date(breaks = seq.Date(date_start, date_end, "1 week"),
                 position = "bottom",
                 labels = lubridate::day(seq.Date(date_start, date_end, "1 week")),
                 sec.axis = dup_axis(
                   name = "National"
                 )) +
    coord_fixed(ratio = 28, clip = "off")
  
}

#' @description Extract and restyle legend
#' @param plot_nat National data plot
#' @param barwidth Numeric: Define width of each bar in percentile bin legend
#' @param barheight Numeric: Define height of each bar in percentile bin legend
#' @param text_size Numeric: Define size of legend text
restyle_legend_explainer <- function(plot_nat, barwidth, barheight, text_size){
  # Restyle legend 
  plot_nat_legend <- plot_nat +
    guides(fill = guide_colorsteps(
      title = "",
      nrow = 1,
      direction = 'horizontal',
      label.position = "bottom",
      barwidth = barwidth,
      barheight = barheight,
      background = element_rect(fill = NA),
      show.limits = TRUE
    )) +
    theme(legend.background = element_rect(fill = NA),
          text = element_text(family = font_legend, color = text_color, size = text_size))
  
  legend <- get_legend(plot_nat_legend)
  
}

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
cowplot_national_explainer <- function(plot_nat, date_start, flow_label, 
                                       source_label, legend, explainer_label, file_png, 
                                       width, height, font_legend, text_color){

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
  
  
  # # Extract from plot
  # plot_legend <- get_legend(restyle_legend)
  # 
  # compose final plot
  ggdraw(ylim = c(0,1), 
         xlim = c(0,1)) +
    # a white background
    draw_grob(canvas,
              x = 0, y = 1,
              height = 0.37, width = 0.37,
              hjust = 0, vjust = 1) +
    # national-level plot
    draw_plot(plot_nat+ labs(x = "Day of month") + theme(legend.position = 'none',
                                                         text = element_text(family = font_legend, color = text_color)),
              x = (1-plot_margin)*0.08,
              y = 0.27,
              height = 0.54 ,
              width = (1-plot_margin)*0.8) +
    # add legend
    draw_plot(legend,
              x = (1-plot_margin)*0.5,
              y = 0.07,
              height = 0.12 ,
              width = 0.02-plot_margin) +
    # draw title
    draw_label(sprintf('%s %s', plot_month, plot_year),
               x = plot_margin*2, y = 1-plot_margin*1.2,
               size = 16,
               hjust = 0,
               vjust = 1,
               fontfamily = font_legend,
               color = text_color,
               lineheight = 1)  +
    # stylized streamflow title
    draw_image(title_flow ,
               x = plot_margin*2,
               y = 1-(1.5*plot_margin),
               height = 0.16,
               width = 0.74,
               hjust = 0,
               vjust = 1) +
    # percentile info
    draw_label(flow_label,
               x = (1-plot_margin)*0.18,
               y = 0.22,
               hjust = 0,
               vjust = 1,
               fontfamily = font_legend,
               color = text_color,
               size = 6) +
    # add data source
    draw_label(source_label, 
               x = 1-plot_margin*2, y = plot_margin, 
               fontface = "italic", 
               size = 5, 
               hjust = 1, vjust = 0,
               fontfamily = font_legend,
               color = text_color,
               lineheight = 1.1) +
    draw_label(explainer_label, 
               x = 0.5, y = 1-plot_margin*2, 
               size = 5.5, 
               hjust = 0, vjust = 1,
               fontfamily = font_legend,
               color = text_color) +
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
    draw_label("Normal Range",
               x = 0.88, y = 0.5, 
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
    draw_plot(normal_range_arrow, # for normal range
              x = 0.755, 
              y = 0.495,
              height = 0.035, 
              width = 0.04,
              hjust = 0,
              vjust = 0.5)+
    draw_plot(low_range_arrow, # for low streamflow
              x = 0.755, 
              y = 0.35,
              height = 0.035, 
              width = 0.055,
              hjust = 0,
              vjust = 0.5)+
    # add logo
    draw_image(usgs_logo, x = plot_margin*2, y = plot_margin*1, width = 0.125, hjust = 0, vjust = 0, halign = 0, valign = 0)
  
  
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
    draw_label(sprintf("How do %s's\nstreamflow\nconditions\ncompare to the\npast?", plot_month),
               x = 0.05,
               y = 0.5,
               size = 28,
               hjust = 0,
               vjust = 0.5,
               fontfamily = font_legend,
               color = "#222222")
  
  ggsave(file_png, width = width, height = height, dpi = 300, units = c("px"))
  
}