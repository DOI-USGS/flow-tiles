#' @description Create tile grid for state map
make_carto_grid <- function(){
  us_state_grid1 %>% 
    as_tibble() %>%
    add_row(row = 7, col = 11, code = "PR", name = "Puerto Rico") %>% # add PR
    filter(code != "DC") # remove DC (only has 3 gages)
}
#' @description Pull state fips code to bind to state grid
get_state_fips <- function(){
  maps::state.fips %>% 
    distinct(fips, abb) %>%
    add_row(fips = 02, abb = 'AK')%>%
    add_row(fips = 15, abb = 'HI')%>%
    add_row(fips = 72, abb = 'PR') %>%
    mutate(state_cd = str_pad(fips, 2, "left", pad = "0"))
}

#' @description Basic plotting theme
#' @param base Font size for relative scaling
#' @param color_bknd The final plot background color
#' @param text_color Color for the font
theme_flowfacet <- function(base = 12, color_bknd, text_color){
  theme_classic(base_size = base) +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12, vjust = 1, color = text_color),
          strip.placement = "inside",
          strip.background.x = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size = 14, face = "bold"),
          plot.background = element_blank(),
          panel.background = element_blank(),
          panel.spacing.x = unit(-2, "pt"),
          panel.spacing.y = unit(-5, "pt"),
          plot.margin = margin(0, 0, 0, 0, "pt"),
          legend.box.background = element_rect(fill = color_bknd, color = NA))
          
 }

#' @description Plot states as tiled cartogram
#' @param fips State codes
#' @param pal color palette for each bin level
#' @param usa_grid the grid layout for plotting with
#' @param color_bknd Plot background color
#' @param sigma_val Value assigned to sigma (blurring) in `ggfx::with_shawdow()`
#' @param xoffset_val Value assigned to x_offset (offset of the shadow) in  `ggfx::with_shawdow()`
#' @param yoffset_val Value assigned to y_offset (offset of the shadow) in  `ggfx::with_shawdow()`
plot_state_cartogram <- function(state_data, fips, pal, usa_grid, color_bknd, sigma_val, xoffset_val, yoffset_val){
  state_data %>% 
    left_join(fips) %>% # to bind to cartogram grid
    ggplot(aes(date, prop)) +
    with_shadow(
      geom_area(aes(fill = percentile_cond)),
      colour = "black",
      x_offset = xoffset_val,
      y_offset = yoffset_val,
      sigma = sigma_val,
      stack = TRUE,
      with_background = FALSE
    ) +
    scale_fill_manual(values = rev(pal)) +
    facet_geo(~abb, grid = usa_grid, move_axes = FALSE) +
    scale_y_continuous(trans = "reverse") +
    theme_flowfacet(base = 12, color_bknd, text_color) +
    theme(plot.margin = margin(50, 50, 50, 50, "pt"),
          panel.spacing.y = unit(-5, "pt"),
          panel.spacing.x = unit(4, "pt"),
          strip.text = element_text(vjust = -1),
          legend.position = 'none'
          )+
    coord_fixed(ratio = 28)

}

#' @description Plot nationa level flow conditions
#' @param national_data The proportion of sites in each flow condition, daily
#' @param date_start first day of focal month
#' @param date_end last day of focal month
#' @param pal color palette for each bin level
#' @param color_bknd Plot background color
#' @param axis_title_size manual adjustment of axis title size in theming 
#' @param axis_text_size manual adjustmet of axis text sizing in theming
#' @paramam axis_title_bottom_size manual adjustment of axis title bottom sizing in theming
plot_national_area <- function(national_data, date_start, date_end, pal, color_bknd, axis_title_size,
                               axis_text_size, axis_title_bottom_size, axis_title_top_size){
  
  # to label flow categories
  sec_labels <- national_data  %>%
    filter(date == max(national_data$date)) %>%
    distinct(percentile_cond, prop) %>%
    mutate(prop = cumsum(prop))
  
  plot_nat <- national_data %>% 
    ggplot(aes(date, prop)) +
    geom_area(aes(fill = percentile_bin)) +
    theme_classic() +
    # For feb 2025 do "-27" instead of "-30" for x axis label
    labs(x = lubridate::month(date_end - 27, label = TRUE, abbr = FALSE), #
         y="% of Streamgages") +
    #labs(x = lubridate::month(date_end - 30, label = TRUE, abbr = FALSE),
    #     y="") +
    scale_fill_manual(values = rev(pal)) +
    scale_y_continuous(trans = "reverse", #
                       breaks = rev(c(0, 0.25, 0.5, 0.75, 1)), #
                       labels = c("0%", "25%", "50%", "75%", "100%") #
    ) +
    theme_flowfacet(base = 12, color_bknd, text_color) +
    theme(axis.text.y = element_text(size = axis_text_size, #
                                     vjust = 0.5, #c(1, 0), #
                                     hjust = 1), #
          axis.title.y = element_text(size = axis_title_bottom_size, #
                                      margin = margin(r = 20)),
          axis.title.x.bottom = element_text(size = axis_title_bottom_size,
                                             vjust = -1,
                                             margin = margin(t = 10)),
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
  
  
  return(plot_nat)
}

#' @description Compose the final plot and annotate
#' @param file_out Filepath to save to
#' @param plot_left The national plot to position on the left
#' @param plot_right The state tiles to position on the right
#' @param date_start first day of focal month
#' @param width Desired width of output plot
#' @param height Desired height of output plot
#' @param color_bknd Plot background color
#' @param text_color Color of text in plot
#' @param font_legend font styling 
#' @param source_label Source label placed in bottom right of plot
combine_plots <- function(file_svg, plot_left, plot_right, date_start, width, height, color_bknd, text_color, font_legend, source_label){
  
  plot_month <- lubridate::month(date_start, label = TRUE, abbr = FALSE)
  plot_year <- lubridate::year(date_start)
  
  # import fonts
  font_legend <- 'Noto Sans Mono'
  font_add_google(font_legend)
  showtext_opts(dpi = 300, regular.wt = 200, bold.wt = 700)
  showtext_auto(enable = TRUE)

  
  # usgs logo
  usgs_logo <- magick::image_read('in/usgs_logo.png') %>%
    magick::image_colorize(100, text_color)
  
  # streamflow title
  title_flow <- magick::image_read('in/streamflow.png')
  
  plot_margin <- 0.025
  
  # background
  canvas <- grid::rectGrob(
    x = 0, y = 0, 
    width = 16, height = 9,
    gp = grid::gpar(fill = color_bknd, alpha = 1, col = color_bknd)
  )
  
  # Restyle legend
  plot_left <- plot_left +
    guides(fill = guide_colorsteps(
      title = "",
      nrow = 1,
      direction = 'horizontal',
      label.position = "bottom",
      barwidth = 22,
      barheight = 1,
      background = element_rect(fill = NA),
      show.limits = TRUE,
      even.steps = TRUE
    )) +
      theme(legend.background = element_rect(fill = NA),
            text = element_text(family = font_legend, color = text_color))
  
  # Extract from plot
  plot_legend <- get_legend(plot_left)

  # compose final plot
  ggdraw(ylim = c(0,1), 
         xlim = c(0,1)) +
    # a white background
    draw_grob(canvas,
              x = 0, y = 1,
              height = 9, width = 16,
              hjust = 0, vjust = 1) +
    # national-level plot
    draw_plot(plot_left+theme(legend.position = 'none'),
              x = plot_margin*2,
              y = 0.25,
              height = 0.45 ,
              width = 0.3-plot_margin*2) +
    # state tiles
   draw_plot(plot_right+theme(text = element_text(family = font_legend, color = text_color)),
             x = 1,
             y = 0+plot_margin*2,
             height = 1- plot_margin*4, 
             width = 1-(0.3+plot_margin*3),
             hjust = 1,
             vjust = 0) +
    # add legend
    draw_plot(plot_legend,
              x = plot_margin*2,
              y = 0.1,
              height = 0.13 ,
              width = 0.3-plot_margin) +
    # draw title
   draw_label(sprintf('%s %s', plot_month, plot_year),
              x = plot_margin*2, y = 1-plot_margin*4, 
              size = 42, 
              hjust = 0, 
              vjust = 1,
              fontfamily = font_legend,
              color = text_color,
              lineheight = 1)  +
    # stylized streamflow title
    draw_image(title_flow,
               x = plot_margin*2,
               y = 1-(6*plot_margin),
               height = 0.1, 
               width = 0.55,
               hjust = 0,
               vjust = 1) +
    # percentile info
    draw_label("Flow percentile at USGS streamgages\nrelative to the historic record.", 
               x = plot_margin*2,
               y = 0.24,
               hjust = 0,
               vjust = 1,
               fontfamily = font_legend,
               color = text_color) +
    # add data source
    draw_label(source_label, 
               x = 1-plot_margin*2, y = plot_margin*2, 
               fontface = "italic", 
               size = 14, 
               hjust = 1, vjust = 0,
               fontfamily = font_legend,
               color = text_color,
               lineheight = 1.1) +
   # add logo
  draw_image(usgs_logo, x = plot_margin*2, y = plot_margin*2, width = 0.1, hjust = 0, vjust = 0, halign = 0, valign = 0)
  
  # Save and convert file
  ggsave(file_svg, width = width, height = height, dpi = 300)
  return(file_svg)
  
}

#' @description Remove clipping masks from facets
#' @param file_in Filepath to svg output
#' @param file_out Filepath to save
rm_facet_clip <- function(svg_in, file_out, width){
  
  # Read in svg
  x <- read_xml(svg_in) 
  
  # Find defs with clipPath children
  x_clips <- x %>%
    xml_children() %>%
    xml_ns_strip() %>% 
    xml_find_all("//defs") %>%
    xml_children() %>%
    xml_find_all("//clipPath") 
  
  # Drop clipPaths around each tile
  x_drop <- x_clips[4:length(x_clips)] # 4 is based on manual review of svg
  # TODO: find clipPaths using shared attr
  xml_remove(x_drop)
  
  # Add xmlns back in and save svg
  xml_set_attr(x, attr = "xmlns", 'http://www.w3.org/2000/svg')
  write_xml(x, file = svg_in)
  # Render the svg into a png image with rsvg via magick
  img <- magick::image_read_svg(svg_in, width = width*300)
  magick::image_write(img, file_out)

}

#' @description Adjusting legend for flow timeseries national plot - Instagram 
#' @param plot_nat  Plot flow timeseries nationally
#' @param text_color Color of text in plot
#' @param font_legend font styling 
restyle_legend <- function(plot_nat, text_color, font_legend, barwidth, barheight, text_size){
  
  # Restyle legend
  plot_nat <- plot_nat +
    guides(fill = guide_colorsteps(
      title = "",
      nrow = 1,
      direction = 'horizontal',
      label.position = "bottom",
      barwidth = barwidth,
      barheight = barheight,
      background = element_rect(fill = NA),
      show.limits = TRUE,
      even.steps = TRUE
    )) +
    theme(legend.background = element_rect(fill = NA),
          text = element_text(family = font_legend, color = text_color, size = text_size))
  
  get_legend(plot_nat)
  
}

# national level flow time series  - instagram versioning (slide 1)
#' @description Compose the final plot and annotate
#' @param file_png Filepath to save to
#' @param plot_nat The national plot styled for instagram 
#' @param date_start First day of focal month
#' @param width Desired width of output plot
#' @param height Desired height of output plot
#' @param color_bknd Plot background color
#' @param text_color Color of text in plot
#' @param flow_label Flow percentile label placed above legend
#' @param source_label Source label placed in bottom right of plot
#' @param restyle_legend re-stylizing legend national flow timeseries plot
#' @param font_legend font styling 
#' @param low_col Hex color for low streamflow label and arrow
#' @param high_col Hex color for high streamflow label and arrow
#' @param low_lab Character string for low streamflow label
#' @param high_lab Character string for high streamflow label
#' @param typ_lab Character string for typical/normal streamflow label
#' @param typ_lab_ypos Numeric value for typical label y-position (0-1)
#' @param typ_arr_ypos Numeric value for typical arrow y-position (0-1)
national_ig <- function(file_png, plot_nat_ig, date_start, width, height, color_bknd,
                        text_color, flow_label, source_label, restyle_legend, 
                        font_legend, low_col, high_col, low_lab, high_lab, typ_lab, 
                        typ_lab_ypos, typ_arr_ypos){
  
  plot_month <- lubridate::month(date_start, label = TRUE, abbr = FALSE)
  plot_year <- lubridate::year(date_start)

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
      geom_curve(aes(x = 13, y = 3,
                     xend = 11, yend = 2),
                 arrow = grid::arrow(length = unit(1.25, 'lines')),
                 curvature = -0.3, angle = 80, ncp = 10,
                 color = high_col, linewidth = 1.25))
  
  (normal_range_arrow <- ggplot() +
      theme_void()+
      geom_curve(aes(x = 13, y = 3,
                     xend = 11, yend = 3),
                 arrow = grid::arrow(length = unit(1.25, 'lines')),
                 curvature = 0, angle = 100, ncp = 10,
                 color = text_color, linewidth = 1.25))
  
  (low_range_arrow <- ggplot() +
      theme_void()+
      geom_curve(aes(x = 13, y = 3,
                     xend = 11, yend = 3),
                 arrow = grid::arrow(length = unit(1.25, 'lines')),
                 curvature = 0, angle = 100, ncp = 10,
                 color = low_col, linewidth = 1.25))
  
  (high_range_arrow <- ggplot() +
      theme_void()+
      geom_curve(aes(x = 13, y = 3,
                     xend = 11, yend = 3),
                 arrow = grid::arrow(length = unit(1.25, 'lines')),
                 curvature = 0, angle = 100, ncp = 10,
                 color = high_col, linewidth = 1.25))
  
  # compose final plot
  ggdraw(ylim = c(0,1), 
         xlim = c(0,1)) +
    # a white background
    draw_grob(canvas,
              x = 0, y = 1,
              height = 0.37, width = 0.37,
              hjust = 0, vjust = 1) +
    # national-level plot
    draw_plot(plot_nat_ig + labs(x = "Day of month") +
                theme(legend.position = 'none',
                      text = element_text(family = font_legend,
                                          color = text_color)),
              x = -0.065,
              y = 0.32,
              height = 0.44 ,
              width = (1-plot_margin)*1.08) +
    # High streamflow label
    draw_label(high_lab,
               x = 0.787,
               y = 0.688,
               size = 22,
               hjust = 0.5,
               vjust = 0,
               fontfamily = font_legend,
               color = high_col) +
    # High streamflow arrow
    draw_plot(high_range_arrow,
              x = 0.75,
              y = 0.712,
              height = 0.06,
              width = 0.06,
              hjust = 1,
              vjust = 0.5) +
    # Typical streamflow label
    draw_label(typ_lab,
               x = 0.797,
               y = typ_lab_ypos,
               size = 22,
               hjust = 0.5,
               vjust = 0,
               fontfamily = font_legend,
               color = text_color) +
    # Typical streamflow arrow
    draw_plot(normal_range_arrow,
              x = 0.75,
              y = typ_arr_ypos,
              height = 0.06,
              width = 0.06,
              hjust = 1,
              vjust = 0.5) +
    # Low streamflow label
    draw_label(low_lab,
               x = 0.787,
               y = 0.37,
               size = 22,
               hjust = 0.5,
               vjust = 0,
               fontfamily = font_legend,
               color = low_col) +
    # Low streamflow arrow
    draw_plot(low_range_arrow,
              x = 0.75,
              y = 0.395,
              height = 0.06,
              width = 0.06,
              hjust = 1,
              vjust = 0.5) + 
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
               size = 24) 
  
  # Save and convert file
  ggsave(file_png, width = width, height = height, dpi = 300, units = c("px"))
  return(file_png)
}

# flow timeseries for states - instagram versioning (slide 2)
#' @description Compose the final plot and annotate
#' @param file_out Filepath to save to
#' @param plot_nat The national plot to position on the left
#' @param plot_cart The state tiles to position on the right
#' @param date_start first day of focal month
#' @param width Desired width of output plot
#' @param height Desired height of output plot
#' @param color_bknd Plot background color
#' @param text_color Color of text in plot
#' @param flow_label Flow percentile label placed above legend
#' @param source_label Source label placed in bottom right of plot
#' @param restyle_legend Restylizing legend national flow timeseries plot
#' @param font_legend font styling 
cartogram_ig <- function(file_svg, plot_nat, plot_cart, date_start, width, height, color_bknd,
                         text_color, flow_label, source_label, restyle_legend, font_legend){
  plot_month <- lubridate::month(date_start, label = TRUE, abbr = FALSE)
  plot_year <- lubridate::year(date_start)
  
  # usgs logo
  usgs_logo <- magick::image_read('in/usgs_logo.png') %>%
    magick::image_colorize(100, text_color) |> magick::image_scale('250x')
  
  # streamflow title
  title_flow <- magick::image_read('in/streamflow.png')|> magick::image_scale('800x')
  
  plot_margin <- 0.025
  
  # background
  canvas <- grid::rectGrob(
    x = 0, y = 0, 
    width = 16, height = 9,
    gp = grid::gpar(fill = color_bknd, alpha = 1, col = color_bknd)
  )
  
  #  # Extract from plot
  # plot_legend <- get_legend(restyle_legend)
  
  # compose final plot
  ggdraw(ylim = c(0,1), 
         xlim = c(0,1)) +
    # a white background
    draw_grob(canvas,
              x = 0, y = 1,
              height = 0.37, width = 0.37,
              hjust = 0, vjust = 1) +
    # state tiles
    draw_plot(plot_cart+theme(text = element_text(family = font_legend, color = text_color),
                               strip.text = element_text(size = 6, vjust = -3)),
              x = 1.09,
              y = -0.05,
              height = 1.27,
              width = 1.17,
              hjust = 1,
              vjust = 0) + 
    # draw title
    draw_label(sprintf('%s %s', plot_month, plot_year),
               x = plot_margin*1.4, y = 1-plot_margin*1.2,
               size = 16,
               hjust = 0,
               vjust = 1,
               fontfamily = font_legend,
               color = text_color,
               lineheight = 1)  +
    # stylized streamflow title
    draw_image(title_flow ,
               x = plot_margin*1.4,
               y = 1-(1.5*plot_margin),
               height = 0.16,
               width = 0.74,
               hjust = 0,
               vjust = 1) +
    # add legend
    draw_plot(restyle_legend,
              x = (1-plot_margin)*0.5,
              y = 0.07,
              height = 0.12 ,
              width = 0.02-plot_margin) +
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
    # add logo
    draw_image(usgs_logo, x = plot_margin*2, y = plot_margin*1, width = 0.125, hjust = 0, vjust = 0, halign = 0, valign = 0)
  
  # Save and convert file
  ggsave(file_svg, width = width, height = height, dpi = 300, units = c("px"))
  return(file_svg)
  
}


# Create list of state level cartograms 
#' @param state_data df with proportion of sites in each flow category by each state
#' @param filter_state vector of states to filter making of cartograms by 
#' @param fips State codes
#' @param pal color palette for each bin level
#' @param usa_grid the grid layout for plotting with
#' @param color_bknd Plot background color
#' @param sigma_val Value assigned to sigma (blurring) in `ggfx::with_shawdow()`
#' @param xoffset_val Value assigned to x_offset (offset of the shadow) in  `ggfx::with_shawdow()`
#' @param yoffset_val Value assigned to y_offset (offset of the shadow) in  `ggfx::with_shawdow()`
#' @param date_start first day of focal month
#' @param date_end last day of focal month
#' @param text_color Color of text in plot
#' @param axis_title_size manual adjustment of axis title size in theming 
#' @param axis_text_size manual adjustmet of axis text sizing in theming
#' @param axis_title_bottom_size manual adjustment of axis title bottom sizing in theming
#' @param axis_title_top_size manual adjustment of axis title top sizing in theming
#' @return list of individual state level cartograms 
plot_state_cartogram_long <- function(state_data, filter_states, fips, pal, usa_grid, color_bknd, sigma_val, xoffset_val, 
                                      yoffset_val, font_legend, text_color, date_end, date_start, 
                                      axis_title_size, axis_text_size, axis_title_bottom_size, axis_title_top_size){
  
  states_cart_list <- state_data %>% 
    left_join(fips) %>%
    split(.$abb) 
  
  filter_states_cart_list <- function(states_cart_list, filter_states) {
    filtered_list <- map(states_cart_list, ~ filter(.x, abb %in% filter_states))
    filtered_list <- keep(filtered_list, ~ nrow(.x) > 0)  # Keep only non-empty data frames
    return(filtered_list)
  }
  
  filtered_states_cart_list <- filter_states_cart_list(states_cart_list, filter_states)
  
  states_cart_list <- filtered_states_cart_list |> 
    map(~ ggplot(data = .x, aes(date, prop)) +
          with_shadow(
            geom_area(aes(fill = percentile_cond)),
            colour = "black",
            x_offset = xoffset_val,
            y_offset = yoffset_val,
            sigma = sigma_val,
            stack = TRUE,
            with_background = FALSE
          ) +
          theme_classic() +
          scale_fill_manual(values = rev(pal)) +
          scale_y_continuous(trans = "reverse") +
          theme_flowfacet(base = 12, color_bknd, text_color) +
          ggtitle(paste(unique(.x$abb))) +
          labs(x = lubridate::month(date_end - 30, label = TRUE, abbr = FALSE),
               y="") +
          scale_fill_manual(values = rev(pal)) +
          scale_y_continuous(trans = "reverse",
                             breaks = rev(c(0.05,0.5, 0.95)), 
                             labels = c("0%","gages","100%"),
                             sec.axis = dup_axis(
                               labels = c("Dry", "", "Wet")
                             )) +
          theme(axis.text.y = 
                  element_text(size = axis_text_size,
                               vjust = c(1, 0), 
                               hjust = 1),
                axis.title.x.bottom = element_text(size = axis_title_bottom_size,
                                                   vjust = -1,
                                                   margin = margin(t = 5)),
                axis.title.x.top = element_text(size = axis_title_top_size,
                                                vjust = 0,
                                                margin = margin(b = -5)),
                axis.text.x.bottom = element_text(size = axis_text_size,
                                                  vjust = 1,
                                                  # nudge labels up closer to bottom
                                                  margin = margin(t = -7)),
                plot.margin = margin(50, 50, 50, 50, "pt"),
                legend.position = 'none',
                plot.title = element_text(hjust = 0.5, size = 55, margin = margin(10, 0, 5, 0),
                                          family = font_legend, color = text_color),
                text = element_text(family = font_legend)
          ) +
          scale_x_date(breaks = seq.Date(date_start, date_end, "1 week"),
                       position = "bottom",
                       labels = lubridate::day(seq.Date(date_start, date_end, "1 week")),
                       sec.axis = dup_axis(
                         name = " "
                       )) +
          coord_fixed(ratio = 28, clip = "off") +
          labs(x = NULL)
    )
  
  return(states_cart_list)
}

# State level cartograms in long format for instragram story (9:16)
#' @param state_plot_list list of individual state cartograms
#' @param state_names names of individual states to purr::map by
#' @param create_out_folder sub folder path for state level pngs
#' @param date_start first day of focal month
#' @param date_emd last day of focal month
#' @param width Desired width of output plot
#' @param height Desired height of output plot
#' @param background_color Plot background color
#' @param text_color Color of text in plot
#' @param flow_label Flow percentile label placed above legend
#' @param source_label Source label placed in bottom right of plot
#' @param restyle_legend Restylizing legend for state level plots 
#' @param font_legend font styling 
#' @return long format state cartograms in file format [state_name]_[month]_[year].png
plot_state_long <- function(state_plot_list, filter_states, width, height, 
                                 dpi, create_out_folder,
                                 background_color, text_color,
                                 date_start, source_label, flow_label,
                                 restyle_legend) {
  
  # pull out indiviudal state names and state cartograms to `purr::map` by 
  state_names <- pluck(filter_states, 1)
  state_plot_list <- pluck(state_plot_list, 1)
  
  # The background canvas for your viz
  canvas <- grid::rectGrob(
    x = 0, y = 0,
    width = width, height = height,
    gp = grid::gpar(fill = background_color, alpha = 1, col = background_color
    )
  )
  
  plot_margin =  0.025
  
  # pull out month and year
  plot_month <- lubridate::month(date_start, label = TRUE, abbr = FALSE)
  plot_year <- lubridate::year(date_start)
  
  # streamflow label
  title_flow <- magick::image_read('in/streamflow.png') |> magick::image_scale('800x')
  
  # usgs logo
  usgs_logo <- magick::image_read('in/usgs_logo.png') %>%
    magick::image_colorize(100, text_color)
  
  # compose final plot
  state_plts <- ggdraw(ylim = c(0,1),
         xlim = c(0,1)) +
    # Background
    draw_grob(canvas,
              x = 0, y = 1,
              height = 0.37, width = 0.37,
              hjust = 0, vjust = 1) +
    # Add main plot for each individual state
    draw_plot(state_plot_list,
              x = (1-plot_margin)*-0.04,
              y = -0.01,
              height = 1.1,
              width = (1-plot_margin)*1.1) +
    # Draw title
    draw_label(sprintf('%s %s', plot_month, plot_year),
               x = plot_margin*1.7, y = 1-plot_margin*2.4,
               size = 60,
               hjust = 0,
               vjust = 1,
               fontfamily = font_legend,
               color = text_color,
               lineheight = 1) +
    # Stylized streamflow title
    draw_image(title_flow,
               x = plot_margin*1.8,
               y = 1-plot_margin*-1.5,
               height = 0.36,
               width = 0.94,
               hjust = 0,
               vjust = 1) + 
    # Explainer text
    draw_label(source_label,
               fontfamily = font_legend,
               x = 1-plot_margin*2.4,
               y = plot_margin,
               size = 16,
               hjust = 1, vjust = 0,
               color = text_color,
               lineheight = 1.1) +
    # Add logo
    draw_image(usgs_logo,
               x = plot_margin*2.65,
               y = plot_margin*1,
               width = 0.175,
               hjust = 0,
               vjust = 0,
               halign = 0,
               valign = 0) + 
    # Add legend
    draw_plot(restyle_legend,
              x = (1-plot_margin)*0.31,
              y = -0.04,
              height = 0.42,
              width = 0.42-plot_margin) +
    # Percentile info
    draw_label(flow_label,
               x = (1-plot_margin)*0.13,
               y = 0.25,
               hjust = 0,
               vjust = 1,
               fontfamily = font_legend,
               color = text_color,
               size = 18)
    
  outfolder_month_year_path <- paste0(create_out_folder,"_", plot_month, "_", plot_year)
  
  out_folder <- dir.create(file.path(outfolder_month_year_path), showWarnings = FALSE)
  
  filename <- paste0(outfolder_month_year_path, "/", state_names, "_", plot_month, "_", plot_year, ".png")
  
  # Save and convert file
  ggsave(filename, width = width, height = height, dpi = dpi) 
  return(filename)
}