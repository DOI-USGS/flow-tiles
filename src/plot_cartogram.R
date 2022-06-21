#' @description Create tile grid for state map
make_carto_grid <- function(){
  us_state_grid1 %>% 
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
plot_state_cartogram <- function(state_data, fips, pal, usa_grid, color_bknd){
  state_data %>% 
    left_join(fips) %>% # to bind to cartogram grid
    ggplot(aes(date, prop)) +
    with_shadow(
      geom_area(aes(fill = percentile_cond)),
      colour = "black",
      x_offset = 2,
      y_offset = 2,
      sigma = 5,
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
plot_national_area <- function(national_data, date_start, date_end, pal, color_bknd){
  
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
         y="") +
    scale_fill_manual(values = rev(pal)) +
    scale_y_continuous(trans = "reverse",
                       breaks = rev(c(0.05,0.5, 0.95)), 
                       labels = c("0%","gages","100%"),
                       sec.axis = dup_axis(
                         labels = c("Dry", "", "Wet")
                       )) +
    theme_flowfacet(base = 12, color_bknd, text_color) +
    theme(axis.text.y = 
            element_text(size = 12, 
                         vjust = c(1, 0), 
                         hjust = 1),
          axis.title.x.bottom = element_text(size = 20,
                                             vjust = -1,
                                             margin = margin(t = 5)),
          axis.title.x.top = element_text(size = 20,
                                          vjust = 0,
                                          margin = margin(b = -5)),
          axis.text.x.bottom = element_text(size = 12,
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
combine_plots <- function(file_svg, plot_left, plot_right, date_start, width, height, color_bknd){
  
  plot_month <- lubridate::month(date_start, label = TRUE, abbr = FALSE)
  plot_year <- lubridate::year(date_start)
  
  # import fonts
  font_legend <- 'Noto Sans Mono'
  font_add_google(font_legend)
  showtext_opts(dpi = 300, regular.wt = 200, bold.wt = 700)
  showtext_auto(enable = TRUE)
  
  text_color <- "#444444"
  
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
      even.steps = FALSE
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
    draw_label("Flow percentile at USGS streamgages\nrelative to the historic daily record.", 
               x = plot_margin*2,
               y = 0.25,
               hjust = 0,
               vjust = 1,
               fontfamily = font_legend,
               color = text_color) +
    # add data source
    draw_label("Data: USGS National Water Information System", 
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
