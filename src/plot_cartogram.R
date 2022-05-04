make_carto_grid <- function(){
  us_state_grid1 %>% 
    add_row(row = 7, col = 11, code = "PR", name = "Puerto Rico") %>% # add PR
    filter(code != "DC") # remove DC (only has 3 gages)
}
get_state_fips <- function(){
  maps::state.fips %>% 
    distinct(fips, abb) %>%
    add_row(fips = 02, abb = 'AK')%>%
    add_row(fips = 15, abb = 'HI')%>%
    add_row(fips = 72, abb = 'PR') %>%
    mutate(state_cd = str_pad(fips, 2, "left", pad = "0"))
}

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
          legend.position = "none",
          plot.margin = margin(0, 0, 0, 0, "pt"))
          
 }

plot_state_cartogram <- function(state_data, fips, pal, usa_grid, color_bknd){
  state_data %>% 
    left_join(fips) %>% # to bind to cartogram grid
    ggplot(aes(date, prop)) +
    with_shadow(
      geom_area(aes(fill = cond)),
      colour = "black",
      x_offset = 2,
      y_offset = 2,
      sigma = 5,
      stack = TRUE
    ) +
    scale_fill_manual(values = rev(pal)) +
    facet_geo(~abb, grid = usa_grid, move_axes = FALSE) +
    scale_y_continuous(trans = "reverse") +
    theme_flowfacet(base = 12, color_bknd, text_color) +
    theme(plot.margin = margin(50, 50, 50, 50, "pt"),
          #panel.spacing.y = unit(-8, "pt"),
          strip.text.x = element_text(margin = margin(b = -5)),
          panel.spacing.x = unit(4, "pt"),
          strip.text = element_text(vjust = -1))+
    coord_fixed(ratio = 28)

}

plot_national_area <- function(national_data, date_start, date_end, pal, color_bknd){
  # to label flow categories
  sec_labels <- national_data  %>%
    filter(date == max(national_data$date)) %>%
    distinct(cond, prop) %>%
    mutate(prop = cumsum(prop)) %>%
    # control positions of categorical labels
    mutate(position = case_when(
      cond == 'Wettest' ~ 1,
      cond == 'Driest' ~ 0,
      cond == 'Wetter' & prop > 0.95 ~ 0.95,
      cond == 'Wet' & prop > 0.9 ~ 0.9,
      cond == 'Drier' & prop < 0.05 ~ 0.05,
      cond == 'Dry' & prop < 0.1 ~ 0.1,
      TRUE ~ prop
    ),
    # shift labels to be within y-extent of national plot
    label_vjust = case_when(
      cond == 'Wettest' ~ 1,
      cond == 'Driest' ~ 0, 
      TRUE ~ 0.5
    ))
  
  # force top and bottom labels to 0 and 1
  
  plot_nat <- national_data %>% 
    ggplot(aes(date, prop)) +
    geom_area(aes(fill = cond)) +
    theme_classic() +
    labs(x = lubridate::month(date_end - 30, label = TRUE, abbr = FALSE),
         y="") +
    scale_fill_manual(values = rev(pal)) +
    scale_y_continuous(trans = "reverse",
                       breaks = rev(c(0.05,0.08, 0.15, 0.5, 0.75, 0.92, 0.95)), 
                       labels = c("0%","","","","", "","100%"),
                       sec.axis = dup_axis(
                         breaks = sec_labels$position,
                         labels = sec_labels$cond,
                         guide = guide_axis(n.dodge = 7)
                       )) +
    theme_flowfacet(base = 12, color_bknd, text_color) +
    theme(axis.text.y = 
            element_text(size = 12, 
                         vjust = c(1, 0), # align yaxis labels to plot bounds
                         hjust = 1),
          axis.title.x.bottom = element_text(size = 24,
                                             vjust = -1,
                                             margin = margin(t = 5)),
          axis.title.x.top = element_text(size = 24,
                                          vjust = 0,
                                          margin = margin(b = -5)),
          axis.text.x.bottom = element_text(size = 14,
                                            vjust = 1,
                                            # nudge labels up closer to bottom
                                            margin = margin(t = -7))) +
    scale_x_date(breaks = seq.Date(date_start, date_end, "1 week"),
                 position = "bottom",
                 labels = lubridate::day(seq.Date(date_start, date_end, "1 week")),
                 sec.axis = dup_axis(
                   name = "National"
                 )) +
    guides(fill = guide_legend("")) +
    coord_fixed(ratio = 28, clip = "off")
  
  plot_nat
  
  return(plot_nat)
}

combine_plots <- function(file_out, plot_left, plot_right, date_start, width, height, color_bknd){
  
  plot_month <- lubridate::month(date_start, label = TRUE, abbr = FALSE)
  plot_year <- lubridate::year(date_start)
  
  # import fonts
  font_legend <- 'Noto Sans Mono'
  font_add_google(font_legend)
  showtext_opts(dpi = 300, regular.wt = 200, bold.wt = 700)
  showtext_auto(enable = TRUE)
  
  text_color <- "#444444"
  
  # logo
  usgs_logo <- magick::image_read('../logo/usgs_logo_white.png') %>%
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
  
  ggdraw(ylim = c(0,1), 
         xlim = c(0,1)) +
    # a white background
    draw_grob(canvas,
              x = 0, y = 1,
              height = 9, width = 16,
              hjust = 0, vjust = 1) +
    # national-level plot
    draw_plot(plot_left+theme(text = element_text(family = font_legend, color = text_color)),
              x = plot_margin*2,
              y = 0.25,
              height = 0.45 ,
              width = 0.3-plot_margin*4) +
    # state tiles
    draw_plot(plot_right+theme(text = element_text(family = font_legend, color = text_color)),
              x = 1-plot-margin*2,
              y = 0+plot_margin*2,
              height = 1- plot_margin*4, 
              width = 1-(0.3+plot_margin*2),
              hjust = 1,
              vjust = 0) +
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
  
  
  ggsave(file_out, width = width, height = height, dpi = 300)
  return(file_out)
  
}