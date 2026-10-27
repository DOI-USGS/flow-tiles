library(targets)
library(showtext)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c('tidyverse', 'lubridate', 'geofacet', 'cowplot','ggfx','showtext', 'xml2', 'dataRetrieval', 'svglite', 'rsvg', 'arrow'))

source("src/prep_data.R")
source("src/plot_cartogram.R")
source("src/ig_slides.R")

# wet to dry color scale
pal_wetdry <- c("#002D5E", "#0C7182", "#6CB7B0", "#C0C0C0", "#F0DB85", "#AF9423", "#A84E0B")
  #c("#002D5E", "#0C7182", "#6CB7B0", "#A7D2D8", "#E0D796", "#AF9423", "#A84E0B")
percentile_breaks = c(0, 0.05, 0.1, 0.25, 0.75, 0.9, 0.95, 1)
percentile_labels <- c("Driest", "Drier", "Dry", "Normal","Wet","Wetter", "Wettest")
color_bknd <- "#F4F4F4"
text_color = "#444444"

# Enable font styling   
font_legend <- 'Noto Sans Mono'
font_add_google(font_legend)
showtext_opts(dpi = 300, regular.wt = 200, bold.wt = 700)
showtext_auto(enable = TRUE)

base_url <- "https://dfi09q69oy2jm.cloudfront.net/visualizations/current_conditions/streamflow/data/sf_categorizations_"

# draw label text
flow_label <- "Streamflow percentile at USGS streamgages\nrelative to the historic record."
#"Flow percentile at USGS streamgages relative\nto the historic record."
source_label <- "Data: USGS Water Data for the Nation"

# to produce the flow cartogram, run tar_make() in the console
list(
  # Focal month: change this to switch months
  tar_target(
    focal_month,
    as.Date("2026-09-01")
  ),
  # All dates in the focal month
  tar_target(
    dates_to_pull,
    seq(focal_month, by = "day", length.out = days_in_month(focal_month))
  ),
  # Download and combine all daily parquets for the month
  tar_target(
    dv,
    fetch_month_parquets(dates_to_pull, base_url)
  ),
  tar_target(
    date_start,
    min(dates_to_pull)
  ),
  tar_target(
    date_end,
    max(dates_to_pull)
  ),
  # Map category strings to 7-level flow condition factor
  tar_target(
    flow,
    normalize_flow(dv)
  ),
  # Build site -> state_cd lookup from parquet data (state info already present)
  tar_target(
    dv_site,
    build_dv_site(dv)
  ),
  # Count the number of sites in each state
  tar_target(
    sites_state,
    site_count_state(flow, dv_site)
  ),
  # Count total number of sites nationally
  tar_target(
    sites_national,
    site_count_national(sites_state)
  ),
  # Find the proportion of sites in each flow category nationally
  tar_target(
    flow_national,
    flow_by_day(flow, sites_national)
  ),
  # Find the proportion of sites in each flow category by each state
  tar_target(
    flow_state,
    flow_by_day_by_state(flow, dv_site, sites_state)
  ),
  # Define grid for tile positioning
  tar_target(
    usa_grid,
    make_carto_grid()
  ),
  # Pull fips codes to join state data to grid
  tar_target(
    fips,
    get_state_fips()
  ),
  # Plot flow timeseries for states
  tar_target(
    plot_cart,
    plot_state_cartogram(state_data = flow_state, fips, pal = pal_wetdry, usa_grid, color_bknd,  
                         sigma_val = 5 , xoffset_val = 2, yoffset_val = 2) 
  ),
  # Plot flow timeseries nationally
  tar_target(
    plot_nat,
    plot_national_area(national_data = flow_national, pal = pal_wetdry, date_start, date_end, color_bknd,
                       axis_title_size = 20, axis_text_size = 12, axis_title_bottom_size = 20, axis_title_top_size = 20)
  ),
  # Combine charts and assemble final plot
  tar_target(
    flow_cartogram_svg,
    combine_plots(file_svg = "out/flow_cartogram.svg", 
                  plot_left = plot_nat, 
                  plot_right = plot_cart, 
                  date_start,
                  width = 16, height = 9, color_bknd, text_color, font_legend,
                  source_label),
    format = "file"
  ), 
  # Remove facet clipping and save as png
  tar_target(
    flow_cartogram_png,
    rm_facet_clip(svg_in = flow_cartogram_svg, 
                  file_out = "out/flow_cartogram.png",
                  width = 16),
    format = "file"
  ),
  
  # Plot flow timeseries for states with adjusted sigma and offset values - Instagram 
  tar_target(
    plot_cart_ig,
    plot_state_cartogram(state_data = flow_state, fips, pal = pal_wetdry, usa_grid, color_bknd,  
                         sigma_val = 2.5 , xoffset_val = 0.5, yoffset_val =0.5) 
  ),
  
  # Plot flow timeseries nationally with % and wet/dry labels - Instagram
  tar_target(
    plot_nat_ig,
    plot_national_ig(national_data = flow_national, pal = pal_wetdry, text_color, font_legend,
                     text_size = 9.5, low_lab = "Dry", high_lab = "Wet")
  ),

  # Full-bleed national plot for Instagram title slide background
  tar_target(
    plot_nat_bkgd_ig,
    plot_national_bkgd_ig(national_data = flow_national, pal = pal_wetdry, alpha = 1)
  ),

  # Restyling legend for Instagram dimensions
  tar_target(
    restyle_legend_ig,
    restyle_legend(plot_nat, text_color, font_legend,
                   barwidth = 14,
                   barheight = 0.7,
                   text_size = 7)
  ),

  # Restyling legend for Instagram story dimensions
  tar_target(
    restyle_legend_ig_story,
    restyle_legend(plot_nat, text_color, font_legend,
                   barwidth = 36,
                   barheight = 2.4,
                   text_size = 25)
  ),

  # Instagram slide 1: title over national flow
  tar_target(
    ig_1_title_png,
    ig_title_slide(file_png = "out/ig_1_title.png",
                   plot_bkgd = plot_nat_bkgd_ig,
                   date_start,
                   width = 1080, height = 1350, color_bknd,
                   text_color, font_legend),
    format = "file"
  ),

  # Instagram slide 2: national flow with legend
  tar_target(
    ig_2_national_png,
    ig_national_slide(file_png = "out/ig_2_national.png",
                      plot_nat_ig,
                      legend = restyle_legend_ig,
                      date_start,
                      width = 1080, height = 1350, color_bknd,
                      text_color, font_legend, flow_label),
    format = "file"
  ),

  # Instagram slide 3: state cartogram tiles
  tar_target(
    ig_3_cartogram_png,
    ig_cartogram_slide(file_png = "out/ig_3_cartogram.png",
                       plot_cart = plot_cart_ig,
                       date_start,
                       width = 1080, height = 1350, color_bknd,
                       text_color, font_legend),
    format = "file"
  ),
  
  
  # Pull vector of all state names across oconus
  tar_target(
    state_abbr_oconus,
    flow_state %>% 
      left_join(fips) %>%
      split(.$abb) |> 
      names()
  ),
  
  # State name(s) of interest to filter by to plot state level cartogram in long format (9:16) for Instagram story
  # For example, this can be filtered for WSC in CA or OR to post on their Instagram story 
  tar_target(
    state_abbr_of_interest,
    c("CA", "OR", "TX", "CT", "DE", "RI", "MA", "NY", "NJ")
  ),
  
  # Filter oconus list to just states of interest 
  tar_target(
    state_abbr_filter,
    state_abbr_oconus[state_abbr_oconus %in% state_abbr_of_interest]
  ),
  
  # List of state level plots of flow timeseries
  tar_target(
    plot_cart_state_ig_story_list,
    plot_state_cartogram_long(state_data = flow_state, filter_states = state_abbr_filter,
                              fips, pal = pal_wetdry, usa_grid, color_bknd,  
                              sigma_val = 2.5 , xoffset_val = 0.5, yoffset_val =0.5,
                              font_legend, text_color,
                              date_end, date_start,
                              axis_title_size = 26, axis_text_size = 18,
                              axis_title_bottom_size = 22, axis_title_top_size = 24) 
  ),
  
  # Plot state level facet in long format (9:16) for Instagram story 
  tar_target(
    plot_cart_state_ig_story,
    plot_state_long(state_plot_list = plot_cart_state_ig_story_list,
                    filter_states = state_abbr_filter,
                    width = 9, height = 16 , dpi = 300,
                    create_out_folder = "out/state_cartograms",
                    background_color = "#F4F4F4",
                    text_color = text_color,
                    date_start = date_start,
                    source_label = source_label,
                    flow_label = flow_label,
                    restyle_legend = restyle_legend_ig_story),
    pattern = map(plot_cart_state_ig_story_list, state_abbr_filter),
    format = 'file',
    iteration = 'list'
  )
)