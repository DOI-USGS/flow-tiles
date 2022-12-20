library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c('tidyverse', 'lubridate', 'geofacet', 'cowplot','ggfx', 'showtext', 'xml2'))

source("src/prep_data.R")
source("src/plot_cartogram.R")

# wet to dry color scale
pal_wetdry <- c("#002D5E", "#0C7182", "#6CB7B0", "#A7D2D8", "#E0D796", "#AF9423", "#A84E0B")
percentile_breaks = c(0, 0.05, 0.1, 0.25, 0.75, 0.9, 0.95, 1)
percentile_labels <- c("Driest", "Drier", "Dry", "Normal","Wet","Wetter", "Wettest")
color_bknd <- "#F4F4F4"
text_color = "#444444"

# to produce the flow cartogram, run tar_make() in the console
list(
  # Read in data from gage-flow-conditions pipeline output
  tar_target(
    dv,
    read_csv("https://labs.waterdata.usgs.gov/visualizations/data/flow_conditions_202211.csv", col_types = "cTnnnn")
  ),
  tar_target(
    date_start,
    as.Date(min(dv$dateTime))
  ),
  tar_target(
    date_end,
    as.Date(max(dv$dateTime))+1 # using firest date of next month for label positioning
  ),
  # Bin percentile data 
  tar_target(
    flow,
    add_flow_condition(dv, date_start, date_end, breaks = percentile_breaks, break_labels = percentile_labels)
  ),
  # Find list of all active sites
  tar_target(
    site_list,
    unique(flow$site_no)
  ),
  # Pull site info (state) 
  tar_target(
    dv_site,
    dataRetrieval::readNWISsite(siteNumbers = site_list) %>%
      distinct(site_no, state_cd)
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
    plot_state_cartogram(state_data = flow_state, fips, pal = pal_wetdry, usa_grid, color_bknd)
  ),
  # Plot flow timeseries nationally
  tar_target(
    plot_nat,
    plot_national_area(national_data = flow_national, pal = pal_wetdry, date_start, date_end, color_bknd)
  ),
  # Combine charts and assemble final plot
  tar_target(
    flow_cartogram_svg,
    combine_plots(file_svg = "flow_cartogram.svg", 
                  plot_left = plot_nat, 
                  plot_right = plot_cart, 
                  date_start,
                  width = 16, height = 9, color_bknd),
    format = "file"
  ),
  # Remove facet clipping and save as png
  tar_target(
    flow_cartogram_png,
    rm_facet_clip(svg_in = flow_cartogram_svg, 
                  file_out = "flow_cartogram.png",
                  width = 16),
    format = "file"
  ),
  
  # Flow timeseries nationally - instagram 
  tar_target(
    flow_national_instagram_png,
    national_ig(file_svg = "flow_national_ig.png", 
                  plot_left = plot_nat, 
                  plot_right = plot_cart, 
                  date_start,
                  width = 1080, height = 1080, color_bknd),
    format = "file"
  ),
  
  # Flow timeseries for states - instagram
  tar_target(
    flow_cartogram_instagram_png,
    cartogram_ig(file_svg = "flow_cartogram_ig.png", 
                plot_left = plot_nat, 
                plot_right = plot_cart, 
                date_start,
                width = 1080, height = 1080, color_bknd),
    format = "file"
  )
  
)
