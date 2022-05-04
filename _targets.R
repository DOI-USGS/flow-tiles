library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c('tidyverse', 'lubridate', 'geofacet', 'cowplot','ggfx', 'showtext'))

source("src/prep_data.R")
source("src/plot_cartogram.R")

## dates for geofacet map
date_start <- as.Date("2022-04-01")
date_end <- as.Date("2022-05-01") # Need to keep as first date of following month for labeling purposes

# wet to dry color scale
pal_wetdry <- c("#002D5E", "#0C7182", "#6CB7B0", "#A7D2D8", "#E0D796", "#AF9423", "#A84E0B")
color_bknd <- "#F4F4F4"
text_color = "#444444"

# to produce the flow cartogram, run tar_make() in the console
list(
  tar_target(
    dv,
    read_csv("https://labs.waterdata.usgs.gov/visualizations/data/flow_conditions_202204.csv", col_types = "cTnnnnT")
  ),
  tar_target(
    flow,
    add_flow_condition(dv, date_start, date_end)
  ),
  tar_target(
    dv_site,
    dataRetrieval::readNWISsite(siteNumbers = unique(flow$site_no)) %>%
      distinct(site_no, state_cd)
  ),
  tar_target(
    sites_state,
    site_count_state(flow, dv_site)
  ),
  tar_target(
    sites_national,
    site_count_national(sites_state)
  ),
  tar_target(
    flow_national,
    flow_by_day(flow, sites_national)
  ),
  tar_target(
    flow_state,
    flow_by_day_by_state(flow, dv_site, sites_state)
  ),
  tar_target(
    usa_grid,
    make_carto_grid()
  ),
  tar_target(
    fips,
    get_state_fips()
  ),
  tar_target(
    plot_cart,
    plot_state_cartogram(state_data = flow_state, fips, pal = pal_wetdry, usa_grid, color_bknd)
  ),
  tar_target(
    plot_nat,
    plot_national_area(national_data = flow_national, pal = pal_wetdry, date_start, date_end, color_bknd)
  ),
  tar_target(
    flow_cartogram_svg,
    combine_plots(file_out = "flow_cartogram.png", 
                  plot_left = plot_nat, 
                  plot_right = plot_cart, 
                  date_start,
                  width = 16, height = 9, color_bknd),
    format = "file"
  )
)
