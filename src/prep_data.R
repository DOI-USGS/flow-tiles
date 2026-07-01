
#' @description Fetch one day's parquet from the current conditions data source
#' @param date A single Date value
#' @param base_url Base URL prefix; date (YYYY-MM-DD) and .parquet are appended
fetch_one_parquet <- function(date, base_url) {
  url <- paste0(base_url, format(date, "%Y-%m-%d"), ".parquet")
  tmp <- tempfile(fileext = ".parquet")
  tryCatch({
    download.file(url, tmp, quiet = TRUE, mode = "wb")
    arrow::read_parquet(tmp, col_select = c("monitoring_location_id", "state_name", "category", "runtime"))
  }, error = function(e) {
    warning(paste("Could not fetch parquet for", format(date), ":", e$message))
    tibble()
  })
}

#' @description Download and combine all daily parquets for the focal month
#' @param dates_to_pull Vector of Dates covering the focal month
#' @param base_url Base URL prefix for parquet files
fetch_month_parquets <- function(dates_to_pull, base_url) {
  map_dfr(dates_to_pull, fetch_one_parquet, base_url = base_url)
}

#' @description Map new parquet category strings to the 7-level flow condition factor
#' @param dv_new Combined parquet data (monitoring_location_id, state_name, category, runtime)
normalize_flow <- function(dv_new) {
  category_map <- c(
    "<0" = "Driest", "0-5" = "Driest",
    "5-10" = "Drier",
    "10-25" = "Dry",
    "25-75" = "Normal",
    "75-90" = "Wet",
    "90-95" = "Wetter",
    "95-100" = "Wettest", ">100" = "Wettest"
  )
  cond_levels <- c("Driest", "Drier", "Dry", "Normal", "Wet", "Wetter", "Wettest")
  # guide_colorsteps() requires (lower, upper] format — mirrors what cut() used to produce
  bin_levels <- c("(0,0.05]", "(0.05,0.1]", "(0.1,0.25]", "(0.25,0.75]", "(0.75,0.9]", "(0.9,0.95]", "(0.95,1]")
  bin_map <- setNames(bin_levels, cond_levels)

  dv_new |>
    filter(!is.na(category), category != "NA") |>
    mutate(
      site_no = str_remove(monitoring_location_id, "^USGS-"),
      date = as.Date(runtime),
      percentile_cond = factor(category_map[category], levels = cond_levels),
      percentile_bin = factor(bin_map[category_map[category]], levels = bin_levels)
    ) |>
    filter(!is.na(percentile_cond)) |>
    select(site_no, date, percentile_cond, percentile_bin)
}

#' @description Build site_no -> state_cd lookup from parquet data (replaces readNWISsite)
#' @param dv_new Combined parquet data with monitoring_location_id and state_name
build_dv_site <- function(dv_new) {
  state_fips_lookup <- maps::state.fips |>
    mutate(state_name = str_to_title(str_extract(polyname, "^[^:]+"))) |>
    distinct(fips, state_name) |>
    mutate(state_cd = str_pad(fips, 2, "left", "0")) |>
    select(state_name, state_cd) |>
    bind_rows(tibble(
      state_name = c("Alaska", "Hawaii", "Puerto Rico"),
      state_cd = c("02", "15", "72")
    ))

  dv_new |>
    distinct(monitoring_location_id, state_name) |>
    mutate(site_no = str_remove(monitoring_location_id, "^USGS-")) |>
    left_join(state_fips_lookup, by = "state_name") |>
    select(site_no, state_cd)
}

#' @description Bin percentile data (`percentile_bin`) into flow condition (`percentile_cond`) categories
#' @param data_in 1 month of streamflow percentiles generated from `gage-conditions-gif` pipeline
#' @param date_start first day of focal month
#' @param date_end last day of focal month
#' @param breaks Percentile values to bin data at
add_flow_condition <- function(data_in, date_start, date_end, breaks, break_labels = c("Driest", "Drier", "Dry", "Normal","Wet","Wetter", "Wettest")){
  data_in %>% 
    mutate(date = as.Date(dateTime)) %>%
    filter(date >= date_start, date <= date_end, !is.na(per)) %>%
    mutate(percentile_bin = cut(per, breaks = breaks, include.lowest = TRUE),
           percentile_cond = factor(percentile_bin, labels = break_labels))
  
}

#' @description Count the total number of observed sites per state each day
#' @param data_in Binned percentile data
#' @param dv_site Site data with state localities
site_count_state <- function(data_in, dv_site){
    data_in %>%
      left_join(dv_site)%>%
      group_by(state_cd, date) %>%
      summarize(total_gage = length(unique(site_no)))  %>% 
      mutate(fips = as.numeric(state_cd))

}

#' @description Count total number of active sites nationally each day
#' @param data_in Binned percentile data
site_count_national <- function(data_in){
  data_in %>%
    group_by(date) %>%
    summarize(total_gage = sum(total_gage))
  
}

#' @description Calculate proportion of sites in each percentile bin through time
#' @param data_in Binned percentile data
#' @param sites_national Total number of active sites each day
flow_by_day <- function(data_in, sites_national) {
  data_in %>%
    group_by(date, percentile_cond, percentile_bin) %>%
    summarize(n_gage = length(unique(site_no))) %>%
    left_join(sites_national) %>%
    mutate(prop = n_gage/total_gage)
}

#' @description Calculate proportion of sites in each percentile bin through time
#' @param data_in Binned percentile data
#' @param sites_national Total number of active sites each day by state
flow_by_day_by_state <- function(data_in, dv_site, sites_state) {
  data_in  %>%
    left_join(dv_site) %>% # adds state info for each gage
    group_by(state_cd, date, percentile_cond) %>% # aggregate by state, day, flow condition
    summarize(n_gage = length(unique(site_no)))  %>%
    left_join(sites_state) %>% # add total_gage
    mutate(prop = n_gage/total_gage) %>% # proportion of gages
    pivot_wider(id_cols = !n_gage, names_from = percentile_cond, values_from = prop, values_fill = 0) %>% # complete data for timepoints with 0 gages
    pivot_longer(cols = c("Normal", "Wet", "Wetter", "Wettest", "Driest", "Drier", "Dry"), 
                 names_to = "percentile_cond", values_to = "prop")

}

