
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

