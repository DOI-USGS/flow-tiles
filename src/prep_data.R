add_flow_condition <- function(data_in, date_start, date_end){
  data_in %>% 
    mutate(date = as.Date(dateTime),
           cond = 
             case_when(
               per < 0.05 ~ 'Driest',
               per >= 0.05 & per < 0.1 ~ "Drier",
               per >= 0.1 & per < 0.25 ~ "Dry", 
               per >= 0.25 & per < 0.75 ~ "Normal",
               per >= 0.75 & per < 0.9 ~ "Wet", 
               per >= 0.9 & per < 0.95 ~ "Wetter", 
               per >= 0.95 ~ "Wettest"
             ),
           cond = factor(cond, ordered = TRUE, levels = c("Driest", "Drier", "Dry", "Normal","Wet","Wetter", "Wettest"))) %>%
    filter(date >= date_start, date <= date_end, !is.na(cond)) 
}

site_count_state <- function(data_in, dv_site){
    data_in %>%
      left_join(dv_site)%>%
      group_by(state_cd, date) %>%
      summarize(total_gage = length(unique(site_no)))  %>% 
      mutate(fips = as.numeric(state_cd))

}

site_count_national <- function(data_in){
  data_in %>%
    group_by(date) %>%
    summarize(total_gage = sum(total_gage))
  
}

flow_by_day <- function(data_in, sites_national) {
  data_in %>%
    group_by(date, cond) %>%
    summarize(n_gage = length(unique(site_no))) %>%
    left_join(sites_national) %>%
    mutate(prop = n_gage/total_gage)
}

flow_by_day_by_state <- function(data_in, dv_site, sites_state) {
  data_in  %>%
    left_join(dv_site) %>% # adds state info for each gage
    group_by(state_cd, date, cond) %>% # aggregate by state, day, flow condition
    summarize(n_gage = length(unique(site_no)))  %>%
    left_join(sites_state) %>% # add total_gage
    mutate(prop = n_gage/total_gage) %>% # proportion of gages
    pivot_wider(id_cols = !n_gage, names_from = cond, values_from = prop, values_fill = 0) %>% # complete data for timepoints with 0 gages
    pivot_longer(cols = c("Normal", "Wet", "Wetter", "Wettest", "Driest", "Drier", "Dry"), 
                 names_to = "cond", values_to = "prop")

}

