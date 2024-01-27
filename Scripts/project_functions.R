
est_ctmax = function(temp_data, time_data) {
  
  # Loads data from temperature sensors (logging at 5 second intervals)
  temp_converted = temp_data %>% 
    select(-Date) %>% 
    mutate("Time" = lubridate::hms(Time)) %>% 
    mutate("time_point" = row_number(), # Assigns each time point a sequential value
           "second_passed" = lubridate::time_length(Time - first(Time)), # Calculates the time passed in seconds since logging began
           "minute_passed" = second_passed / 60,
           "minute_interval" = floor(second_passed / 60)) %>% # Integer math to convert from seconds since logging began to minute time interval 
    pivot_longer(cols = c(Temp1, Temp2, Temp3), # Pivots data set so there's only one column of temperature data
                 names_to = "sensor",
                 values_to = "temp_C") %>% ungroup()
  
  time_converted = time_data %>% 
    drop_na(ctmax_minute) %>%
    mutate(time = (ctmax_minute + (ctmax_second / 60)) - 2, # Accounts for the two minute start up delay in the temperature logger
           "rank" = dense_rank(desc(time)))
  
  ### Combine with time data to get CTmax values 
  ind_measurements = time_converted %>% 
    group_by(tube) %>% 
    summarise("ctmax" = mean(filter(
      temp_converted, minute_passed > (time - (0.1 * rank)) & 
        minute_passed < time)$temp_C))
  
  ct_data = inner_join(time_converted, ind_measurements, by = c("tube")) %>% 
    select(experiment_date, experiment, lab_temp, salt, replicate, treatment, tube, rank, time, ctmax)
  
  return(ct_data)
}


expand_surv = function(prelim_surv) {
  
  expanded_data = prelim_surv %>% 
    mutate(treatment = as.numeric(treatment)) %>% 
    pivot_longer(cols = starts_with("hour_"),
                 values_to = "surv", 
                 names_to = "hour",
                 names_prefix = "hour_") %>% ### Gets data into long format
    group_by(treatment, replicate) %>% 
    mutate(initial = first(surv)) %>% 
    group_by(treatment, replicate, hour) %>% 
    drop_na(surv) %>% 
    mutate(hour = as.numeric(hour),
           "ind_surv" = paste(rep(c(0,1), c(surv, initial - surv)), collapse = ",")) %>% ### Assembles vector of mortality events for each time point (0 = survived, 1 = mortality)
    select(-surv) %>% 
    separate_rows(ind_surv, sep = ",", convert = T) %>% ### Separates data so it's one event per ro
    mutate("ID" = row_number()) %>%  
    ungroup() %>% 
    group_by(treatment, replicate, ID) %>% 
    select(salt, treatment, replicate, ID, initial, hour, ind_surv) 
  
  return(expanded_data)
}

make_surv = function(expanded_data){
  
  mort_1 = expanded_data %>% ungroup() %>%  
    group_by(treatment, ID) %>% 
    filter(ind_surv == 1) ### Pulls out only the mortality events 
  
  if(dim(mort_1)[1] > 0){
    mort_1 = mort_1 %>% 
      filter(hour == min(hour)) ### Isolates when mortality was observed
  }  
  
  mort_2 = expanded_data %>% ungroup() %>%  
    group_by(treatment, ID) %>% 
    filter(hour == max(hour)) %>% ### Pulls out just the final observations
    filter(ind_surv == 0) ### Filters to just survivors
  
  surv_data = bind_rows(mort_1, mort_2) # Combines mortality and survival events 
  
  return(surv_data)
}
