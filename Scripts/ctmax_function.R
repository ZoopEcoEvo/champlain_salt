
est_ctmax <- function(temp_data, time_data) {
  
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
    select(experiment_date, lab_temp, replicate, treatment, tube, rank, time, ctmax)
  
  return(ct_data)
}
