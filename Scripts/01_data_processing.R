# Identifies all survival data for experiments 
surv_files = dir(path = "Raw_data/surv_data/")[str_detect(dir(path = "Raw_data/surv_data/"), pattern = "2024")]

# Identifies all files for the CTmax experiments 
ctmax_files = dir(path = "Raw_data/ctmax_data/")[!str_detect(dir(path = "Raw_data/ctmax_data/"), pattern = "trial")]
ctmax_times = ctmax_files[str_detect(ctmax_files, pattern = "_obs")]
ctmax_temps = ctmax_files[str_detect(ctmax_files, pattern = "_temp")]

#### Processing survival files ####
surv_data = data.frame()
daily_prop_data = data.frame()

for(file in surv_files){
  
  # Extracts experiment date from file name
  exp_date = str_split(string = str_replace_all(file, pattern = "_", replacement = "-"), pattern = "-surv")[[1]][1]
  
  raw_data = read.csv(file = paste("Raw_data/surv_data/", file, sep = "")) %>% 
    discard(~all(is.na(.x)))
  
  # Uses custom functions to process survival data into usable formats
  data = raw_data %>% 
    expand_surv() %>% 
    make_surv() %>% 
    mutate("exp_date" = lubridate::as_date(exp_date))
  
  # Adds experimental data to collective data frame
  surv_data = bind_rows(surv_data, data)
  
  ### Processes data to provide daily proportional survival data
  
  prop_data = data.frame()
  for(i in 4:dim(raw_data)[2]){
    
    col_name = colnames(raw_data)[i]
    hour = as.numeric(str_split_fixed(col_name, pattern = "_", n = 2)[2])
    
    day_data = raw_data %>%  
      select(salt:{{col_name}}) %>% 
      expand_surv() %>% 
      make_surv() %>% 
      ungroup() %>% 
      group_by(salt, treatment, replicate, initial) %>% 
      summarise(num_died = sum(ind_surv), .groups = "keep") %>%  
      mutate(total_surv = initial - num_died,
             prop_surv = total_surv / initial,
             hour = hour,
             exp_date = lubridate::as_date(exp_date))
    
    prop_data = bind_rows(prop_data, day_data)
  }
  
  daily_prop_data = bind_rows(daily_prop_data, prop_data)
  
}

# Write the data to files in the Output directory
write.csv(surv_data, file = "Output/Output_data/surv_data.csv", row.names = F)
write.csv(daily_prop_data, file = "Output/Output_data/daily_prop_data.csv", row.names = F)

#### Processing the CTmax data ####

ctmax_data = data.frame()
for(file in ctmax_times){
  
  meta_info = str_split_fixed(file, pattern = "obs", n = 2)

  time_data = read.csv(file = paste("Raw_data/ctmax_data/", file, sep = ""))
  temp_data = read.csv(file = paste("Raw_data/ctmax_data/", meta_info[1], "temp.csv", sep = ""))
  
  exp_data = est_ctmax(temp_data = temp_data, time_data = time_data)
  
  ctmax_data = bind_rows(ctmax_data, exp_data)
}

# Write the data to files in the Output directory
write.csv(ctmax_data, file = "Output/Output_data/ctmax_data.csv", row.names = F)


