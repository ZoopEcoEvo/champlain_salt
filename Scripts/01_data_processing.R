# Identifies all survival data for experiments 
surv_files = dir(path = "Raw_data/2024_data/surv_data/")[str_detect(dir(path = "Raw_data/2024_data/surv_data/"), pattern = "2024")]
sic_surv = surv_files[str_detect(surv_files, "2024_01")]
oreg_surv = surv_files[str_detect(surv_files, "2024_08")]

# Identifies all files for the CTmax experiments 
ctmax_files = dir(path = "Raw_data/2024_data/ctmax_data/")[!str_detect(dir(path = "Raw_data/2024_data/ctmax_data/"), pattern = "trial")]
ctmax_times = ctmax_files[str_detect(ctmax_files, pattern = "_obs")]
ctmax_temps = ctmax_files[str_detect(ctmax_files, pattern = "_temp")]

#### Processing survival files ####
surv_data = data.frame()
daily_prop_data = data.frame()

for(file in sic_surv){
  
  # Extracts experiment date from file name
  exp_date = str_split(string = str_replace_all(file, pattern = "_", replacement = "-"), pattern = "-surv")[[1]][1]
  
  raw_data = read.csv(file = paste("Raw_data/2024_data/surv_data/", file, sep = "")) %>% 
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
  
  time_data = read.csv(file = paste("Raw_data/2024_data/ctmax_data/", file, sep = ""))
  temp_data = read.csv(file = paste("Raw_data/2024_data/ctmax_data/", meta_info[1], "temp.csv", sep = ""))
  
  exp_data = est_ctmax(temp_data = temp_data, time_data = time_data)
  
  ctmax_data = bind_rows(ctmax_data, exp_data)
}

# Write the data to files in the Output directory
write.csv(ctmax_data, file = "Output/Output_data/ctmax_data.csv", row.names = F)

#### Processing the 2025/2026 CTmax data ####

# Processing the 2025 acclimation experiment data
surv_files = dir(path = "Raw_data/2025_data/surv_data/")
ctmax_files = dir(path = "Raw_data/2025_data/ctmax_data/")

ctmax_acclim_data = data.frame()
for(ctmax in ctmax_files){
  
  ctmax_run = read.csv(file = paste("Raw_data/2025_data/ctmax_data/", ctmax, sep = ""))
  
  ctmax_acclim_data = bind_rows(ctmax_acclim_data, ctmax_run)
}

acclim_data = ctmax_acclim_data %>%  
  janitor::clean_names() %>%  
  drop_na(start_temperature) %>%  
  select(collection_date, 'collection_temp' = lake_temperature_degrees_c, "lab_temp" = lab_temperature, 
         start_temperature, assay_date, species, exp_rep, treatment, tube, dose_assignment, "cl_conc" = cl_concentrations, "ctmax" = ct_max_temp) %>% 
  mutate(collection_date = lubridate::as_date(collection_date, format = "%m/%d/%y"),
         assay_date = lubridate::as_date(assay_date, format = "%m/%d/%y"), 
         acc_time = as.integer(assay_date - collection_date), 
         cl_conc = cl_conc / 1000) %>% 
  filter(ctmax > 15) # Filtering out abnormally low CTmax value

write.csv(acclim_data, file = "Output/Output_data/acclim_data.csv", row.names = F)

#### Processing the 2025/2026 Survival data ####


surv_2025_data = data.frame()
for(surv in surv_files){
  
  surv_run = read.csv(file = paste("Raw_data/2025_data/surv_data/", surv, sep = "")) %>% 
    janitor::clean_names() %>% 
    select(treatment, species, replication, collection_date, "collection_temp" = lake_temperature_degrees_c, 
           "cl_conc" = cl_concentration_mg_l, ends_with("_hour")) %>% 
    mutate("initial" = x0_hour) %>% 
    pivot_longer(cols = ends_with("_hour"), 
                 names_to = "time", 
                 names_prefix = "x",
                 values_to = "surviving") %>% 
    mutate(time = parse_number(time)) %>% 
    drop_na(replication)
  
  
  
  surv_2025_data = bind_rows(surv_2025_data, surv_run)
}


## Survival Curves Figure - Save as 7x8" landscape
surv_2025_data %>% 
  mutate(prop_surv = surviving/initial, 
         day = factor(floor(time / 24))) %>% 
  filter(collection_date != "9/22/25") %>% 
  write.csv(file = "Output/Output_data/surv_2025.csv", row.names = F)


