
capitalize_first <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

### Processing Data from Three Sources
# Winter 2024 (L. sicilis survival and CTmax; Road Salt and Instant Ocean)
# Summer 2024 (Skistodiaptomus survival; Road Salt)
# Fall 2025/Winter 2026 (L. sicilis & L. minutus survival and CTmax; Ion-specific)

#### Processing the Survival data ####
#### Processing the 2024/2025 Survival data ####

# Identifies all survival data for experiments 
surv_files = dir(path = "Raw_data/2024_data/surv_data/")[str_detect(dir(path = "Raw_data/2024_data/surv_data/"), pattern = "2024")]
sic_surv = surv_files[str_detect(surv_files, "2024-01")]
oreg_surv = surv_files[str_detect(surv_files, "2024-08")]

surv_data = data.frame()
daily_prop_data = data.frame()

for(file in surv_files){
  
  # Extracts experiment date from file name
  exp_date = str_split(string = str_replace_all(file, pattern = "_", replacement = "-"), pattern = "-surv")[[1]][1]
  
  raw_data = read.csv(file = paste("Raw_data/2024_data/surv_data/", file, sep = "")) %>% 
    discard(~all(is.na(.x)))
  
  if(file %in% sic_surv){
    raw_data$species = "L. sicilis"
    
    raw_data$sex = "female"
  }else{
    raw_data$species = "Skistodiaptomus"}
  
  raw_data = raw_data %>% 
    select(species, sex, salt, treatment, replicate, starts_with("hour_"))
  
  # Uses custom functions to process survival data into usable formats
  data = raw_data %>% 
    expand_surv() %>% 
    make_surv() %>% 
    arrange(treatment) %>% 
    mutate("exp_date" = lubridate::as_date(exp_date))
  
  # Adds experimental data to collective data frame
  surv_data = bind_rows(surv_data, data)
  
  ### Processes data to provide daily proportional survival data
  
  prop_data = data.frame()
  for(i in 6:dim(raw_data)[2]){
    
    col_name = colnames(raw_data)[i]
    hour = as.numeric(str_split_fixed(col_name, pattern = "_", n = 2)[2])
    
    day_data = raw_data %>%  
      select(species, sex, salt:{{col_name}}) %>% 
      expand_surv() %>% 
      make_surv() %>% 
      ungroup() %>% 
      group_by(species, sex, salt, treatment, replicate, initial) %>% 
      summarise(num_died = sum(ind_surv), .groups = "keep") %>%  
      mutate(total_surv = initial - num_died,
             prop_surv = total_surv / initial,
             hour = hour,
             exp_date = lubridate::as_date(exp_date))
    
    prop_data = bind_rows(prop_data, day_data)
  }
  
  daily_prop_data = bind_rows(daily_prop_data, prop_data)
  
}


#### Processing the 2025/2026 Survival data ####

# Processing the 2025 acclimation experiment data
surv_files_2025 = dir(path = "Raw_data/2025_data/surv_data/")

surv_2025_data = data.frame()
daily_prop_data_2025 = data.frame()

for(file in surv_files_2025){
  
  # Extracts experiment date from file name
  exp_date = str_split(string = str_replace_all(file, pattern = "_", replacement = "-"), pattern = "-survival")[[1]][1]
  
  raw_data = read.csv(file = paste("Raw_data/2025_data/surv_data/", file, sep = "")) %>% 
    discard(~all(is.na(.x))) |> 
    janitor::clean_names() |> 
    mutate(sex = "female", 
           replicate = row_number(), 
           exp_date = exp_date) %>% 
    select(species, sex, replicate, exp_date = collection_date, "salt" = treatment, "treatment" = cl_concentration_mg_l, 
           starts_with("x")) 
  
  # Uses custom functions to process survival data into usable formats
  data = raw_data %>% 
    expand_surv() %>% 
    make_surv() %>% 
    arrange(treatment) %>% 
    mutate("exp_date" = lubridate::as_date(exp_date))
  
  # Adds experimental data to collective data frame
  surv_2025_data = bind_rows(surv_2025_data, data)
  
  ### Processes data to provide daily proportional survival data
  
  prop_data = data.frame()
  for(i in 7:dim(raw_data)[2]){
    
    col_name = colnames(raw_data)[i]
    hour = parse_number(col_name)
    
    day_data = raw_data %>%  
      select(species, sex, replicate, salt:{{col_name}}) %>% 
      expand_surv() %>% 
      make_surv() %>% 
      ungroup() %>% 
      group_by(species, sex, salt, treatment, replicate, initial) %>% 
      summarise(num_died = sum(ind_surv), .groups = "keep") %>%  
      mutate(total_surv = initial - num_died,
             prop_surv = total_surv / initial,
             hour = hour)
    
    prop_data = bind_rows(prop_data, day_data)
  }
  
  daily_prop_data_2025 = bind_rows(daily_prop_data_2025, prop_data)
  
}


all_surv_data = surv_data |> 
  mutate(replicate = as.character(replicate)) |> 
  bind_rows(surv_2025_data |> mutate(replicate = as.character(replicate))) |> 
  filter(salt != "InstantOcean") |> 
  mutate(species = capitalize_first(species))

all_prop = daily_prop_data |> 
  mutate(replicate = as.integer(as.factor(replicate))) |> 
  bind_rows(daily_prop_data_2025) |> 
  filter(salt != "InstantOcean") |> 
  mutate(species = capitalize_first(species),
         day = floor(hour/24), 
         est_chloride = if_else(salt == "RoadSalt", treatment * 0.7, treatment))

all_prop |> 
  ggplot(aes(x = est_chloride, y = prop_surv, colour = factor(day))) + 
  facet_grid(salt~species, scales = "free") + 
  geom_hline(yintercept = 0.5) + 
  geom_point() + 
  geom_smooth(method = "glm", 
              se = F, 
              method.args = list(family = "binomial")) + 
  theme_bw() + 
  theme(panel.grid = element_blank())


# Write the data to files in the Output directory
write.csv(surv_data, file = here::here("output", "output_data", "surv_data.csv"), row.names = F)

write.csv(daily_prop_data, file = here::here("output", "output_data", "daily_prop_data.csv"), row.names = F)

write.csv(all_prop, file = here::here("output", "output_data", "all_prop.csv"), row.names = F)



#### Processing the CTmax data ####
#### Processing the 2024/2025 CTmax data ####
# Identifies all files for the CTmax experiments 
ctmax_files = dir(path = "Raw_data/2024_data/ctmax_data/")[!str_detect(dir(path = "Raw_data/2024_data/ctmax_data/"), pattern = "trial")]
ctmax_times = ctmax_files[str_detect(ctmax_files, pattern = "_obs")]
ctmax_temps = ctmax_files[str_detect(ctmax_files, pattern = "_temp")]

ctmax_data = data.frame()
for(file in ctmax_times){
  
  # Matching temp-logger file shares the same date/rep, swapping "obs" for "temp"
  temp_file = str_replace(file, pattern = "_obs", replacement = "_temp")
  stopifnot("No matching temperature file found" = temp_file %in% ctmax_temps)
  
  time_data = read.csv(file = paste("Raw_data/2024_data/ctmax_data/", file, sep = ""))
  temp_data = read.csv(file = paste("Raw_data/2024_data/ctmax_data/", temp_file, sep = ""))
  
  exp_data = est_ctmax(temp_data = temp_data, time_data = time_data) 
  
  ctmax_data = bind_rows(ctmax_data, exp_data) %>% 
    mutate(species = "L. sicilis", 
           sex = "female")
}

ctmax_data_2024 = ctmax_data |> 
  select("collection_temp" = lab_temp, "assay_date" = experiment_date,
         species, sex, "exp_rep" = experiment, "treatment" = salt, tube, 
         "dose_assignment" = treatment, ctmax) |> 
  mutate(acc_time = 6, 
         lab_temp = collection_temp,
         sex = "female",
         tube = as.numeric(tube),
         assay_date = lubridate::as_date(assay_date, format = "%m/%d/%y"))


#### Processing the 2025/2026 CTmax data ####

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
         cl_conc = cl_conc / 1000, 
         tube = parse_number(tube)) %>% 
  filter(ctmax > 15) # Filtering out abnormally low CTmax value

write.csv(acclim_data, file = here::here("output", "output_data", "acclim_data.csv"), row.names = F)

all_ctmax_data = bind_rows(acclim_data, ctmax_data_2024) |> 
  filter(treatment != "InstantOcean") |> 
  mutate(cl_conc = if_else(dose_assignment == "control", 0, cl_conc), 
         species = capitalize_first(species),
         dose_assignment = if_else(cl_conc == 0, "Control", "Salt"),
         dose_assignment = if_else(is.na(cl_conc), "Salt", dose_assignment))

all_ctmax_data |> 
  ggplot(aes(x = dose_assignment, y = ctmax)) + 
  facet_grid(species~treatment) + 
  geom_boxplot()


# Write the data to files in the Output directory
write.csv(all_ctmax_data, file = here::here("output", "output_data", "ctmax_data.csv"), row.names = F)
