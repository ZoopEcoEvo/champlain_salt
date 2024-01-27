# Load in required packages
library(rmarkdown)
library(survival)
library(survminer)
library(tidyverse)
library(lubridate)
library(slider)
library(dataRetrieval)
library(ggpubr)
library(ggsurvfit)
library(dabestr)
library(lme4)

source("Scripts/project_functions.R")

#Determine which scripts should be run
prelim_report = F #Makes a preliminary report based on environmental data from Lake Champlain
process_data = T #Runs data analysis 
make_report = T #Runs project summary
knit_manuscript = F #Compiles manuscript draft

if(prelim_report == T){
  
  prelim_surv = read.csv(file = "Raw_data/surv_data/trial_surv_1.csv")
  prelim_surv2 = read.csv(file = "Raw_data/surv_data/trial_surv_2.csv")
  
  ctmax_temp1 = read.csv(file = "Raw_data/ctmax_data/trial_2023_12_19_1_temp.CSV")
  ctmax_time1 = read.csv(file = "Raw_data/ctmax_data/trial_2023_12_19_1_obs.csv")
  ctmax_temp2 = read.csv(file = "Raw_data/ctmax_data/trial_2024_01_13_temp.CSV")
  ctmax_time2 = read.csv(file = "Raw_data/ctmax_data/trial_2024_01_13_1_obs.csv")
  
  render(input = "Output/Reports/prelim_data.Rmd", #Input the path to your .Rmd file here
         output_format = "all")
}

############################
### Read in the RAW data ###
############################

if(process_data == T){
  source(file = "Scripts/01_data_processing.R")
}

##################################
### Read in the PROCESSED data ###
##################################

if(make_report == T){
  surv_data = read.csv(file = "Output/Output_data/surv_data.csv") %>% 
    mutate(exp_day = ceiling(hour / 24))
  
  daily_prop_data = read.csv(file = "Output/Output_data/daily_prop_data.csv") %>% 
    mutate(exp_day = ceiling(hour / 24))
  
  ctmax_data = read.csv(file = "Output/Output_data/ctmax_data.csv")
  
  render(input = "Output/Reports/report.Rmd", #Input the path to your .Rmd file here
         #output_file = "report", #Name your file here if you want it to have a different name; leave off the .html, .md, etc. - it will add the correct one automatically
         output_format = "all")
}

##################################
### Read in the PROCESSED data ###
##################################

if(knit_manuscript == T){
  render(input = "Manuscript/manuscript_name.Rmd", #Input the path to your .Rmd file here
         output_file = paste("dev_draft_", Sys.Date(), sep = ""), #Name your file here; as it is, this line will create reports named with the date
                                                                  #NOTE: Any file with the dev_ prefix in the Drafts directory will be ignored. Remove "dev_" if you want to include draft files in the GitHub repo
         output_dir = "Output/Drafts/", #Set the path to the desired output directory here
         output_format = "all",
         clean = T)
}
