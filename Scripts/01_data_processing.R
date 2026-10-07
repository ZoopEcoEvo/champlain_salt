# =============================================================================
# 01_data_processing.R
#
# Combines raw survival and CTmax files (2024 + 2025/26 cohorts) into tidy,
# individual-level datasets and writes them to Output/Output_data/.
# Requires Scripts/project_functions.R to already be sourced (supplies
# expand_surv(), make_surv(), est_ctmax()) -- see Scripts/02_controller.R.
#
# Produces:
#   - survival_data   -> Output/Output_data/surv_data.csv
#       One row per individual per experiment, with the hour it died
#       (ind_surv == 1) or was last observed alive (ind_surv == 0).
#   - daily_prop_data -> Output/Output_data/daily_prop_data.csv
#       Observed proportion surviving at each check, for descriptive plots.
#   - ctmax_data      -> Output/Output_data/ctmax_data.csv
#       One row per individual CTmax measurement.
#
# Model fitting and plotting live in Output/Reports/report.Rmd, not here
# (see CHECKLIST.md Phase 2).
# =============================================================================

### Processing Data from Three Sources
# Winter 2024 (L. sicilis survival and CTmax; Road Salt and Instant Ocean)
# Summer 2024 (Skistodiaptomus survival; Road Salt)
# Fall 2025/Winter 2026 (L. sicilis & L. minutus survival and CTmax; Ion-specific)


# =============================================================================
# 1. SURVIVAL DATA: combine raw files into one tidy, individual-level table
# =============================================================================

## ---- 1a. 2024 cohort (L. sicilis winter trials; Skistodiaptomus summer trial) ----
# Files: Raw_data/2024_data/surv_data/2024-*.csv
# Columns: salt, treatment (dose), [sex], replicate, hour_* (count of
# survivors remaining at each check)

surv_files_2024 <- dir("Raw_data/2024_data/surv_data") |>
  keep(~ str_detect(.x, "^2024"))                # drops the "trial_*" pilot files
sic_files <- surv_files_2024[str_detect(surv_files_2024, "2024-01")]

surv_2024 <- map_dfr(surv_files_2024, function(file) {
  exp_date <- str_extract(file, "^\\d{4}-\\d{2}-\\d{2}")

  raw_data <- read.csv(file.path("Raw_data/2024_data/surv_data", file)) |>
    discard(~ all(is.na(.x)))

  if (file %in% sic_files) {
    raw_data$species <- "L. sicilis"
    raw_data$sex <- "female"
  } else {
    raw_data$species <- "Skistodiaptomus"   # sex already present in this file
  }

  raw_data |>
    select(species, sex, salt, treatment, replicate, starts_with("hour_")) |>
    expand_surv() |>
    make_surv() |>
    mutate(exp_date = as_date(exp_date))
})

## ---- 1b. 2025/2026 cohort (L. sicilis & L. minutus, ion-specific trials) ----
# Files: Raw_data/2025_data/surv_data/*.csv
# Columns differ from 2024: wide, descriptive headers; dose stored as
# chloride concentration (mg/L); hour columns are "0 Hour", "26 Hour", etc.

surv_files_2025 <- dir("Raw_data/2025_data/surv_data")

surv_2025 <- map_dfr(surv_files_2025, function(file) {
  exp_date <- str_extract(file, "^\\d{4}-\\d{2}-\\d{2}")

  raw_data <- read.csv(file.path("Raw_data/2025_data/surv_data", file)) |>
    # Several 2025 files have fully-blank trailing rows (all commas, no
    # values). Empty strings in character columns read as "" rather than NA,
    # and `discard()` only drops all-NA *columns*, not rows -- so blank rows
    # would otherwise survive as phantom records with blank species/salt.
    # Convert "" -> NA, then explicitly drop any row that's NA everywhere.
    mutate(across(where(is.character), ~ na_if(.x, ""))) |>
    filter(if_any(everything(), ~ !is.na(.x))) |>
    discard(~ all(is.na(.x))) |>
    janitor::clean_names() |>
    mutate(sex = "female", replicate = row_number()) |>
    select(species, sex, replicate, "salt" = treatment,
           "treatment" = cl_concentration_mg_l, starts_with("x"))

  raw_data |>
    expand_surv() |>
    make_surv() |>
    mutate(exp_date = as_date(exp_date))
})

## ---- 1c. Combine cohorts, harmonize labels, drop pilot arm ----------------
# "Instant Ocean" was only run alongside the 2024 L. sicilis trials as a
# check against the main RoadSalt dose-response design; excluded here as in
# the prior pipeline.

survival_data <- bind_rows(
    surv_2024 |> mutate(replicate = as.character(replicate)),
    surv_2025 |> mutate(replicate = as.character(replicate))
  ) |>
  mutate(
    species = case_when(
      str_to_lower(species) %in% c("l. sicilis", "l.sicilis") ~ "L. sicilis",
      str_to_lower(species) %in% c("l. minutus", "l.minutus") ~ "L. minutus",
      TRUE ~ "Skistodiaptomus"
    ),
    # Harmonize dose onto a common chloride-equivalent scale so RoadSalt/
    # NaCl/MgCl2 doses are comparable; 0.7 factor per prior analysis --
    # document its source (see CHECKLIST.md Phase 1 data dictionary item).
    est_chloride = if_else(salt == "RoadSalt", treatment * 0.7, treatment)
  ) |>
  filter(salt != "InstantOcean") |>
  select(species, sex, salt, treatment, est_chloride, replicate, exp_date,
         ID, initial, hour, ind_surv) |>
  arrange(species, salt, exp_date, treatment, replicate)


# =============================================================================
# 2. DAILY PROPORTIONAL SURVIVAL: observed proportion surviving at each check
# =============================================================================
# Kept separate from `survival_data` because it's a repeated-measures count
# (survivors remaining at each check), not an individual event/censoring
# time -- used for descriptive plots against model predictions.

## ---- 2a. 2024 cohort -------------------------------------------------------
daily_prop_2024 <- map_dfr(surv_files_2024, function(file) {
  exp_date <- str_extract(file, "^\\d{4}-\\d{2}-\\d{2}")

  raw_data <- read.csv(file.path("Raw_data/2024_data/surv_data", file)) |>
    discard(~ all(is.na(.x)))

  if (file %in% sic_files) {
    raw_data$species <- "L. sicilis"
    raw_data$sex <- "female"
  } else {
    raw_data$species <- "Skistodiaptomus"
  }

  raw_data |>
    select(species, sex, salt, treatment, replicate, starts_with("hour_")) |>
    pivot_longer(starts_with("hour_"), names_to = "hour", values_to = "n_surviving",
                 names_prefix = "hour_") |>
    mutate(hour = as.numeric(hour)) |>
    group_by(species, sex, salt, treatment, replicate) |>
    mutate(initial = first(n_surviving)) |>
    ungroup() |>
    mutate(exp_date = as_date(exp_date))
})

## ---- 2b. 2025/2026 cohort ---------------------------------------------------
daily_prop_2025 <- map_dfr(surv_files_2025, function(file) {
  exp_date <- str_extract(file, "^\\d{4}-\\d{2}-\\d{2}")

  raw_data <- read.csv(file.path("Raw_data/2025_data/surv_data", file)) |>
    # Same blank-trailing-row issue as section 1b -- see note there.
    mutate(across(where(is.character), ~ na_if(.x, ""))) |>
    filter(if_any(everything(), ~ !is.na(.x))) |>
    discard(~ all(is.na(.x))) |>
    janitor::clean_names() |>
    mutate(sex = "female", replicate = row_number()) |>
    select(species, sex, replicate, "salt" = treatment,
           "treatment" = cl_concentration_mg_l, starts_with("x"))

  raw_data |>
    pivot_longer(starts_with("x"), names_to = "hour", values_to = "n_surviving") |>
    mutate(hour = parse_number(hour)) |>
    group_by(species, sex, salt, treatment, replicate) |>
    mutate(initial = first(n_surviving)) |>
    ungroup() |>
    mutate(exp_date = as_date(exp_date))
})

## ---- 2c. Combine, harmonize, derive proportion + day -----------------------
daily_prop_data <- bind_rows(
    daily_prop_2024 |> mutate(replicate = as.character(replicate)),
    daily_prop_2025 |> mutate(replicate = as.character(replicate))
  ) |>
  mutate(
    species = case_when(
      str_to_lower(species) %in% c("l. sicilis", "l.sicilis") ~ "L. sicilis",
      str_to_lower(species) %in% c("l. minutus", "l.minutus") ~ "L. minutus",
      TRUE ~ "Skistodiaptomus"
    ),
    est_chloride = if_else(salt == "RoadSalt", treatment * 0.7, treatment),
    prop_surv = n_surviving / initial,
    day = round(hour / 24)
  ) |>
  filter(salt != "InstantOcean")


# =============================================================================
# 3. CTMAX DATA: combine raw files into one tidy, individual-level table
# =============================================================================

## ---- 3a. 2024 cohort (paired observation/temperature-logger files) --------
ctmax_files_2024 <- dir("Raw_data/2024_data/ctmax_data") |>
  discard(~ str_detect(.x, "trial"))
obs_files_2024  <- ctmax_files_2024[str_detect(ctmax_files_2024, "_obs")]
temp_files_2024 <- ctmax_files_2024[str_detect(ctmax_files_2024, "_temp")]

ctmax_2024 <- map_dfr(obs_files_2024, function(file) {
  temp_file <- str_replace(file, "_obs", "_temp")
  stopifnot("No matching temperature file found" = temp_file %in% temp_files_2024)

  est_ctmax(
    temp_data = read.csv(file.path("Raw_data/2024_data/ctmax_data", temp_file)),
    time_data = read.csv(file.path("Raw_data/2024_data/ctmax_data", file))
  )
}) |>
  mutate(species = "L. sicilis", sex = "female") |>
  select("collection_temp" = lab_temp, "assay_date" = experiment_date, species, sex,
         "exp_rep" = experiment, salt, tube, "dose_assignment" = treatment, ctmax) |>
  mutate(
    acc_time = 6,   # fixed 6 h lab acclimation for this cohort -- document source (CHECKLIST Phase 1)
    assay_date = as_date(assay_date, format = "%m/%d/%y")
  )

## ---- 3b. 2025/2026 cohort (one row per animal already) --------------------
ctmax_files_2025 <- dir("Raw_data/2025_data/ctmax_data")

ctmax_2025 <- map_dfr(ctmax_files_2025, ~ read.csv(file.path("Raw_data/2025_data/ctmax_data", .x))) |>
  janitor::clean_names() |>
  drop_na(start_temperature) |>
  select(collection_date, "collection_temp" = lake_temperature_degrees_c,
         "lab_temp" = lab_temperature, start_temperature, assay_date, species, exp_rep,
         salt = treatment, tube, dose_assignment, "cl_conc" = cl_concentrations,
         ctmax = ct_max_temp) |>
  mutate(
    collection_date = as_date(collection_date, format = "%m/%d/%y"),
    assay_date = as_date(assay_date, format = "%m/%d/%y"),
    acc_time = as.integer(assay_date - collection_date),
    cl_conc = cl_conc / 1000,
    tube = parse_number(tube)
  )

## ---- 3c. Combine cohorts, harmonize labels ---------------------------------
ctmax_data <- bind_rows(ctmax_2024, ctmax_2025) |>
  mutate(
    species = case_when(
      str_to_lower(species) %in% c("l. sicilis", "l.sicilis") ~ "L. sicilis",
      str_to_lower(species) %in% c("l. minutus", "l.minutus") ~ "L. minutus",
      TRUE ~ species
    ),
    dose_group = case_when(
      salt == "RoadSalt" & dose_assignment == "control" ~ "Control",
      salt == "RoadSalt" & dose_assignment == "salt" ~ "Salt",
      salt != "RoadSalt" & cl_conc == 0 ~ "Control",
      salt != "RoadSalt" & cl_conc > 0 ~ "Salt"
    )
  ) |>
  filter(salt != "InstantOcean", ctmax > 15) |>  # drop pilot arm + one implausible low reading (see CHECKLIST)
  arrange(species, salt, assay_date, tube)


# =============================================================================
# 4. WRITE TIDY DATASETS
# =============================================================================
write.csv(survival_data, file = "Output/Output_data/surv_data.csv", row.names = FALSE)
write.csv(daily_prop_data, file = "Output/Output_data/daily_prop_data.csv", row.names = FALSE)
write.csv(ctmax_data, file = "Output/Output_data/ctmax_data.csv", row.names = FALSE)
