# Project Cleanup Checklist — Champlain Salt

Generated from a review of `Raw_data/`, `Scripts/01_data_processing.R`, `Scripts/02_controller.R`,
`Scripts/project_functions.R`, and `Output/Reports/report.Rmd`. Items are grouped by priority.
Line numbers refer to the code as of this writing and will shift as items are completed.

## Phase 0 — Reproducibility blockers (do first)

- [x] **Standardize raw data file names** across years/types to `YYYY-MM-DD_<type>[_repN].csv`.
      Done — see `Raw_data/rename_log.csv` for the full old → new mapping (97 files renamed via
      `git mv`, history preserved).
- [x] **Update `Scripts/01_data_processing.R` to match the new file names**:
  - [x] Lines 18–19: updated `"2024_01"`/`"2024_08"` patterns to `"2024-01"`/`"2024-08"`.
  - [x] Line 27: no change needed — happens to still parse correctly against the new names.
  - [x] Line 91 (now ~92): fixed `"-Survival"` → `"-survival"` (new files use lowercase).
  - [x] `est_ctmax()` file-pairing logic (was lines 175–178): rewritten to build the matching
        temp-logger filename by swapping `"_obs"` → `"_temp"` in place (preserving the `_repN`
        suffix), with a `stopifnot()` guard confirming the matched file actually exists.
- [x] **Update `Scripts/02_controller.R` hardcoded raw-data paths** (lines 42, 46) to the new
      file names.
- [x] **Run `01_data_processing.R` end-to-end after the above fixes** — confirmed it completes
      without error (`surv_data`: 645 rows, `surv_2025_data`: 6060 rows, `all_prop`: 1967 rows,
      `ctmax_data_2024`: 90 rows, `acclim_data`: 267 rows, `all_ctmax_data`: 337 rows).
- [ ] **New correctness bug found while testing** (unrelated to renaming): in the 2025/2026
      survival loop (`01_data_processing.R`, ~line 103), `surv_2025_data = bind_rows(surv_2025_data, data)`
      references a stale `data` object left over from the *first* (2024) survival loop — it should
      reference `raw_data` (the object actually built from the current file in this loop). As
      written, `surv_2025_data` is populated with repeated copies of the last 2024 survival record
      rather than the real 2025/2026 per-file data. This needs a fix before `surv_2025_data`/
      `daily_prop_data_2025` can be trusted.
- [ ] **Resolve the missing output files**: `02_controller.R` (lines 36, 39, 57, 65) reads
      `Output/Output_data/surv_data.csv`, `daily_prop_data.csv`, `acclim_data.csv`, and
      `surv_2025.csv`, but only `all_prop.csv` and `ctmax_data.csv` are currently written by
      `01_data_processing.R`. Either:
  - [ ] add `write.csv()` calls for `surv_data`, `daily_prop_data`, `daily_prop_data_2025`
        (combined), and the acclimation CTmax object (`acclim_data`) in `01_data_processing.R`, or
  - [ ] update `02_controller.R` to read the objects actually produced.
  - [ ] Determine where `surv_2025.csv` (currently present on disk but orphaned — not written by
        any script) should come from, or regenerate it from source and document the step.
- [ ] **Fix the `InstantOcean` filter bug** in `01_data_processing.R:135` — filters
      `treatment != "InstantOcean"` but `treatment` holds a numeric dose in that pipeline; should
      filter on `salt` (as the parallel line 141 correctly does). Currently masked because
      `all_surv_data` is never written to disk — remove the dead `write.csv` comment at line 159
      once resolved, or re-enable it correctly.

## Phase 1 — Data standardization (file renaming — mostly done)

- [x] Unify date format to `YYYY-MM-DD` across all raw file names.
- [x] Unify extension casing to lowercase `.csv` (2024 CTmax temperature logs were `.CSV`).
- [x] Fix the one-off `-Ramp` vs `_Ramp` separator typo (`03_22_26-Ramp.csv`).
- [x] Rename supplementary files (`Body Size Measurements.csv`, `Water_Sample_Full_Data.csv`,
      `column_data.pdf`) to snake_case with a year prefix.
- [ ] Decide whether to also rename the year folders (`2024_data`, `2025_data`) — note that
      `2025_data` actually spans Sept 2025 – Mar 2026 (an academic-year label, not a calendar
      year), which is easy to misread. Consider `2023_24_season/`, `2025_26_season/`, or similar.
- [ ] Harmonize species-name strings at the source (`"L. sicilis"` vs `"L. Sicilis"` vs
      `"L.sicilis"`, `"L. minutus"` vs `"L. Minutus"`) in one place during ingestion, rather than
      the three independent `case_when`/`if_else` blocks currently in `01_data_processing.R:136`,
      `02_controller.R:50-54`, and `02_controller.R:58/68-71`.
- [ ] Add a short data dictionary (e.g. `Background/data_dictionary.md`) documenting what each
      raw column means per year/type, especially `treatment` (means different things in the
      survival vs. CTmax pipelines), `salt`, `dose_assignment`, `cl_conc`, and the `0.7` chloride
      conversion factor used for RoadSalt (`01_data_processing.R:144`) — cite its source.
- [ ] Decide what to do with `Raw_data/2025_data/ramp_data/` — currently never read by any script.
      Either incorporate it (if it's needed for CTmax QC) or document why it's archival-only.
- [ ] Extract text from `Raw_data/2025_data/2025_column_data.pdf` (likely a column/data dictionary)
      and fold its contents into the data dictionary above.

## Phase 2 — Processing pipeline refactor (`Scripts/01_data_processing.R`)

- [ ] Replace the two near-duplicate survival for-loops (2024 block ~13–77, 2025 block ~80–129)
      with a single parameterized function, e.g. `read_survival_file(path, schema = c("2024","2025"))`,
      once column schemas are reconciled.
- [ ] Replace the two near-duplicate CTmax blocks (~166–196, ~198–220) similarly.
- [ ] Replace column-position-dependent loops (`for(i in 6:dim(raw_data)[2])`,
      `for(i in 7:dim(raw_data)[2])`) with `select(starts_with("hour_"))` /
      `select(matches("^x?\\d+$"))`-style selection so they don't silently break if upstream
      `select()` order changes.
- [ ] Standardize on one pipe operator (`|>`) throughout; currently mixes `|>` and `%>%` within
      the same blocks.
- [ ] Document or parameterize magic numbers: `acc_time = 6` (line 191), the `0.7` RoadSalt
      chloride conversion (line 144), and the `ctmax > 15` outlier cutoff (line 220) — move these
      to named constants with a comment explaining their derivation.
- [ ] Make the `est_ctmax()` join (`project_functions.R:28`) key on `experiment_date`/`experiment`
      in addition to `tube`, since tube numbers repeat across trials.
- [ ] Replace the row-order-dependent `ID` assignment / `bind_rows(mort_1, mort_2)` pattern in
      `expand_surv()`/`make_surv()` (`project_functions.R:49-76`) with an explicit, stable
      per-individual key.
- [ ] Move the exploratory `ggplot` calls at lines 146–155 and 229–232 out of the processing
      script (either delete, or move into a QC/diagnostics script that isn't run as part of the
      main pipeline).
- [ ] Standardize `here::here()` usage and fix path casing (`here::here("output", "output_data")`
      vs actual folder `Output/Output_data`) — use consistent casing everywhere or rely on a
      case-insensitive-safe convention explicitly.

## Phase 3 — Report / analysis cleanup (`Output/Reports/report.Rmd`)

- [ ] Move `theme_matt`/`theme_matt_facets` (duplicated in `report.Rmd` and `prelim_data.Rmd`)
      into `project_functions.R` and `source()` it instead.
- [ ] Replace the *Skistodiaptomus* survival section (`report.Rmd:132-155`), which reads a raw CSV
      directly and recomputes proportions inline, with the shared `expand_surv()`/`make_surv()`
      pipeline output.
- [ ] Replace the manual per-row expansion loop (`report.Rmd:361-382`) with the existing
      `expand_surv()`/`make_surv()` functions.
- [ ] Move hardcoded exclusion filters (`report.Rmd:178-180`, excluding specific experiment
      dates) into the data-processing script with a documented reason, rather than leaving them
      unexplained in the report.
- [ ] Replace the hardcoded date-based duplicate-run disambiguation
      (`02_controller.R:61`: `assay = if_else(assay_date == "2026-03-31" & treatment == "NaCl", 2, 1)`)
      with a rule driven by the `_rep1`/`_rep2` file-naming convention now in place.
- [ ] Remove dead/commented-out analysis code (`report.Rmd:108-122`, `231-236`, `389-391`).
- [ ] Document the rationale for using three different modeling frameworks (Cox PH, `glmmTMB`,
      per-subset `glm`/`dose.p` for LC50) across the different survival datasets.
- [ ] Decide the status of `prelim_data.Rmd` (superseded pilot report) — archive it or clearly
      label it as historical so it isn't confused with the current `report.Rmd`.

## Phase 4 — Documentation & housekeeping

- [ ] Update `README.md` — it references stale script names (`01_Data_analysis.R`,
      `02_make_report.R`) and a stale `Data/` directory that no longer match the actual project
      layout (`01_data_processing.R`, `02_controller.R`, `Raw_data/`).
- [ ] Fill in the placeholder fields in `README.md` (title, authors, affiliations, funding).
- [ ] Add a top-level `.gitignore` entry (or confirm existing ones cover) `.DS_Store`, `.Rhistory`,
      `.RData` — currently several `.DS_Store` files are present under `Raw_data/`.
- [ ] Reconcile `Background/data_templates/raw_surv_data.xlsx` (blank data-entry template) with
      the fact that no script ingests `.xlsx` — either update the template to match the current
      CSV export format, or document the manual Excel→CSV export step.
- [ ] Label figure chunks in `report.Rmd`/`prelim_data.Rmd` so output files in
      `Output/Figures/report/` aren't all named `unnamed-chunk-N`.
