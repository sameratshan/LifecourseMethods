###############################################################################
# Purpose: Load libaries and datasets for National Child Development Study (NCDS)
###############################################################################

# Load Libraries
library(here)
library(tidyverse)
library(haven)
library(labelled)
library(lavaan)

# Load Datasets

## Harmonized Childhood Covariates
df.NCDS.HCOVS <- read_stata(here("Data", "UKDA-8552-stata", "stata", "stata13", "ncds_closer_wp9.dta"))

## NCDS Early Childhood 0-2
df.NCDS.Early <- read_stata(here("Data", "UKDA-5565-stata", "stata", "stata13", "ncds0123.dta")) %>%
  select(ncdsid,
         ### Parent's BMI
         n1199, # Father's Height
         n1196, # Father's Weight
         n1205, # Mother's Height
         n1202, # Mother's Weight
         ### Father/Father Figure Occupation
         n2383,
         ### Child's Birthweight
         n646,
         ### Person's Per Room
         n607
  ) %>%
  mutate(
    across(c(n1199, n1205),
           ~ na_if(as.numeric(.x), -1)),
    across(c(n607, n2383),
           ~ {
             x_old <- .x
             x_new <- na_if(as.numeric(x_old), -1)
             copy_labels(x_old, x_new)
           }),
    father_height_m = n1199 * 0.0254, # Convert inches to meters
    mother_height_m = n1205 * 0.0254, # Convert inches to meters
    
    # Extract label strings
    father_label = haven::as_factor(n1196),
    mother_label = haven::as_factor(n1202),
    
    f_match = str_match(father_label, "^(\\d{1,2})st\\s*(\\d{1,2})lbs\\s*(?:,|to)\\s*(\\d{1,2})-(\\d{1,2})$"), # Regex to capture stone and pound ranges for fathers
    m_match = str_match(mother_label, "^(\\d{1,2})st\\s*(\\d{1,2})lbs\\s*(?:,|to)\\s*(\\d{1,2})-(\\d{1,2})$"), # Regex to capture stone and pound ranges for mothers
    
    # Convert captures to numeric
    f_stone_lo = as.numeric(f_match[, 2]),
    f_pound_lo = as.numeric(f_match[, 3]),
    f_stone_hi = as.numeric(f_match[, 4]),
    f_pound_hi = as.numeric(f_match[, 5]),
    
    m_stone_lo = as.numeric(m_match[, 2]),
    m_pound_lo = as.numeric(m_match[, 3]),
    m_stone_hi = as.numeric(m_match[, 4]),
    m_pound_hi = as.numeric(m_match[, 5]),
    
    # Compute midpoints in lbs
    f_weight_lb = ((f_stone_lo * 14 + f_pound_lo) + (f_stone_hi * 14 + f_pound_hi)) / 2, # Convert stone and pound ranges to a single weight in pounds for fathers
    m_weight_lb = ((m_stone_lo * 14 + m_pound_lo) + (m_stone_hi * 14 + m_pound_hi)) / 2, # Convert stone and pound ranges to a single weight in pounds for mothers
    
    # Convert to kg
    father_weight_kg = f_weight_lb * 0.453592, # Convert pounds to kilograms for fathers
    mother_weight_kg = m_weight_lb * 0.453592, # Convert pounds to kilograms for mothers
    
    # Calculate BMI for both parents
    father_bmi = father_weight_kg / (father_height_m^2), # Calculate BMI for fathers
    mother_bmi = mother_weight_kg / (mother_height_m^2) # Calculate BMI for mothers
  ) %>% 
  mutate(
    n646 = case_when(
      n646 == -1 ~ NA_character_,
      n646 == 508 ~ "High",
      n646 == 509 ~ "Low",
      n646 < 88 ~ "Low",
      n646 <= 120 ~ "Normal",
      n646 > 120 ~ "High"
    ) %>% factor(
      levels = c("Low", "Normal", "High")
    ),
    n607 = case_when(
      n607 == 1 ~ "Up to 1",
      n607 == 2 ~ "Over 1 to 1.5",
      n607 == 3 ~ "Over 1.5 to 2",
      n607 == 4 ~ "Over 2"
    ) %>% factor(
      levels = c("Up to 1", "Over 1 to 1.5", "Over 1.5 to 2", "Over 2")
    ),
    n2383 = case_when(
      n2383 == 1 ~ "No male present",
      n2383 == 2 ~ "Unemployed, sick, retired, or other",
      n2383 == 3 ~ "Unemployed, sick, retired, or other",
      n2383 == 4 ~ "Unemployed, sick, retired, or other",
      n2383 == 5 ~ "Unemployed, sick, retired, or other",
      n2383 == 6 ~ "Employed",
      TRUE        ~ NA_character_
    ) %>% factor(
      levels = c("Employed", "Unemployed, sick, retired, or other", "No male present")
  )
  ) %>%
  set_variable_labels(n646 = "Birthweight (ounces)") %>%
  set_variable_labels(n2383 = "Father Occupation") %>%
  select(ncdsid,father_bmi, mother_bmi, n646, n607, n2383) # Early Life Covariates


## NCDS Sweep 9 (age 55)
df.NCDS.S9 <- read_stata(here("Data", "UKDA-7669-stata", "stata", "stata13", "ncds_2013_flatfile.dta")) %>%
  select(NCDSID, N9CMSEX, N9INTMON, N9INTYR) %>%
  left_join(
    read_stata(here("Data", "UKDA-7669-stata", "stata", "stata13", "ncds_2013_derived.dta")) %>%
      select(NCDSID, ND9BMI) %>%
      rename(bmi = ND9BMI) %>%
      mutate(bmi = case_when(as.numeric(bmi) < 0 ~ NA_real_, TRUE ~ as.numeric(bmi))),
    by = "NCDSID"
  ) %>%
  rename(ncdsid = NCDSID, sex = N9CMSEX) %>%
  mutate(
    sex = recode(sex, `1` = "Male", `2` = "Female"),
    birth_decimal = 1958 + (3 - 0.5) / 12, # Assuming birth in March 1958 for all participants (midpoint of the birth month)
    interview_month = as.numeric(N9INTMON),
    interview_year = as.numeric(N9INTYR),
    interview_decimal = interview_year + (interview_month - 0.5) / 12, # Convert interview date to decimal format (year + fraction of year)
    xage = interview_decimal - birth_decimal, # Calculate age at interview in decimal years
    visitage = 55,
    stid = "NCDS",
    wtself = 2
  ) %>%
  select(ncdsid, stid, visitage, sex, xage, wtself, bmi)



## NCDS Sweep 10 (age 62)
df.NCDS.S10 <- read_stata(here("Data", "UKDA-9412-stata", "stata", "stata13_se", "ncds10_age62_main_interview.dta")) %>%
  select(ncdsid,
         nd10bmi_rec, nd10bmi_r2_rec, nd10ageint, nd10ageint_r2,
         n10cmsex,
         nd10hachq) %>%
  mutate(across(c(nd10bmi_r2_rec, nd10bmi_rec, nd10ageint_r2, nd10ageint, n10cmsex),
                ~ case_when(as.numeric(.x) < 0 ~ NA_real_, TRUE ~ as.numeric(.x)))) %>%
  mutate(
    bmi = coalesce(nd10bmi_r2_rec, nd10bmi_rec), # Use the r2 version of BMI if available, otherwise use the original
    xage = coalesce(nd10ageint_r2, nd10ageint) / 12, # Convert age from months to years, using the r2 version if available
    visitage = 62,
    sex = ifelse(n10cmsex == 1, "Male", "Female"),
    wtself = 2,
    educ_qual = case_when(
      nd10hachq < 0             ~ NA_character_,
      nd10hachq == 0            ~ "No Qualifications",
      nd10hachq %in% c(1, 2, 3) ~ "GCSEs or Equivalent",
      nd10hachq %in% c(4, 5)    ~ "A Levels or Equivalent",
      nd10hachq %in% c(6, 7, 8) ~ "Degree or Higher"
    ),
    educ_qual = factor(educ_qual, levels = c("No Qualifications", "GCSEs or Equivalent",
                                              "A Levels or Equivalent", "Degree or Higher"))
  ) %>%
  relocate(c("bmi", "xage"), .after = "ncdsid")


df.NCDS.S10.bio <- read_stata(here("Data", "UKDA-9412-stata", "stata", "stata13_se", "ncds10_age62_biomeasures.dta")) %>%
  select(ncdsid,
         n10nursedatem, n10nursedatey,
         nd10mbmi_rec,
         n10hscrp,
         n10hscrpq) %>%
  mutate(across(c(nd10mbmi_rec, n10hscrp),
                ~ case_when(as.numeric(.x) < 0 ~ NA_real_, TRUE ~ as.numeric(.x)))) %>%
  mutate(
    birth_decimal = 1958 + (3 - 0.5) / 12, # Assuming birth in March 1958 for all participants (midpoint of the birth month)
    interview_month = as.numeric(n10nursedatem),
    interview_year = as.numeric(n10nursedatey),
    interview_decimal = interview_year + (interview_month - 0.5) / 12, # Convert interview date to decimal format (year + fraction of year)
    xage_bio = interview_decimal - birth_decimal # Calculate age at interview in decimal years based on the nurse visit date, which is when the biomeasures were taken
  )


## Harmonized HBMI file
df.NCDS.HBMI <- read_stata(here("Data", "UKDA-8549-stata", "stata", "stata13",
                                "ncds_closer_wp1.dta")) %>%
  mutate(sex = recode(as.character(sex), `1` = "Male", `2` = "Female"))

df.NCDS.S10.harmonized <- df.NCDS.S10 %>%
  select(ncdsid, bmi, xage, visitage, sex, wtself) %>%
  left_join(df.NCDS.S10.bio %>% select(ncdsid, nd10mbmi_rec, xage_bio),
            by = "ncdsid") %>%
  mutate(
    bmi    = coalesce(nd10mbmi_rec, bmi),
    xage   = coalesce(xage_bio, xage),
    wtself = if_else(!is.na(nd10mbmi_rec), 1, wtself),
    sex    = as.character(sex),
    stid   = "NCDS"
  ) %>%
  select(ncdsid, stid, visitage, sex, xage, wtself, bmi)

ids_hbmi <- unique(df.NCDS.HBMI$ncdsid) # IDs that already exist in the legacy HBMI file 

### Keep ONLY those Sweep-9 / Sweep-10 rows whose ID is in HBMI
df.NCDS.S9_keep  <- df.NCDS.S9             %>% filter(ncdsid %in% ids_hbmi)
df.NCDS.S10_keep <- df.NCDS.S10.harmonized %>% filter(ncdsid %in% ids_hbmi)

### Stitch them together and balance the panel
df.NCDS.HBMI <- bind_rows(
  df.NCDS.HBMI,
  df.NCDS.S9_keep,
  df.NCDS.S10_keep
) %>% 
  mutate(
    ncdsid   = as.character(ncdsid),
    visitage = as.integer(visitage),
    stid     = coalesce(stid, "NCDS")
  ) %>% 
  complete(ncdsid, visitage = c(55L, 62L), fill = list(stid = "NCDS")) %>% 
  arrange(ncdsid, visitage) %>% 
  group_by(ncdsid) %>% 
  fill(sex, closerid, stid, .direction = "downup") %>% 
  ungroup()

# List of biomarkers to include
biomarkers <- c("loghsCRP")
# List of covariates across all models
covs <- c("sex", "sweep10_age", "educ_qual", "father_bmi", "mother_bmi", "n646", "n607", "n2383")

# Define the analytic sample by merging necessary datasets and selecting relevant variables
df.model <- df.NCDS.S10.bio %>% # Start with the wave 10 biomarker dataset
  left_join(df.NCDS.S10 %>% select(ncdsid, sex, educ_qual), by = "ncdsid") %>% # Merge education and sex from wave 10
  left_join(df.NCDS.Early, by = "ncdsid") %>% # Merge early life covariates
  rename(
    hsCRP = n10hscrp,
    sweep10_age = xage_bio
  ) %>%
  mutate(
    loghsCRP = ifelse(hsCRP > 10, NA, log(hsCRP)) # Log-transform hsCRP and set values above 10 mg/L to NA, as they may indicate acute infection rather than chronic inflammation
  ) %>%
  drop_na(loghsCRP, all_of(covs))

# Define the BMI dataset for the analytic sample, keeping only those with complete data on BMI and age, and creating a self-report indicator based on the wtself variable
df.NCDS.HBMI.complete.analytic <- df.NCDS.HBMI %>%
  filter(ncdsid %in% df.model$ncdsid) %>%
  filter(visitage != 62) %>% # Exclude wave 10 BMI measurements to avoid confounding
  mutate(self_report = ifelse(wtself == 2, 1, 0)) %>%
  select(ncdsid, xage, sex, bmi, self_report) %>%
  drop_na(bmi, xage) %>%
  mutate(ncdsid = as_factor(ncdsid))

# Merge the BMI dataset with the main analytic sample dataset for the model dataset
df.model <- df.model %>% 
  filter(ncdsid %in% df.NCDS.HBMI.complete.analytic$ncdsid) %>%
  select(ncdsid, loghsCRP, all_of(covs)) %>%
  filter(complete.cases(.))

# Define the formula for the bam model with a factor-smooth interaction between age and individual ID, and self-reported vs. measured BMI

# mod.BMI.fs.bam <- mgcv::bam(
#   bmi ~ self_report + 
#     s(xage, ncdsid, bs = 'fs', k = 4),  
#   data = df.NCDS.HBMI.complete.analytic,
#   discrete = TRUE,  # Speed up computation
#   nthreads = 4      # Use parallel processing
# )

# saveRDS(mod.BMI.fs.bam, file = here("Data","Models","mod.BMI.fs.bam.rds"))


