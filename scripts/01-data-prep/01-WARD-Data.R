#-------------------------WARD Data Pre-Processing-----------------------------
#-Author: A. Jordan Nafa----------------------------Created: February 1, 2022-#
#-R Version: 4.1.2--------------------------------Last Modified: July 8, 2022-#

# Load the necessary libraries for the script----
pacman::p_load(
  "tidyverse",
  "data.table",
  "dtplyr",
  install = FALSE
)

#-----------------------------------------------------------------------------#
#---------------UCDP Translation Tables and Actor Information------------------
#-----------------------------------------------------------------------------#

# Load the Dyad ID translation tables
dyad_trans <- read_csv("data/ucdp/translate_dyad.csv", col_types = "cccd") %>% 
  # Exclude one-sided violence dyads
  filter(type_of_violence == 1)  %>% 
  # Split name into side a and side b
  separate(name, into = c("side_a", "side_b"), sep = " - ")

#-----------------------------------------------------------------------------#
#-------------------Importing and Cleaning the WARD Data-----------------------
#-----------------------------------------------------------------------------#

# Read in the WARD v1.3 Data
ward <- read_rds("data/ward/WARD_v1.3.rds") %>%
  # Split comma-seperated cells into unique rows
  separate_rows(dyadid_old, sep = ", ") %>%
  # Transmute a subset of the data
  transmute(
    # Name of Side A in the Conflict
    sidea = sidea,
    ## GWNO Code for side a
    gwno_sidea = gwnoa,
    # Rebel Group Abbreviation
    rebel_name = sideb,
    # Rebel Group ID
    rebel_id = case_when(
      rebel_name == "Seleka" ~ "554",
      sidebid == 4456.1 ~ "4456",
      TRUE ~ as.character(sidebid)
    ),
    # Old UCDP Dyad ID
    dyadid_old = str_squish(dyadid_old),
    # Female Rebel Presence (Best Estimate)
    femrebels = female_combatants_best,
    # Female Rebel Prevalence (Best Estimate)
    femrebels_prv = factor(
      cat4_prevalence_best,
      levels = 0:3,
      labels = c("None", "Low", "Moderate", "High")
    ),
    # Female Rebel Presence (Best Estimate, Excluding Suicide Bombers)
    femrebels_exs = female_combatants_exs,
    # Female Rebel Prevelance (Best Estimate, Excluding Suicide Bombers)
    femrebels_exsprv = factor(
      cat4_prevalence_exs,
      levels = 0:3,
      labels = c("None", "Low", "Moderate", "High")
    )
  ) %>% 
  # Merge in the dyad id translation table 
  left_join(dyad_trans[, 1:2], by = c("dyadid_old" = "old_id"))

# Write the Pre-Processed Data to a File
write_rds(
  x = ward, 
  file = "output/project-data/WARD_Data.rds",
  compress = "gz",
  compression = 9L
)
