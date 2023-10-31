#---------------------------Main Data Build file--------------------------------
#-Author: A. Jordan Nafa----------------------------Created: November 13, 2022-#
#-R Version: 4.2.1----------------------------------Revised: November 15, 2022-#

# Load the necessary libraries
pacman::p_load(
  "tidyverse", # Suite of packages for data management
  "sjlabelled", # Package for working with labels
  install = FALSE
)

#------------------------------------------------------------------------------#
#---------------------Reading in the Pre-Processed Data-------------------------
#------------------------------------------------------------------------------#

# CWSP Data
cwsp <- read_rds("output/project-data/cwsp_data.rds") %>% 
  # Drop one observation without a UCDP ID
  drop_na(cwsp_dyad_id) %>% 
  # Generate a row identfier
  mutate(row_id = 1:n())
  
# WARD Female Combatants Data
ward <- read_rds("output/project-data/WARD_Data.rds")

# FORGE Data
forge <- read_rds("output/project-data/forge_rebel_origins.rds")

# QSI Measurement Model Output
qsi <- read_rds("output/project-data/rebel_gov_index.rds") %>% 
  # Subset and rename
  select(
    new_id = actor,
    qsi_pol = Political_med,
    qsi_pol_sd = Political_sd,
    qsi_econ = Economic_med,
    qsi_econ_sd = Economic_sd,
    qsi_social = Social_med,
    qsi_social_sd = Social_sd
  )

#------------------------------------------------------------------------------#
#------------------------------Merging the Data---------------------------------
#------------------------------------------------------------------------------#

# Merge the data sets
project_df <- cwsp %>% 
  # Merge in the latent trait estimates
  left_join(qsi, by = "new_id") %>% 
  # Merge in the ward data
  left_join(ward, by = "new_id") %>% 
  # Merge in the forge data
  left_join(
    forge, 
    by = c("new_id" = "dyad_id"),
    suffix = c("", "_forge")
  ) %>% 
  # Subset the relevant predictors
  select(
    country:rebels_name,
    party_name, 
    party_ban:first_regional, 
    qsi_pol:qsi_social_sd,
    femrebels:femrebels_exsprv,
    party_parent,
    party_parent_name = party_name_forge,
    ethnic_group:ideol_notes
  ) %>% 
  # Create predictors for the analysis
  mutate(
    # Whether a rebel group formed a party, even if that party didn't compete
    party_exist = if_else(!is.na(party_name), 1, 0),
    # Whether a rebel group formed a party and participated in elections
    elect_party = if_else(non_participation == 0, 1, 0),
    # No post-conflict election
    no_election = case_when(
      is.na(first_legislative) & is.na(first_presidential) & is.na(first_regional) ~ 1,
      TRUE ~ 0
    ),
    # Presence of Female Rebels (Best Estimate)
    across(
      c(femrebels, femrebels_exs),
      ~ factor(
        replace_na(.x, 0),
        levels = 0:1,
        labels = c("No Female Combatants", "Female Combatants")
      )),
    # Presence of Female Rebels Ordinal
    across(
      c(femrebels_prv, femrebels_exsprv),
      ~ factor(
        replace_na(as.integer(.x), 1),
        levels = 1:4,
        labels = c("None", "Low", "Moderate", "High")
      )),
    # Rebel Group's Parent Organization is a Political Party
    party_affil = replace_na(party_parent, 0),
    # Communist or Left-Wing
    left_ideol = if_else(communist == 1 | leftist == 1, 1, 0),
    # Africa indicator
    africa = if_else(region == 4, 1, 0),
    # UN Intervention
    un_intervention = replace_na(un_intervention, 0)
  )

# Write the predictions to a file
write_rds(
  project_df,
  "output/project-data/model_data.rds",
  compress = "xz", 
  compression = 9L
)


