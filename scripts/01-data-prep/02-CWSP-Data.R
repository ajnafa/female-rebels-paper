#------------------------CWSP Data Pre-Processing------------------------------
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
#----------------UCDP Translation Tables and Actor Information-----------------
#-----------------------------------------------------------------------------#

# Load the Dyad ID translation tables
dyad_trans <- read_csv("data/ucdp/translate_dyad.csv", col_types = "cccd") %>% 
  # Exclude one-sided violence dyads
  filter(type_of_violence == 1)  %>% 
  # Split name into side a and side b
  separate(name, into = c("side_a", "side_b"), sep = " - ")

#-----------------------------------------------------------------------------#
#--------------------Importing and Cleaning the CWSP Data----------------------
#-----------------------------------------------------------------------------#

# Read in the CWSP Data from Daly (2020)
cwsp <- read_rds("data/cwsp/CWSP_Dataset_V2_JPR.rds") %>% 
  # Keep observations for rebel parties
  filter(rebel == 1) %>% 
  # Rename the election year variables
  rename_with( ~ str_replace_all(
    .x,
    c(
      "year_first_legislative" = "first_legislative",
      "year_first_presidential" = "first_presidential",
      "year_first_regional" = "first_regional"
    )
  )) %>% 
  # Transmute a subset of the data
  transmute(
    # Country Name
    country = location,
    # Region
    region,
    # Conflict Start Year
    conflict_start = epstartyear,
    # Conflict End Year
    conflict_end = ependyear,
    # Conflict Identifier
    conflict_id = as.character(conflictid),
    # Rebel Group Name
    rebels_name = belligerent,
    # Original Dyad ID
    orig_dyadid = dyadid,
    # Dyad ID
    cwsp_dyad_id = case_when(
      belligerent == "Mozambique National Resistance Movement (RENAMO)" ~ "99",
      belligerent == "NTC" ~ "823",
      belligerent == "Cobra" ~ "189",
      belligerent == "FUC" ~ "455",
      dyadid == 291 ~ "303",
      dyadid == 42 ~ "40",
      TRUE ~  as.character(dyadid)
    ),
    # Successor Party Name
    party_name = na_if(
      str_replace_all(successorparty, "NA|Unclear", ""),
      ""
      ),
    # Succesor Party Banned
    party_ban = banned,
    # Successor Party Participation
    non_participation = non_participation,
    # Conflict Recurrence Initiated by Rebels
    reinit_war = reinit_war,
    # Conflict Mediation/UN Intervention
    un_intervention = unintrvnall,
    # Focus on only the first of each type of election to limit confounding
    across(starts_with("first_"))
  ) %>% 
  # Merge in the UCDP dyad translation table
  left_join(dyad_trans[,1:2], by = c("cwsp_dyad_id" = "old_id"))

# Write the Pre-Processed Data to a File
write_rds(
  x = cwsp, 
  file = "output/project-data/cwsp_data.rds",
  compress = "gz",
  compression = 9L
)
