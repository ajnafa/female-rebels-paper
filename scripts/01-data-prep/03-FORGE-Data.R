#------------------------FORGE Data Pre-Processing-----------------------------
#-Author: A. Jordan Nafa----------------------------Created: February 1, 2022-#
#-R Version: 4.1.2--------------------------------Last Modified: July 8, 2022-#

# Load the necessary libraries for the script----
pacman::p_load(
  "tidyverse",
  "data.table",
  "dtplyr",
  "lubridate",
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
#-------------------Importing and Cleaning the FORGE Data----------------------
#-----------------------------------------------------------------------------#

# Read in the FORGE Data
forge <- haven::read_dta("data/forge/forge_v1.0_public.dta") %>% 
  # Transmute a subset of the data
  transmute(
    # Country
    cntry = cname,
    # UCDP Conflict ID
    conflict_id = as.character(conflict_id),
    # UCDP Dyad ID
    dyad_id = case_when(
      gacronym == "NSCN-K" ~ "509",
      gacronym == "JKLF" ~ "792",
      gacronym == "TNSM" ~ "856",
      gacronym == "TNSM" ~ "856",
      str_detect(gacronym, "Gervais Nyangoma") ~ "592",
      TRUE ~ as.character(dyadid)
    ) %>% na_if(., ""),
    # Rebel Group ID
    rebel_id = as.character(actorid),
    # Rebel Group Acronym
    rebel_name_abv = gacronym,
    # Rebel Group Name
    rebel_name = gname,
    # Year of Formation
    form_year = foundyear,
    # Date of Formation
    form_date = mdy(case_when(
      !is.na(foundmo) & !is.na(foundday) ~ str_c(
        foundmo, foundday, foundyear, sep = "-"
      ),
      foundmo == 2 & is.na(foundday) ~ str_c(
        foundmo, "28", foundyear, sep = "-"
      ),
      !is.na(foundmo) & is.na(foundday) ~ str_c(
        foundmo, "30", foundyear, sep = "-"
      ),
      is.na(foundmo) & is.na(foundday) ~ str_c(
        "12", "31", foundyear, sep = "-"
      )
    )),
    # Date of First UCDP Incompatability
    fight_year = fightyear,
    # Parent Organization is a Political Party
    party_parent = preorgpar,
    # Name of Parent Organization
    party_name = if_else(party_parent == 1, preorgname, NA_character_),
    # Rebel Group Ethnic
    ethnic_group = ethnic,
    # Rebel Group Ethnic Identity
    ethnic_identity = na_if(ethnicity, ""),
    # Rebel Group Religious
    relig_group = religious,
    # Rebel Group Religious
    relig_identity = religion,
    # Rebel Group Goals: Independence
    idependence = goalindep,
    # Rebel Group Goals: Autonomy
    autonomy = goalauto,
    # Rebel Group Goals: Improved Rights
    rights = goalrights,
    # Rebel Group Goals: Improved Representation
    representation = goalrep,
    # Rebel Group Goals: Regime Change
    regime_change = goalchange,
    # Rebel Group Goals: Regime Change
    democratization = goaldem,
    # Rebel Group Goals: Other
    other_goal = goalother,
    # Rebel Group Goals: Notes
    goal_notes = goalnote,
    # Rebel Group Ideology: Communist
    communist = ideolcom,
    # Rebel Group Ideology: Other Leftist
    leftist = ideolleft,
    # Rebel Group Ideology: Right-Wing
    rightwing = ideolright,
    # Rebel Group Ideology: Nationalist 
    nationalist = ideolnat,
    # Rebel Group Ideology: Anti-System
    antisystem = ideolanti,
    # Rebel Group Ideology: Religious
    religious = ideolrel,
    # Rebel Group Ideology: Other
    other_ideol = ideoloth,
    # Rebel Group Ideology: Notes
    ideol_notes = ideolnote
  ) %>% 
  # Merge in the dyad id translation table 
  left_join(dyad_trans[, 1:2], by = c("dyad_id" = "new_id"))

# Write the Pre-Processed Data to a File
write_rds(
  x = forge, 
  file = "output/project-data/forge_rebel_origins.rds",
  compress = "gz",
  compression = 9L
)
