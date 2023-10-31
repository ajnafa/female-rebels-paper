#-----------------Quasi-State Institutions Measurement Model--------------------
#-Author: A. Jordan Nafa--------------------------------Created: July 13, 2022-#
#-R Version: 4.1.2----------------------------------Revised: November 12, 2022-#

## Load the necessary libraries
pacman::p_load(
  "tidyverse", # Suite of packages for data management 
  "brms", # Bayesian regression models with Stan
  "tidybayes", # Functions for wrangling posteriors tidy-style
  "bayesplot", # Package for making tables of model output
  "ggdist", # Package for customizing html and latex tables
  "patchwork", # Combining multiple plots into one
  "future", # Package for parallel computation
  install = FALSE
)

# Set future memory limit options
options(
  future.globals.maxSize = 1e11,
  future.rng.onMisuse = "ignore",
  cmdstanr_write_stan_file_dir = "output/fits/measurement/mirt/"
)

#-----------------------------------------------------------------------------#
#------------------------------Data Preparation--------------------------------
#-----------------------------------------------------------------------------#

# Load in the QSI data from 04-QSI-Data.R
qsi_mirt <- read_rds("output/project-data/qsi_long_data.rds") %>% 
  # Transmute the relevant vectors
  transmute(
    qsi,
    actor = join_id,
    start_year,
    stop_year,
    across(c(actor, item, dimension), ~ as_factor(.x))
  ) %>% 
  ## Drop any missing observations
  drop_na() %>% 
  ## Filter the dimensions for the model
  filter(
    dimension %in% c("Political", "Social", "Economic") &
    !item %in% c("pol_polwing", "pol_attemptio", "soc_rep", "econ_illegalnet")
    ) %>% 
  # Drop unused levels
  droplevels()

#------------------------------------------------------------------------------#
#---------------Specifying the Bayesian Item Response Model 1-------------------
#------------------------------------------------------------------------------#

# Model 1 includes item-by-dimension varying intercepts and dimension-by-actor 
# varying slopes but not intercepts.

## Formula for a multi-dimensional 2PL model
qsi_2pl_mirt_form_mod1 <- bf(
  qsi ~ beta + exp(logalpha) * theta,
  logalpha ~ 1 + (1 | dimension/item),# Item-by-Dimension varying intercepts
  theta ~ 0 + (0 + dimension | actor), # Dimension-varying slopes over actors
  beta ~ 1 + (1 | item), # Item-specific intercepts
  family = brmsfamily("bernoulli", link = "logit"),
  nl = TRUE
)

# Specify priors for the model parameters
qsi_mirt_2pl_priors_mod1 <- 
  prior(normal(0, 1), class = b, nlpar = beta) +
  prior(normal(0, 1), class = b, nlpar = logalpha) +
  prior(student_t(10, 0, 1), class = sd, nlpar = beta) +
  prior(student_t(10, 0, 1), class = sd, nlpar = logalpha) +
  prior(student_t(10, 0, 1), class = sd, nlpar = theta) +
  prior(lkj(5), class = cor)

# Fit the MIRT 2PL model
qsi_mirt_2pl_fit_mod1 <- brm(
  formula = qsi_2pl_mirt_form_mod1,
  prior = qsi_mirt_2pl_priors_mod1,
  data = qsi_mirt,
  cores = 8,
  chains = 6,
  iter = 10e3,
  warmup = 5e3,
  thin = 4,
  init = 0.5,
  refresh = 100,
  seed = 12345,
  backend = "cmdstanr",
  control = list(
    adapt_delta = .99,
    max_treedepth = 15,
    step_size = 0.02
  ),
  stan_model_args = list(stanc_options = list("O1")),
  save_pars = save_pars(all = TRUE),
  file = "output/fits/measurement/mirt/QSI_2PL_MIRT_Mod1.rds",
  output_basename = "QSI_2PL_MIRT_Mod1",
  save_model = "output/fits/measurement/mirt/QSI_2PL_MIRT_Mod1.stan"
)

#------------------------------------------------------------------------------#
#-------------------K-Fold Cross Validation for Model 1-------------------------
#------------------------------------------------------------------------------#

# Use future to parallelize cross validation
plan(multisession(workers = 12))

# Perform k-fold cross validation for the full model
qsi_mirt_2pl_fit_mod1 <- add_criterion(
  qsi_mirt_2pl_fit_mod1,
  criterion = "kfold",
  folds = "grouped",
  group = "item",
  save_fits = TRUE,
  cores = 6,
  chains = 6,
  iter = 6e3,
  warmup = 5e3,
  seed = 12345,
  ndraws = 1e3,
  allow_new_levels = TRUE,
  control = list(
    adapt_delta = .99,
    max_treedepth = 15,
    step_size = 0.02
  ),
  stan_model_args = list(stanc_options = list("O1")),
  save_pars = save_pars(all = TRUE)
)

# Close Future Sessions
plan(sequential)

#------------------------------------------------------------------------------#
#---------------Specifying the Bayesian Item Response Model 2-------------------
#------------------------------------------------------------------------------#

# Model 2 includes item-by-dimension varying intercepts and dimension-by-actor 
# varying slopes and intercepts.

## Formula for a multi-dimensional 2PL model
qsi_2pl_mirt_form_mod2 <- bf(
  qsi ~ beta + exp(logalpha) * theta,
  logalpha ~ 1 + (1 | dimension/item),# Item-by-Dimension varying intercepts
  theta ~ 0 + (1 + dimension | actor), # Dimension-varying slopes over actors
  beta ~ 1 + (1 | item), # Item-specific intercepts
  family = brmsfamily("bernoulli", link = "logit"),
  nl = TRUE
)

# Specify priors for the model parameters
qsi_mirt_2pl_priors_mod2 <- 
  prior(normal(0, 1), class = b, nlpar = beta) +
  prior(normal(0, 1), class = b, nlpar = logalpha) +
  prior(student_t(10, 0, 1), class = sd, nlpar = beta) +
  prior(student_t(10, 0, 1), class = sd, nlpar = logalpha) +
  prior(student_t(10, 0, 1), class = sd, nlpar = theta) +
  prior(lkj(5), class = cor)

# Fit the MIRT 2PL model
qsi_mirt_2pl_fit_mod2 <- brm(
  formula = qsi_2pl_mirt_form_mod2,
  prior = qsi_mirt_2pl_priors_mod2,
  data = qsi_mirt,
  cores = 8,
  chains = 6,
  iter = 10e3,
  warmup = 5e3,
  thin = 4,
  init = 0.5,
  refresh = 100,
  seed = 12345,
  backend = "cmdstanr",
  control = list(
    adapt_delta = .99,
    max_treedepth = 15,
    step_size = 0.02
  ),
  stan_model_args = list(stanc_options = list("O1")),
  save_pars = save_pars(all = TRUE),
  file = "output/fits/measurement/mirt/QSI_2PL_MIRT_Mod2.rds",
  output_basename = "QSI_2PL_MIRT_Mod2",
  save_model = "output/fits/measurement/mirt/QSI_2PL_MIRT_Mod2.stan"
)

#------------------------------------------------------------------------------#
#--------------------K-Fold Cross Validation for Model 2------------------------
#------------------------------------------------------------------------------#

# Use future to parallelize cross validation
plan(multisession(workers = 12))

# Perform k-fold cross validation for the full model
qsi_mirt_2pl_fit_mod2 <- add_criterion(
  qsi_mirt_2pl_fit_mod2,
  criterion = "kfold",
  folds = "grouped",
  group = "item",
  save_fits = TRUE,
  cores = 6,
  chains = 6,
  iter = 6e3,
  warmup = 5e3,
  seed = 12345,
  ndraws = 1e3,
  allow_new_levels = TRUE,
  control = list(
    adapt_delta = .99,
    max_treedepth = 15,
    step_size = 0.02
  ),
  stan_model_args = list(stanc_options = list("O1")),
  save_pars = save_pars(all = TRUE)
)

# Close Future Sessions
plan(sequential)

# Compare the two models
loo_compare(
  qsi_mirt_2pl_fit_mod2,
  qsi_mirt_2pl_fit_mod1,
  criterion = "kfold"
)

#------------------------------------------------------------------------------#
#-----------------------------Posterior Predictions-----------------------------
#------------------------------------------------------------------------------#

# CWSP Data
cwsp <- read_rds("output/project-data/cwsp_data.rds") %>% 
  # Drop one observation without a UCDP ID
  drop_na(cwsp_dyad_id) %>% 
  # Generate a row identfier
  mutate(row_id = 1:n())

# Generate unique combinations
cwsp_preds <- cwsp %>% 
  expand(
    actor = unique(new_id), 
    dimension = unique(qsi_mirt$dimension)
    )

# Generate posterior predictions for the observed data
post_preds <- cwsp_preds %>% 
  # Add expected draws
  add_linpred_draws(
    newdata = .,
    object = qsi_mirt_2pl_fit_mod2,
    allow_new_levels = TRUE,
    sample_new_levels = "gaussian"
  ) %>% 
  # Pivot to wide form
  pivot_wider(
    id_cols = c(actor, .draw), 
    names_from = dimension, 
    values_from = .linpred
  )  %>% 
  # Aggregate the predictions
  summarise(across(
    Social:Economic, 
    list(med = ~ median(.x), sd = ~ sd(.x))
  )) %>% 
  # Ungroup
  ungroup()

# Write the predictions to a file
write_rds(
  post_preds,
  "output/project-data/rebel_gov_index.rds",
  compress = "xz", 
  compression = 9L
)

#------------------------------------------------------------------------------#
#--------------------------Item Characteristic Curves---------------------------
#------------------------------------------------------------------------------#

# Draws for the Item-by-Dimension varying intercepts
logalpha_draws <- qsi_mirt_2pl_fit_mod2 %>% 
  # Tidy data frame of the parameters
  spread_draws(
    b_logalpha_Intercept, 
    `r_dimension:item__logalpha`[item,],
    ndraws = 3000,
    seed = 12345
  ) %>% 
  # Build item draws
  mutate(
    logalpha = b_logalpha_Intercept + `r_dimension:item__logalpha`,
    dimension = str_extract(item, "^[^_]*"), # Stripped down dimension names
    item = str_remove_all(item, "^[^_]*_")  # Item Name
    )

# Draws for the Item-specific intercepts
beta_draws <- qsi_mirt_2pl_fit_mod2 %>% 
  # Tidy data frame of the parameters
  spread_draws(
    b_beta_Intercept, 
    `r_item__beta`[item,],
    ndraws = 3000,
    seed = 12345
  ) %>% 
  # Build item draws
  mutate(beta = b_beta_Intercept + r_item__beta)


icc_uncertainty_post <- logalpha_draws %>%
  # Merge the two data frames
  left_join(beta_draws, by = c(".chain", ".iteration", ".draw", "item")) %>%
  # Add Item labels
  mutate(label = case_when(
    item == "pol_flag" ~ "Rebel Flag",
    item == "pol_orggov" ~ "Organize Government",
    item == "pol_elect" ~ "Elections",
    item == "pol_gov" ~ "Government",
    item == "pol_border" ~ "Border Patrol",
    item == "pol_id" ~ "Identification Documents",
    item == "pol_joinio" ~ "Join International Organization",
    item == "pol_embassy" ~ "Embassy",
    item == "pol_constitution" ~ "Constitution",
    item == "pol_media" ~ "Media",
    item == "pol_other" ~ "Other Political",
    item == "econ_currency" ~ "Currency",
    item == "econ_negresource" ~ "Negotiate Resource Rights",
    item == "econ_treaty" ~ "Economic Treaty",
    item == "econ_tax" ~ "Taxation",
    item == "econ_other" ~ "Other Economic",
    item == "soc_edu" ~ "Education",
    item == "soc_health" ~ "Health",
    item == "soc_infra" ~ "Infrastructure",
    item == "soc_trans" ~ "Public Transportation",
    item == "soc_law" ~ "Rule of Law",
    item == "soc_sec" ~ "Policing/Security",
    item == "soc_jus" ~ "Justice",
    item == "soc_house" ~ "Housing",
    item == "soc_aid" ~ "Welfare/Aid",
    item == "soc_other" ~ "Other Social"
  )) %>%
  # Expand by values of the eta
  expand_grid(eta = seq(from = -7, to = 7, length.out = 100)) %>%
  # Group by .draw, item, and dimension
  group_by(.draw, item, dimension) %>%
  # Inverse logit transform to get probabiity
  mutate(prob = inv_logit_scaled(exp(logalpha) * (beta + eta))) %>%
  # Ungroup
  ungroup()


icc_unertainty_econ <- icc_uncertainty_post %>% 
  # Filter economic items
  filter(dimension == "Economic") %>% 
  # Initiate a ggplot2 object
  ggplot(aes(x = eta, color = label, y = prob, group = interaction(label, .draw))) +
  # Add a line geom for each item
  geom_line(alpha = 0.05, show.legend = FALSE) +
  #
  facet_wrap(~ label, scales = "free_x") +
  # Use the Viridis color pallet
  scale_color_viridis_d(option = "H") +
  # Add labels to the plot
  labs(
    title = "Item Characteristic Curves for the Rebel Governance Index Economic Items",
    x = parse(text = "bold(theta[Economic])"),
    y = parse(text = "bold(Pr(QSI[j] * {phantom() == phantom()} * 1))")
  ) +
  # x axis scale tweaks
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 8),
    expand = c(0.01, 0)
    ) +
  # y axis scale tweaks
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 6),
    expand = c(0.01, 0)
  ) +
  # Custom plot theme settings
  plot_theme(
    xaxis_size = 30,
    yaxis_size = 25,
    title_size = 35,
    caption_size = 20,
    axis_text_size = 20,
    strip_face = "bold",
    strip_size = 25,
    y_axis_face = "bold",
    x_axis_face = "bold",
    plot.margin = margin(2, 2, 5, 5, "mm"),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    caption.hjust = 0, 
    caption.vjust = -1
  )

# Write the plots to a folder
ggsave(
  filename = "Figure_2.3_Rebel_Governance_ICC_Faceted_social.jpeg",
  plot = icc_unertainty_econ,
  device = "jpeg",
  width = 25,
  height = 15,
  units = "in",
  dpi = "retina",
  limitsize = FALSE
)

icc_unertainty_political <- icc_uncertainty_post %>% 
  # Filter economic items
  filter(dimension == "Political") %>% 
  # Initiate a ggplot2 object
  ggplot(aes(x = eta, color = label, y = prob, group = interaction(label, .draw))) +
  # Add a line geom for each item
  geom_line(alpha = 0.05, show.legend = FALSE) +
  #
  facet_wrap(~ label, scales = "free_x") +
  # Use the Viridis color pallet
  scale_color_viridis_d(option = "H") +
  # Add labels to the plot
  labs(
    title = "Item Characteristic Curves for the Rebel Governance Index Political Items",
    x = parse(text = "bold(theta[Political])"),
    y = parse(text = "bold(Pr(QSI[j] * {phantom() == phantom()} * 1))")
  ) +
  # x axis scale tweaks
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 8),
    expand = c(0.01, 0)
  ) +
  # y axis scale tweaks
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 6),
    expand = c(0.01, 0)
  ) +
  # Custom plot theme settings
  plot_theme(
    xaxis_size = 30,
    yaxis_size = 25,
    title_size = 35,
    caption_size = 20,
    axis_text_size = 20,
    strip_face = "bold",
    strip_size = 25,
    y_axis_face = "bold",
    x_axis_face = "bold",
    plot.margin = margin(2, 2, 5, 5, "mm"),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    caption.hjust = 0, 
    caption.vjust = -1
  )

# Write the plots to a folder
ggsave(
  filename = "Figure_2.4_Rebel_Governance_ICC_Faceted_social.jpeg",
  plot = icc_unertainty_political,
  device = "jpeg",
  width = 25,
  height = 15,
  units = "in",
  dpi = "retina",
  limitsize = FALSE
)

icc_unertainty_social <- icc_uncertainty_post %>% 
  # Filter economic items
  filter(dimension == "Social") %>% 
  # Initiate a ggplot2 object
  ggplot(aes(x = eta, color = label, y = prob, group = interaction(label, .draw))) +
  # Add a line geom for each item
  geom_line(alpha = 0.05, show.legend = FALSE) +
  #
  facet_wrap(~ label, scales = "free_x") +
  # Use the Viridis color pallet
  scale_color_viridis_d(option = "H") +
  # Add labels to the plot
  labs(
    title = "Item Characteristic Curves for the Rebel Governance Index Social Items",
    x = parse(text = "bold(theta[Social])"),
    y = parse(text = "bold(Pr(QSI[j] * {phantom() == phantom()} * 1))")
  ) +
  # x axis scale tweaks
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 8),
    expand = c(0.01, 0)
  ) +
  # y axis scale tweaks
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 6),
    expand = c(0.01, 0)
  ) +
  # Custom plot theme settings
  plot_theme(
    xaxis_size = 30,
    yaxis_size = 25,
    title_size = 35,
    caption_size = 20,
    axis_text_size = 20,
    strip_face = "bold",
    strip_size = 25,
    y_axis_face = "bold",
    x_axis_face = "bold",
    plot.margin = margin(2, 2, 5, 5, "mm"),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    caption.hjust = 0, 
    caption.vjust = -1
  )

# Write the plots to a folder
ggsave(
  filename = "Figure_2.5_Rebel_Governance_ICC_Faceted_social.jpeg",
  plot = icc_unertainty_social,
  device = "jpeg",
  width = 25,
  height = 15,
  units = "in",
  dpi = "retina",
  limitsize = FALSE
)
