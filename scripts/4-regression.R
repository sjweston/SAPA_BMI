# ------------------------------------
# load packages and data             #
# ------------------------------------

#load packages
packages = c("tidyverse", "broom", "sjPlot", "rsample")
lapply(packages, library, character.only = TRUE)
rm(packages)

load("data/cleaned.Rdata")

# ------------------------------------
#     set up bootstraps              #
# ------------------------------------

# number of bootstrap samples
boot.n = 10000

# ------------------------------------
# wrangle data for iteration         #
# ------------------------------------

#end goal of wrangling is a data frame of data frames
# nested dataframes correspond to a single personality trait
# score refers to a participant's score on that trait
# we also standardize each of our variables within gender


sapa_male_trait = sapa_male[train_male, ] %>%
  dplyr::select(-starts_with("p1"), -starts_with("p2")) %>%
  mutate(set = ifelse(row_number() %in% train_male[,1], "train", "test")) %>% #identify which rows in test and training
  gather("trait_name", "trait_score", -ses, -BMI_c, -BMI, -BMI_p, -set) %>% # gather all personality variables
  group_by(trait_name, set) %>% # group by trait and also by whether in test/train
  mutate(trait_score = scale(trait_score)) %>% #standardize
  mutate(ses = scale(ses)) %>% #standardize
  mutate(BMI_p = scale(BMI_p)) %>% #standardize
  ungroup() %>% group_by(trait_name) %>%  #group only by trait
  nest() #nest data frames


sapa_female_trait = sapa_female[train_female, ] %>%
  dplyr::select(-starts_with("p1"), -starts_with("p2")) %>%
  mutate(set = ifelse(row_number() %in% train_male[,1], "train", "test")) %>% #identify which rows in test and training
  gather("trait_name", "trait_score", -ses, -BMI_c, -BMI, -BMI_p, -set) %>% # gather all personality variables
  group_by(trait_name, set) %>% # group by trait and also by whether in test/train
  mutate(trait_score = scale(trait_score)) %>% #standardize
  mutate(ses = scale(ses)) %>% #standardize
  mutate(BMI_p = scale(BMI_p)) %>% #standardize
  ungroup() %>% group_by(trait_name) %>%  #group only by trait
  nest() #nest data frames

# ------------------------------------
# regression iteration (males)       #
# ------------------------------------

male_reg = sapa_male_trait %>%
  mutate(cov = map(data, ~lm(BMI_p ~ trait_score + ses, data = .))) %>%
  mutate(int = map(data, ~lm(BMI_p ~ trait_score*ses, data = .))) 

male_plot = male_reg %>%
  mutate(cov = map(data, ~lm(BMI_p ~ trait_score + ses, data = .))) %>%
  mutate(int = map(data, ~lm(BMI_p ~ trait_score*ses, data = .))) 


male_reg = male_reg %>%
  dplyr::select(-data) %>%
  gather("model", "output", cov, int) %>%
  mutate(output = map(output, broom::tidy, conf.int = FALSE)) %>%
  unnest(cols = c(output))


# ----------------------------------------------
# bootstrap confidence intervals (males)       #
# ----------------------------------------------

set.seed(031720)

male_boot = sapa_male_trait %>%
  mutate(samples = map(data, bootstraps, times = boot.n)) %>%
  dplyr::select(-data) %>%
  unnest(samples) %>%
  mutate(boot_cov = map(splits, ~broom::tidy(lm(BMI_p ~ trait_score + ses, analysis(.))))) %>%
  mutate(boot_int = map(splits, ~broom::tidy(lm(BMI_p ~ trait_score*ses, analysis(.))))) 

male_boot = male_boot %>%
  dplyr::select(-splits, -id) %>%
  gather("model", "summary", -trait_name) %>%
  unnest(cols = c(summary))

male_boot = male_boot %>% 
  group_by(trait_name, model, term) %>%
  summarise(conf.low = quantile(estimate, probs = .025),
            conf.high = quantile(estimate, probs = .975)) %>%
  ungroup() %>%
  mutate(model = gsub("boot_", "", model)) 

male_reg = male_reg %>%
  full_join(male_boot)

save(male_reg, male_plot, file = "data/regression_output_male.Rdata")

# ------------------------------------
# regression iteration (females)       #
# ------------------------------------

# we run the models for men and women separately because R kept crashing whey trying to run this whole script.2

female_reg = sapa_female_trait %>%
  mutate(cov = map(data, ~lm(BMI_p ~ trait_score + ses, data = .))) %>%
  mutate(int = map(data, ~lm(BMI_p ~ trait_score*ses, data = .))) 

female_plot = female_reg %>%
  mutate(cov = map(data, ~lm(BMI_p ~ trait_score + ses, data = .))) %>%
  mutate(int = map(data, ~lm(BMI_p ~ trait_score*ses, data = .))) 


female_reg = female_reg %>%
  dplyr::select(-data) %>%
  gather("model", "output", cov, int) %>%
  mutate(output = map(output, broom::tidy, conf.int = FALSE)) %>%
  unnest(cols = c(output))


# ----------------------------------------------
# bootstrap confidence intervals (females)       #
# ----------------------------------------------

set.seed(031720)

female_boot = sapa_female_trait %>%
  mutate(samples = map(data, bootstraps, times = boot.n)) %>%
  dplyr::select(-data) %>%
  unnest(samples) %>%
  mutate(boot_cov = map(splits, ~broom::tidy(lm(BMI_p ~ trait_score + ses, analysis(.))))) %>%
  mutate(boot_int = map(splits, ~broom::tidy(lm(BMI_p ~ trait_score*ses, analysis(.))))) 

female_boot = female_boot %>%
  dplyr::select(-splits, -id) %>%
  gather("model", "summary", -trait_name) %>%
  unnest()

female_boot = female_boot %>% 
  group_by(trait_name, model, term) %>%
  summarise(conf.low = quantile(estimate, probs = .025),
            conf.high = quantile(estimate, probs = .975)) %>%
  ungroup() %>%
  mutate(model = gsub("boot_", "", model)) 

female_reg = female_reg %>%
  full_join(female_boot)

save(female_reg, female_plot, file = "data/regression_output_male.Rdata")

# --------------------------
#     save outputs         #
# --------------------------

#save(male_reg, female_reg, file = "data/regression_output.Rdata")
#save(male_plot, female_plot, file = "data/regression_plots.Rdata")
