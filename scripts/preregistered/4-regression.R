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


sapa_male_trait = sapa_male %>%
  dplyr::select(-starts_with("p1"), -starts_with("p2")) %>%
  mutate(ses = scale(ses)) %>%
  mutate(cog = scale(cog)) %>%
  mutate(BMI_p = scale(BMI_p)) %>%
  gather("trait_name", "trait_score", -ses, -BMI_p, -BMI, -BMI_c) %>%
  group_by(trait_name) %>%
  mutate(trait_score = scale(trait_score)) %>%
  nest()

sapa_female_trait = sapa_female %>%
  dplyr::select(-starts_with("p1"), -starts_with("p2")) %>%
  mutate(ses = scale(ses)) %>%
  mutate(cog = scale(cog)) %>%
  mutate(BMI_p = scale(BMI_p)) %>%
  gather("trait_name", "trait_score", -ses, -BMI_p, -BMI, -BMI_c) %>%
  group_by(trait_name) %>%
  mutate(trait_score = scale(trait_score)) %>%
  nest()

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
  unnest()


# ----------------------------------------------
# bootstrap confidence intervals (males)       #
# ----------------------------------------------

male_boot = sapa_male_trait %>%
  mutate(samples = map(data, bootstraps, times = boot.n)) %>%
  dplyr::select(-data) %>%
  unnest(samples) %>%
  mutate(boot_cov = map(splits, ~broom::tidy(lm(BMI_p ~ trait_score + ses, analysis(.))))) %>%
  mutate(boot_int = map(splits, ~broom::tidy(lm(BMI_p ~ trait_score*ses, analysis(.))))) 

male_boot = male_boot %>%
  dplyr::select(-splits, -id) %>%
  gather("model", "summary", -trait_name) %>%
  unnest()

male_boot = male_boot %>% 
  group_by(trait_name, model, term) %>%
  summarise(conf.low = quantile(estimate, probs = .025),
            conf.high = quantile(estimate, probs = .975)) %>%
  ungroup() %>%
  mutate(model = gsub("boot_", "", model)) 

male_reg = male_reg %>%
  full_join(male_boot)


# ------------------------------------
# regression iteration (females)       #
# ------------------------------------

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
  unnest()


# ----------------------------------------------
# bootstrap confidence intervals (females)       #
# ----------------------------------------------

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

# --------------------------
#     save outputs         #
# --------------------------

save(male_reg, female_reg, file = "data/regression_output.Rdata")
save(male_plot, female_plot, file = "data/regression_plots.Rdata")
