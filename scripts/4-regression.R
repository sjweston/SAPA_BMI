# ------------------------------------
# load packages and data             #
# ------------------------------------

#load packages
packages = c("tidyverse", "broom")
lapply(packages, library, character.only = TRUE)
rm(packages)

load("data/cleaned.Rdata")

# ------------------------------------
# wrangle data for iteration         #
# ------------------------------------

#end goal of wrangling is a data frame of data frames
# nested dataframes correspond to a single personality trait
# score refers to a participant's score on that trait
# we also standardize each of our variables within gender


sapa_male_trait = sapa_male %>%
  mutate(edu = scale(edu)) %>%
  mutate(income = scale(income)) %>%
  mutate(cog = scale(cog)) %>%
  mutate(BMI = scale(BMI)) %>%
  gather("trait_name", "trait_score", -income, -edu, -cog, -BMI) %>%
  group_by(trait_name) %>%
  mutate(trait_score = scale(trait_score)) %>%
  nest()

sapa_female_trait = sapa_female %>%
  mutate(edu = scale(edu)) %>%
  mutate(income = scale(income)) %>%
  mutate(BMI = scale(BMI)) %>%
  gather("trait_name", "trait_score", -income, -edu, -cog, -BMI) %>%
  group_by(trait_name) %>%
  mutate(trait_score = scale(trait_score)) %>%
  nest()

# ------------------------------------
# regression iteration (males)       #
# ------------------------------------

male_reg = sapa_male_trait %>%
  mutate(cov_edu = map(data, ~lm(BMI ~ trait_score + cog + edu, data = .))) %>%
  mutate(ses_edu = map(data, ~lm(BMI ~ trait_score*edu + cog, data = .))) %>%
  mutate(cov_inc = map(data, ~lm(BMI ~ trait_score + cog + income, data = .))) %>%
  mutate(ses_inc = map(data, ~lm(BMI ~ trait_score*income + cog, data = .))) %>%
  dplyr::select(-data) %>%
  gather("model", "output", cov_edu, ses_edu, cov_inc, ses_inc) %>%
  mutate(output = map(output, broom::tidy, conf.int = TRUE)) %>%
  unnest()

# ------------------------------------
# regression iteration (females)       #
# ------------------------------------

female_reg = sapa_female_trait %>%
  mutate(cov_edu = map(data, ~lm(BMI ~ trait_score + cog + edu, data = .))) %>%
  mutate(ses_edu = map(data, ~lm(BMI ~ trait_score*edu + cog, data = .))) %>%
  mutate(cov_inc = map(data, ~lm(BMI ~ trait_score + cog + income, data = .))) %>%
  mutate(ses_inc = map(data, ~lm(BMI ~ trait_score*income + cog, data = .))) %>%
  dplyr::select(-data) %>%
  gather("model", "output", cov_edu, ses_edu, cov_inc, ses_inc) %>%
  mutate(output = map(output, broom::tidy, conf.int = TRUE)) %>%
  unnest()

save(male_reg, female_reg, file = "data/regression_output.Rdata")
