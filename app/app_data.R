packages = c("tidyverse", "knitr", "kableExtra", "papaja")
lapply(packages, library, character.only = TRUE)
rm(packages)

source("scripts/personality_scales.R")

load("data/regression_output.Rdata")
load("data/regression_plots.Rdata")
load("data/cleaned.Rdata")

female_reg = female_reg %>%
  mutate(gender = "female")

male_reg = male_reg %>%
  mutate(gender = "male")

table = female_reg %>%
  full_join(male_reg) %>%
  mutate(term = factor(term, 
                       levels = c("(Intercept)", "edu", "income", 
                                  "trait_score", "trait_score:edu", "trait_score:income"),
                       labels = c("Intercept",  "Education", "Income", 
                                  "Personality", "Personality x Education", "Personality x Income")))

save(all_names, table, 
     sapa_male, sapa_female,
     female_plot, male_plot,
     female_reg, male_reg, 
     file = "data/app_data.Rdata")
