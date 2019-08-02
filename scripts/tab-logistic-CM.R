# ------------------------------------
# load packages                      #
# ------------------------------------

packages = c("tidyverse", "knitr", "kableExtra", "papaja")
lapply(packages, library, character.only = TRUE)
rm(packages)

source("scripts/personality_scales.R")



# ------------------------------------
# Table. Regression model            #
# ------------------------------------

load("data/logistic_output.Rdata")

female_log = female_log %>%
  filter(model == "int") %>%
  mutate(byclass = map(confusion, "byClass")) %>%
  mutate(byclass = map(byclass, as.data.frame)) %>%
  mutate(byclass = map(byclass, .f = function(x) mutate(x, category = rownames(x)))) %>%
  dplyr::select(trait_name, byclass) %>%
  unnest() %>%
  mutate(gender = "female")

male_log = male_log %>%
  filter(model == "int") %>%
  mutate(byclass = map(confusion, "byClass")) %>%
  mutate(byclass = map(byclass, as.data.frame)) %>%
  mutate(byclass = map(byclass, .f = function(x) mutate(x, category = rownames(x)))) %>%
  dplyr::select(trait_name, byclass) %>%
  unnest() %>%
  mutate(gender = "male")

female_log %>%
  full_join(male_log) %>%
  dplyr::select(trait_name, gender, category, Sensitivity, Specificity) %>%
  mutate(category = gsub("Class: ", "", category)) %>%
  gather("key", "value", Sensitivity, Specificity) %>%
  unite(category, category, key) %>%
  spread(category, value) %>%
  mutate(trait_name = factor(trait_name, levels = c("cog", names(SPI_27_names), names(SPI_5_names)))) %>% 
  arrange(gender, trait_name) %>% 
  mutate(trait_name = rep(c("Cognitive Ability", SPI_27_names, SPI_5_names),times = 2)) %>%
  dplyr::select(-gender) %>%
  kable(., digits = 2, col.names = rep(c("Trait", rep(c("Sensitivitiy", "Specificity"), 4)))) %>%
  kable_styling() %>%
  add_header_above(c(" ", "Normal" = 2, "Obese" = 2, "Overweight" = 2, "Underweight" = 2)) %>%
  group_rows("Female", 1, 33) %>%
  group_rows("Male", 34, 66)
  