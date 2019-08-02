# ------------------------------------
# load packages                      #
# ------------------------------------

packages = c("tidyverse", "knitr", "kableExtra", "papaja")
lapply(packages, library, character.only = TRUE)
rm(packages)

source("scripts/personality_scales.R")
# ------------------------------------
# Table. Descriptives                #
# ------------------------------------

load("data/descriptives.Rdata")

des = data.frame(gender = c("male", "female"))
des$df = descriptives

levels = c("BMI", "Parental Education", "Parental Income", "Cognitive Ability",
           SPI_27_names, SPI_5_names)

des %>%
  unnest() %>%
  mutate(vars = rep(c("gender", "BMI", "Parental Education", "Parental Income", 
                      "Cognitive Ability", SPI_27_names, SPI_5_names),2)) %>%
  dplyr::select(gender, vars, n, mean, sd, min, max) %>%
  rename(x1_n = n, 
         x2_mean = mean, 
         x3_sd = sd, 
         x4_min = min, 
         x5_max = max) %>%
  filter(vars != "gender") %>%
  mutate(vars = factor(vars, levels = levels)) %>%
  gather("key", "value", -gender, -vars) %>%
  unite(key, gender, key) %>%
  spread(key, value) %>%
  kable(., digits = 2,
        col.names = c("Variable", rep(c("N", "Mean", "SD", "Min", "Max"),2))) %>%
  kable_styling() %>%
  add_header_above(c(" ", "Female" = 5, "Male" = 5))
