
# ----- load packages ----


packages = c("tidyverse", "knitr", "kableExtra", "papaja", "here")
lapply(packages, library, character.only = TRUE)
rm(packages)

source("scripts/personality_scales.R")

names(SPI_27_names) = gsub("135_27_5_", "", names(SPI_27_names))
names(SPI_5_names) = gsub("135_27_5_", "", names(SPI_5_names))

load("data/logistic_tidymodels_output.Rdata")


# additive models ---------------------------------------------------------

sapa_male_final %>%
  filter(model == "add") %>%
  select(trait_name, finalfit)
