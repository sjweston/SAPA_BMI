# ------------------------------------
# load packages and data             #
# ------------------------------------

set.seed(052319)

#load packages
packages = c("tidyverse", "janitor", "psych", "devtools")
lapply(packages, library, character.only = TRUE)
rm(packages)

load("toy data/toydata.Rdata")

# -----------------------------------
# filter by age                     #
# -----------------------------------

sapa = sapa %>%
  filter(age < 18) %>%
  filter(!is.na(BMI))

# -----------------------------------
# score 27 personality factors (IRT)#
# -----------------------------------

source("scripts/personality_scales.R")
names(SPI_27_list) <- paste0("spi27_", str_pad(1:27, 2, "0", side = "left"))
keys27 = make.keys(names(sapa),keys.list = SPI_27_list)
score27 = scoreItems(keys27, sapa)

sapa = cbind(sapa, score27$scores)

# -----------------------------------
# score 5 personality factors (CTT) #
# -----------------------------------

names(SPI_5_list) <- paste0("spi5_", 1:5)
keys05 = make.keys(names(sapa),keys.list = SPI_5_list)
score05 = scoreItems(keys05, sapa)

sapa = cbind(sapa, score5$scores)

sapa = sapa %>%
  select(-contains("q_"))

# -----------------------------------
# score SES                         #
# -----------------------------------

#DC to provide guideance

sapa$ses = sapa$occPrestige

sapa = sapa %>%
  select(gender, BMI, ses, cog, contains("spi"))

sapa_male = sapa %>%
  filter(gender == "male") %>%
  dplyr::select(-gender)

sapa_female = sapa %>%
  filter(gender == "female") %>%
  dplyr::select(-gender)

save(sapa, sapa_male, sapa_female, file = "data/cleaned.Rdata")
