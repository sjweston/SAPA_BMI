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

sapa = cbind(sapa, score05$scores)

sapa = sapa %>%
  select(-contains("q_"))

# -----------------------------------
# score SES                         #
# -----------------------------------
sapa$p1edu = as.numeric(sapa$p1edu)
sapa$p2edu = as.numeric(sapa$p2edu)

#or years
sapa = sapa %>%
  mutate(p1edu = case_when(
  p1edu == 1 ~ 6, 
  p1edu == 2 ~ 12, 
  p1edu == 3 ~ 14, 
  p1edu == 4 ~ 14, 
  p1edu == 5 ~ 16, 
  p1edu == 6 ~ 18, 
  p1edu == 7 ~ 18)) 

sapa = sapa %>%
  mutate(p2edu = case_when(
    p2edu == 1 ~ 6, 
    p2edu == 2 ~ 12, 
    p2edu == 3 ~ 14, 
    p2edu == 4 ~ 14, 
    p2edu == 5 ~ 16, 
    p2edu == 6 ~ 18, 
    p2edu == 7 ~ 18)) 

sapa$edu = rowMeans(sapa[,c("p1edu", "p2edu")], na.rm=T)
sapa$income = rowMeans(sapa[,c("p1occIncomeEst","p2occIncomeEst")], na.rm = T)

# -----------------------------------
# split by gender                   #
# -----------------------------------

sapa = sapa %>%
  select(gender, BMI, edu, income, cog, contains("spi"))

sapa_male = sapa %>%
  filter(gender == "male") %>%
  dplyr::select(-gender)

sapa_female = sapa %>%
  filter(gender == "female") %>%
  dplyr::select(-gender)

save(sapa, sapa_male, sapa_female, file = "data/cleaned.Rdata")

