# ------------------------------------
# load packages and data             #
# ------------------------------------

set.seed(052319)

# load packages
packages = c("tidyverse", "janitor", "psych", "devtools", "PAutilities", "measurements")
lapply(packages, library, character.only = TRUE)
rm(packages)

#simulate toy dataset to write code
source("scripts/create_toy.R")

keys = read.csv("data/superKey.csv", header = TRUE, row.names = 1)

# super key -- this contains the master key list for all of SAPA. every item ever administered and every scale you can score
# each row is a single item
# each column is a scale
# the value of a cell is 0 if that item is not part of that scale, 1 if that item positively loads on the scale, and -1 if the item negatively loads on the scale

# -----------------------------------
# filter by age                     #
# -----------------------------------

# remove participants who are 18 years or older
sapa = sapa %>%
  filter(age < 18) %>%
  filter(!is.na(gender)) %>%
  filter(!is.na(height)) %>%
  filter(!is.na(weight)) 

# -----------------------------------
# score SES                         #
# -----------------------------------

#or years
sapa = sapa %>%
  mutate(p1edu = case_when(
  p1edu == "less12yrs" ~ "6", 
  p1edu == "HSgrad" ~ "12", 
  p1edu == "SomeCollege" ~ "14", 
  p1edu == "CurrentInUniv" ~ "14", 
  p1edu == "CollegeDegree" ~ "16", 
  p1edu == "InGradOrProSchool" ~ "18", 
  p1edu == "GradOrProDegree" ~ "20")) 

sapa = sapa %>%
  mutate(p2edu = case_when(
    p2edu == "less12yrs" ~ "6", 
    p2edu == "HSgrad" ~ "12", 
    p2edu == "SomeCollege" ~ "14", 
    p2edu == "CurrentInUniv" ~ "14", 
    p2edu == "CollegeDegree" ~ "16", 
    p2edu == "InGradOrProSchool" ~ "18", 
    p2edu == "GradOrProDegree" ~ "20")) 

sapa$p1edu = as.numeric(sapa$p1edu)
sapa$p2edu = as.numeric(sapa$p2edu)

sapa = sapa %>%
  filter(!is.na(p1edu) | !is.na(p2edu) |
           !is.na(p1occIncomeEst) | !is.na(p2occIncomeEst) |
           !is.na(p1occPrestige) | !is.na(p2occPrestige))

#estimate SES composite

sapa = sapa %>%
  mutate(z.p1edu = scale(p1edu),
         z.p2edu = scale(p2edu),
         z.p1occIncomeEst = scale(p1occIncomeEst),
         z.p2occIncomeEst = scale(p2occIncomeEst),
         z.p2occPrestige = scale(p1occPrestige),
         z.p2occPrestige = scale(p2occPrestige)) 

sapa$ses = rowMeans(sapa[,grepl("^z\\.", names(sapa))], na.rm=T)

sapa = sapa %>%
  dplyr::select(-starts_with("z"))

# -----------------------------------
# score 27 and 5 personality factors (IRT)#
# -----------------------------------

# select just the rows that correspond to variables in the current SAPA dataset
keys = keys[names(sapa), ]

# select just the scales that are scored using the SPI_135 form
keys = keys %>%
  select(contains("SPI_135"))

# score the items (this contains item and scale statistics too!)
scored = scoreItems(keys, sapa)

# add scores to SAPA
sapa = cbind(sapa, scored$scores)

# remove individual items
sapa = sapa %>%
  select(-contains("q_"))
# -------------------------------------------------------------------------
# calculate BMI zscore, percentile, and category based on CDC guidelines  #
# -------------------------------------------------------------------------

# z-scores come from PAutilities package, developed by WHO Multicentre Growth Reference Study (MGRS)
# information about the development of these reference standards can be found at
# https://www.cdc.gov/obesity/childhood/defining.html

# The 2000 CDC growth charts that are used to calculate BMI were developed with data from 5 national 
# health examination surveys that occurred from 1963 to 1994 and supplemental data from surveys that 
# occurred from 1960 to 1995.
# Kuczmarski RJ, Ogden CL, Guo SS, et al. 2000 CDC growth charts for the United States: methods and development. 
# National Center for Health Statistics. Vital Health Stat 11. 2002;(246):1-190

sapa = sapa %>%
  mutate(sex = ifelse(gender == "male", "M", "F"),
         weight = conv_unit(weight, from = "lbs", to = "kg"),
         height = conv_unit(height, from = "inch", to = "cm"))

for(i in 1:nrow(sapa)){
  sapa$BMI_p[i] = get_BMI_percentile(weight_kg = sapa$weight[i], 
                                     height = sapa$height[i], 
                                     age_yrs = sapa$age[i], 
                                     sex = sapa$sex[i],
                                     output = "percentile")
  sapa$BMI_c[i] = as.character(
    get_BMI_percentile(weight_kg = sapa$weight[i], 
                       height = sapa$height[i], 
                       age_yrs = sapa$age[i], 
                       sex = sapa$sex[i],
                       output = "class"))
}


# -----------------------------------
# split by gender                   #
# -----------------------------------

sapa = sapa %>%
  select(gender, BMI, BMI_p, BMI_c, p1edu, p1occPrestige, p1occIncomeEst, p2edu, 
         p2occPrestige, p2occIncomeEst, ses, cog, contains("SPI"))

sapa_male = sapa %>%
  filter(gender == "male") %>%
  dplyr::select(-gender) 

sapa_female = sapa %>%
  filter(gender == "female") %>%
  dplyr::select(-gender)

save(sapa, sapa_male, sapa_female, scored, file = "data/cleaned.Rdata")

