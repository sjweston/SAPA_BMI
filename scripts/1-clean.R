# ------------------------------------
# load packages and data             #
# ------------------------------------

set.seed(052319)

# load packages
packages = c("tidyverse", "janitor", "psych", "devtools", "PAutilities", "measurements", "here")
lapply(packages, library, character.only = TRUE)
rm(packages)

#read in data
load(here("../../data/SAPA/collaboration/SAPAdata07feb2017thru22jul2019forSara2.rdata"))
sapa = SAPAdata07feb2017thru22jul2019x

source(here("scripts/personality_scales.R"))
keys = read.csv("data/superKey.csv", header = TRUE, row.names = 1)
#remove if 

# super key -- this contains the master key list for all of SAPA. every item ever administered and every scale you can score
# each row is a single item
# each column is a scale
# the value of a cell is 0 if that item is not part of that scale, 1 if that item positively loads on the scale, and -1 if the item negatively loads on the scale

# -----------------------------------
# filter by age                     #
# -----------------------------------

# remove participants who are 18 years or older and from the US
sapa = sapa %>%
  filter(age < 18) %>%
  filter(country == "USA") %>%
  filter(!is.na(sex)) %>%
  filter(!is.na(height)) %>%
  filter(!is.na(weight))
# -----------------------------------
# score SES                         #
# -----------------------------------

# make sure occupational variables are numeric
sapa = sapa %>%
  mutate_at(vars(matches("^(p)\\d(occ)")), as.numeric)

#or years
sapa = sapa %>%
  mutate(p1edu = case_when(
  p1edu == "less12yrs" ~ "6", 
  p1edu == "HSgrad" ~ "12", 
  p1edu == "SomeCollege" ~ "14", 
  p1edu == "CurrentInUniv" ~ "14", 
  p1edu == "AssociateDegree" ~ "14", 
  p1edu == "CollegeDegree" ~ "16", 
  p1edu == "InGradOrProSchool" ~ "18", 
  p1edu == "GradOrProDegree" ~ "20")) 

sapa = sapa %>%
  mutate(p2edu = case_when(
    p2edu == "less12yrs" ~ "6", 
    p2edu == "HSgrad" ~ "12", 
    p2edu == "SomeCollege" ~ "14", 
    p2edu == "CurrentInUniv" ~ "14",   
    p2edu == "AssociateDegree" ~ "14", 
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

# ------------------------------------------
# score 5 personality factors (sum scores) #
# ------------------------------------------

# select just the rows that correspond to variables in the current SAPA dataset
vars = names(sapa)
keys = keys[rownames(keys) %in% vars, ]

# select just the Big 5 scales that are scored using the SPI_135 form 
bfkeys = keys %>%
  select(contains("SPI_135")) %>%
  select(1:5)

# score the items (this contains item and scale statistics too!)
scored = scoreItems(keys = bfkeys, items = sapa)
b5scored = scoreItems(keys = spi.keys, items = sapa)

# add scores to SAPA
b5scores = as.data.frame(b5scored$scores[,1:5])
names(b5scores) = paste0("SPI_135_27_5_", names(b5scores))
sapa = cbind(sapa, b5scores)



# -------------------------------------------
# score 27 personality factors (IRT scores) #
# -------------------------------------------

load("../../data/SAPA/IRTinfoSPI27.rdata")

# #reverse score items that should be negatively keyed
# spi_keys = keys %>%
#   select(contains("SPI_135"))%>%
#   select(6:32) %>%
#   mutate(item = rownames(.)) %>%
#   gather("scale", "key", -item) %>%
#   filter(key != 0)
# 
# #confirm each item is only in dataset once
# length(unique(spi_keys$item)) == nrow(spi_keys)
# 
# reverse = spi_keys %>%
#   filter(key == -1) 
# reverse = reverse[,"item"]
# reverse_df = apply(sapa[, reverse], 2, function(x) max(x, na.rm=T) + 1 - x)
# sapa[,reverse] = reverse_df



# IRT score
dataSet <- subset(sapa, select = c(orderForItems))

SPIirtScores <- matrix(nrow=dim(dataSet)[1], ncol=27)

scaleNames = gsub("SPI27_", "", names(IRToutputSPI27))
spi_keys = keys %>%
  select(matches("SPI_135")) %>%
  select(-c(1:5)) %>%
  mutate(item = rownames(.)) %>%
  gather("scale", "key", -item) %>%
  filter(key != 0)

for (i in 1:length(IRToutputSPI27)) {
  data <- subset(dataSet, select = c(rownames(IRToutputSPI27[[i]]$irt$difficulty[[1]])))
  calibrations <- IRToutputSPI27[[i]]
  #check calibration direction
  loadings = calibrations$fa$loadings[,1]
  loadings = ifelse(loadings < 0, -1, 1)
  loadings = data.frame(item = names(loadings), loadings = loadings)
  keys_direction = spi_keys %>%
    filter(grepl(scaleNames[i], scale)) %>%
    full_join(loadings)
  same = sum(keys_direction$key == keys_direction$loadings)
  if(same == 0) data[,1:ncol(data)] = apply(data[,1:ncol(data)], 2, function(x) max(x, na.rm=T) + 1 - x)
  if (same > 0 & same < 5) print("Error in loadings")
  scored <- scoreIrt(calibrations, data, keys = NULL, cut = 0)
  SPIirtScores[,i] <- scored$theta1
}

SPIirtScores <- as.data.frame(SPIirtScores)
colnames(SPIirtScores) <- paste0("SPI_135_27_5_", scaleNames)

#add to sapa dataset
sapa = cbind(sapa, SPIirtScores)

# sc <- scoreItems(spi.keys,sapa)
# sc = as.data.frame(sc$scores)
# 
# colnames(sc) = paste0("SPI_135_27_5_", names(sc))
# 
# sapa = cbind(sapa, sc[,6:32])


# -------------------------------------------
# score ICAR cognition scores (IRT scores) #
# -------------------------------------------
load("../../data/SAPA/IRTinfoICAR.rdata")

# IRT score
dataSet <- subset(sapa, select = c(orderForItems))
ICARirtScores <- matrix(nrow=dim(dataSet)[1], ncol=5)
ICARirtSEs <- matrix(nrow=dim(dataSet)[1], ncol=5)

for (i in 1:length(IRToutputICAR)) {
  data <- subset(dataSet, select = c(names(IRToutputICAR[[i]]$irt$difficulty[[1]])))
  calibrations <- IRToutputICAR[[i]]
  scored <- scoreIrt(calibrations, data, keys = NULL, cut = 0)
  irt.data <- irt.se(calibrations, score = as.matrix(scored[,1]))
  TScoring <- (irt.data[,"scores"]-thetaNormsMeans[i])/thetaNormsSDs[i]
  TScores <- TScoring*10+50
  ICARirtScores[,i] <- TScores
  TScoreSEs <- irt.data[,"se"]*10
  ICARirtSEs[,i] <- TScoreSEs
  rm(TScores, TScoring, TScoreSEs, scored, calibrations, data)
}
ICARirtScores <- as.data.frame(ICARirtScores)
colnames(ICARirtScores) <- scaleNames
ICARirtSEs <- as.data.frame(ICARirtSEs)
colnames(ICARirtSEs) <- scaleNames
rm(IRToutputICAR, thetaNormsMeans, thetaNormsSDs, scaleNames)

#add to sapa dataset
sapa = cbind(sapa, ICARirtScores)
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
  filter(sex != "other") %>%
  mutate(sex = as.factor(as.character(sex))) %>%
  mutate(sex2 = ifelse(sex == "male", "M", "F"),
         weight = conv_unit(weight, from = "lbs", to = "kg"),
         height = conv_unit(height, from = "inch", to = "cm"))

for(i in 1:nrow(sapa)){
  sapa$BMI_p[i] = get_BMI_percentile(weight_kg = sapa$weight[i], 
                                     height = sapa$height[i], 
                                     age_yrs = sapa$age[i], 
                                     sex = sapa$sex2[i],
                                     output = "percentile")
  sapa$BMI_c[i] = as.character(
    get_BMI_percentile(weight_kg = sapa$weight[i], 
                       height = sapa$height[i], 
                       age_yrs = sapa$age[i], 
                       sex = sapa$sex2[i],
                       output = "class"))
}


# -----------------------------------
# split by gender                   #
# -----------------------------------

sapa = sapa %>%
  mutate(cog = ICAR60) %>%
  select(sex, BMI, BMI_p, BMI_c, p1edu, 
         p1occPrestige, p1occIncomeEst, p2edu, 
         p2occPrestige, p2occIncomeEst, ses, cog, contains("SPI"))

sapa_male = sapa %>%
  filter(sex == "male") %>%
  dplyr::select(-sex) 

sapa_female = sapa %>%
  filter(sex == "female") %>%
  dplyr::select(-sex)

save(b5scored, file = "data/alpha.Rdata")
save(sapa, sapa_male, sapa_female, file = "data/cleaned.Rdata")

