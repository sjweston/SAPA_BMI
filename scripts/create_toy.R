# load packages and data             #
# ------------------------------------
set.seed(052319)

#load packages
packages = c("tidyverse", "janitor", "dataverse", "data.table")
lapply(packages, library, character.only = TRUE)
rm(packages)

# load data from Dataverse
Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")
dataset1 <- get_dataset("doi:10.7910/DVN/TZJGAT")
writeBin(get_file("sapaTempData696items22dec2015thru07feb2017.tab", "doi:10.7910/DVN/TZJGAT"), "sapaTempData696items22dec2015thru07feb2017.tab")
sapaTempData696items22dec2015thru07feb2017 <- fread("sapaTempData696items22dec2015thru07feb2017.tab", na.strings=getOption("<NA>","NA"))
sapaTempData696items22dec2015thru07feb2017 <- as.data.frame(sapaTempData696items22dec2015thru07feb2017)
sapaTempData696items22dec2015thru07feb2017 <- subset(sapaTempData696items22dec2015thru07feb2017, select = -c(1))
rm(dataset1)

#load SAPA cognition data collected prior to data to be used in current study
dataset2 <- get_dataset("doi:10.7910/DVN/AD9RVY")
writeBin(get_file("sapaICARData18aug2010thru20may2013.tab", "doi:10.7910/DVN/AD9RVY"), "sapaICARData18aug2010thru20may2013.tab")
sapaICARData18aug2010thru20may2013 <- fread("sapaICARData18aug2010thru20may2013.tab", na.strings=getOption("<NA>","NA"))
sapaICARData18aug2010thru20may2013 <- as.data.frame(sapaICARData18aug2010thru20may2013)
sapaICARData18aug2010thru20may2013 <- subset(sapaICARData18aug2010thru20may2013, select = -c(1))
rm(dataset2)

#rename datasets
sapa_pers = sapaTempData696items22dec2015thru07feb2017
sapa_cog = sapaICARData18aug2010thru20may2013
rm(sapaTempData696items22dec2015thru07feb2017)
rm(sapaICARData18aug2010thru20may2013)

#load personality scale information
source("scripts/personality_scales.R")

# ------------------------------------
# CREATE TOY DATASET TO TEST CODE    #
# ------------------------------------

# this dataset will resemble the dataset used in the analyses in that it will have many observations, 
# 135 personality items that can be scored as 27 or 5 traits, cognition items, SES items and BMI

# to create this dataset, 135 random persoanlity items are selected from the temperament dataset and 
# renamed to match the variables names of the personality items that will be used in the analyses
# we will keep the demographic and SES variables

  # identify personality items
  p_items = names(sapa_pers) %>% str_subset("q_")
  # randomly select 135 items
  use_p = sample(p_items, replace = FALSE, size = 135)
  
  # select all non-personality items and also the random 135 items
  sapa_pers = sapa_pers %>%
    select(-contains("q_"), use_p) 
  
  #rename personality items to match 135 item scale
  names(sapa_pers)[grepl("q_", names(sapa_pers))] = rownames(key.matrix)
  names(sapa_pers) = gsub("-", "", names(sapa_pers))
  
  #create Respondant ID variable
  sapa_pers = sapa_pers%>%
    mutate(RID = sample(10000:99999, size = nrow(.), replace = FALSE))

# next, a random sample from the cognition dataset with size equal to the personality dataset will be extracted. 
# participant ID numbers from the personality dataset will be randomly assigned to this sample and the two datasets
# will be merged
  
  cog_items = names(sapa_cog) %>% str_subset("\\.")
  sapa_cog = sapa_cog %>% 
    select(cog_items) %>%
    remove_empty("rows")
  
  #score cognition items
  sapa_cog$cog = rowMeans(sapa_cog, na.rm = T)

  #randomly sample rows
  sapa_cog = sapa_cog[sample(x = 1:nrow(sapa_cog), size = nrow(sapa_pers), replace = FALSE), "cog"]
  # assign part IDs
  sapa_cog = data.frame(RID = sapa_pers$RID, cog = sapa_cog)
  # merge with personality
  sapa = full_join(sapa_pers, sapa_cog)
  