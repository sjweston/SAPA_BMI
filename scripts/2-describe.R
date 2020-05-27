# ---- load packages and data ----

#load packages
packages = c("tidyverse", "psych")
lapply(packages, library, character.only = TRUE)
rm(packages)

load("data/cleaned.Rdata")

# ---- calculate ----

descriptives = describeBy(sapa, group = "sex")

# ---- save file ----

save(descriptives, file = "data/descriptives.Rdata")
