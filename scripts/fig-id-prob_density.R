# ---- load packages                      ----


packages = c("tidyverse", "here")
lapply(packages, library, character.only = TRUE)
rm(packages)

colors = RColorBrewer::brewer.pal(n = 3, "Dark2")

load(here("data/cleaned.Rdata"))
load(here("data/logistic_output.Rdata"))

# create newdata function
make_new = function(x, y){
  trait_score = seq(from = x, to = y, length.out = 200)
  ses = 0
  df = data.frame(trait_score, ses)
  return(df)
}


source("scripts/personality_scales.R")

# ---- female prob figure ----
# create new data for each trait

female_prep = data.frame(trait = unique(female_log$trait_name), stringsAsFactors = F) %>%
  mutate(trait_name = gsub("_135_27_5", "", trait)) %>%
  mutate(min = -3) %>%
  mutate(max = 3) %>%
  mutate(newdata = map2(.x = min, .y = max, make_new)) %>%
  select(trait_name, newdata) 

prob_dist_data_female = full_join(female_prep, female_log) %>%
  filter(model == "cov") %>%
  select(trait_name, final_mod, newdata) %>%
  mutate(predicted_probs = map2(final_mod, newdata, predict, type = "prob")) %>%
  mutate(newdata = map2(newdata, predicted_probs, cbind)) %>%
  select(trait_name, newdata) %>%
  unnest(cols = c(newdata)) %>%
  mutate(trait_name = gsub("SPI_135_27_5_", "", trait_name)) %>%
  mutate(trait_name = gsub("cog", "Cognitive Ability", trait_name)) 

# ---- male prob figure ----
# create new data for each trait

male_prep = data.frame(trait = unique(male_log$trait_name), stringsAsFactors = F) %>%
  mutate(trait_name = gsub("_135_27_5", "", trait)) %>%
  mutate(min = -3) %>%
  mutate(max = 3) %>%
  mutate(newdata = map2(.x = min, .y = max, make_new)) %>%
  select(trait_name, newdata) 

prob_dist_data_male = full_join(male_prep, male_log) %>%
  filter(model == "cov") %>%
  select(trait_name, final_mod, newdata) %>%
  mutate(predicted_probs = map2(final_mod, newdata, predict, type = "prob")) %>%
  mutate(newdata = map2(newdata, predicted_probs, cbind)) %>%
  select(trait_name, newdata) %>%
  unnest(cols = c(newdata)) %>%
  mutate(trait_name = gsub("SPI_135_27_5_", "", trait_name)) %>%
  mutate(trait_name = gsub("cog", "Cognitive Ability", trait_name)) 