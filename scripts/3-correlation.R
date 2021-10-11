
# ---- load packages and data             #----


#load packages
packages = c("tidyverse", "psych", "corrplot")
lapply(packages, library, character.only = TRUE)
rm(packages)

load(here("data/cleaned.Rdata"))

# ---- correlation matrix -----

R_male = sapa_male %>%
  dplyr::select(-BMI_c) %>%
  cor(use = "pairwise")

R_female = sapa_female %>%
  dplyr::select(-BMI_c) %>%
  cor(use = "pairwise")



#predictors
pred = names(sapa_male) %>% str_subset("BMI", negate = TRUE)


r_bmi_male = corr.test(x = sapa_male$BMI, y = sapa_male[,pred])
r_bmi_female = corr.test(x = sapa_female$BMI, y = sapa_female[,pred])

r_bmi_male = modify(r_bmi_male, as.vector)
r_bmi_female = modify(r_bmi_female, as.vector)

cor.data = data.frame(gender = c("male", "female"))
cor.data$fullr = list(r_bmi_male, r_bmi_female) 


cor.data = cor.data %>%
  mutate(r = map(fullr, "r")) %>%
  mutate(r = map(r, unlist)) %>%
  mutate(rp = map(fullr, "p")) %>%
  mutate(rp = map(rp, unlist)) %>%
  dplyr::select(-fullr) %>%
  unnest(cols = c(r, rp)) %>%
  mutate(pred = rep(pred,2)) %>%
  gather("key", "value", -gender, -pred) %>%
  unite(gender, gender, key) %>%
  spread(gender, value)

save(R_male, R_female, cor.data, file = "data/cor_output.Rdata")
