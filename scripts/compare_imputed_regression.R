# ----- load packages ----


packages = c("tidyverse", "knitr", "kableExtra", "papaja", "here","ggpubr")
lapply(packages, library, character.only = TRUE)
rm(packages)

source(here("scripts/personality_scales.R"))

names(SPI_27_names) = gsub("135_27_5_", "", names(SPI_27_names))
names(SPI_5_names) = gsub("135_27_5_", "", names(SPI_5_names))

# ----- compare distributions ----

load(here("data/cleaned.Rdata"))
complete = sapa[,c("BMI_p", "sex")]
rm(list = c("sapa", "sapa_female", "sapa_male"))

load(here("data/impute_pca/cleaned.Rdata"))
imputed = sapa[,c("BMI_p", "sex")]
rm(list = c("sapa", "sapa_female", "sapa_male"))

complete$version = "complete"
imputed$version = "imputed"

complete %>%
  full_join(imputed) %>% 
  ggplot(aes(x = BMI_p, fill = version)) +
  geom_density(alpha = .5) +
  facet_wrap(~sex) +
  labs(x = "BMI percentile", fill = "Data", 
       title = "Comparison of BMI percentile distribution\nin complete and imputed datasets") +
  theme(legend.position = "top")

# ----- load  -----

# load main results
load(here("data/regression_output.Rdata"))
female_reg_main = female_reg
male_reg_main = male_reg

# load imputePCA results
load(here("data/regression_impute_female.Rdata"))
load(here("data/regression_impute_male.Rdata"))
female_reg_pca = female_reg
male_reg_pca = male_reg

female_reg_main = female_reg_main %>%
  mutate(gender = "female",
         version = "main")

female_reg_pca = female_reg_pca %>%
  mutate(gender = "female",
         version = "pca")

male_reg_main = male_reg_main %>%
  mutate(gender = "male",
         version = "main")

male_reg_pca = male_reg_pca %>%
  mutate(gender = "male",
         version = "pca")


# ----- female statistical significance ---------------------------------------------

full_join(female_reg_main, female_reg_pca) %>%
  select(trait_name, version, model, term, p.value) %>% 
  mutate(trait_name = str_remove(trait_name, "SPI_135_27_5_"),
         trait_name = str_remove(trait_name, "SPI_")) %>% 
  spread(version, p.value) %>% 
  # gather("key", "value", -gender, -version, -trait_name, -model, -y.level, -term) %>%
  # unite(key, key, version) %>%
  # spread(key, value) %>%
  mutate(sig = case_when(
    main < .05 & pca >= .05 ~ "Complete Only",
    main >= .05 & pca < .05 ~ "Imputed Only",
    main < .05 & pca < .05 ~ "Both",
    main >= .05 & pca >= .05 ~ "Neither"
  )) %>%
  filter(term != "(Intercept)") %>%
  mutate(model = ifelse(model == "cov", "Additive Effects", "Joint Effects"),
         term = case_when(
           term == "ses" ~ "SES",
           term == "trait_score" ~ "Trait",
           TRUE ~ "Interaction"
         )) %>%
  ggplot(aes(x = term, fill = sig)) +
  geom_bar(stat = "count", position = "dodge", alpha = .7, color = "black") +
  facet_grid(model~., scales = "free_x") +
  labs(x = "", y = "Count", title = "Statistical significance across models") +
  theme(legend.position = "top") 

# ----- female coef difference ---------------------------------------------

full_join(female_reg_main, female_reg_pca) %>%
  select(trait_name, version, model, term, estimate) %>% 
  mutate(trait_name = str_remove(trait_name, "SPI_135_27_5_"),
         trait_name = str_remove(trait_name, "SPI_")) %>% 
  spread(version, estimate) %>% 
  # gather("key", "value", -gender, -version, -trait_name, -model, -y.level, -term) %>%
  # unite(key, key, version) %>%
  # spread(key, value) %>%
  mutate(difference = pca - main)  %>%
  filter(term != "(Intercept)") %>%
  mutate(model = ifelse(model == "cov", "Additive Effects", "Joint Effects"),
         term = case_when(
           term == "ses" ~ "SES",
           term == "trait_score" ~ "Trait",
           TRUE ~ "Interaction"
         )) %>%
  ggplot(aes(x = term, y = difference)) +
  geom_boxplot() +
  facet_grid(model~., scales = "free_x") +
  labs(x = "", y = "Difference in standardized coefficient estimates") +
  theme(legend.position = "top")

# ----- male statistical significance ---------------------------------------------

full_join(male_reg_main, male_reg_pca) %>%
  select(trait_name, version, model, term, p.value) %>% 
  mutate(trait_name = str_remove(trait_name, "SPI_135_27_5_"),
         trait_name = str_remove(trait_name, "SPI_")) %>% 
  spread(version, p.value) %>% 
  # gather("key", "value", -gender, -version, -trait_name, -model, -y.level, -term) %>%
  # unite(key, key, version) %>%
  # spread(key, value) %>%
  mutate(sig = case_when(
    main < .05 & pca >= .05 ~ "Complete Only",
    main >= .05 & pca < .05 ~ "Imputed Only",
    main < .05 & pca < .05 ~ "Both",
    main >= .05 & pca >= .05 ~ "Neither"
  )) %>%
  filter(term != "(Intercept)") %>%
  mutate(model = ifelse(model == "cov", "Additive Effects", "Joint Effects"),
         term = case_when(
           term == "ses" ~ "SES",
           term == "trait_score" ~ "Trait",
           TRUE ~ "Interaction"
         )) %>%
  ggplot(aes(x = term, fill = sig)) +
  geom_bar(stat = "count", position = "dodge", alpha = .7, color = "black") +
  facet_grid(model~., scales = "free_x") +
  labs(x = "", y = "Count", title = "Statistical significance across models") +
  theme(legend.position = "top") 

# ----- male coef difference ---------------------------------------------

full_join(male_reg_main, male_reg_pca) %>%
  select(trait_name, version, model, term, estimate) %>% 
  mutate(trait_name = str_remove(trait_name, "SPI_135_27_5_"),
         trait_name = str_remove(trait_name, "SPI_")) %>% 
  spread(version, estimate) %>% 
  # gather("key", "value", -gender, -version, -trait_name, -model, -y.level, -term) %>%
  # unite(key, key, version) %>%
  # spread(key, value) %>%
  mutate(difference = pca - main)  %>%
  filter(term != "(Intercept)") %>%
  mutate(model = ifelse(model == "cov", "Additive Effects", "Joint Effects"),
         term = case_when(
           term == "ses" ~ "SES",
           term == "trait_score" ~ "Trait",
           TRUE ~ "Interaction"
         )) %>%
  ggplot(aes(x = term, y = difference)) +
  geom_boxplot() +
  facet_grid(model~., scales = "free_x") +
  labs(x = "", y = "Difference in standardized coefficient estimates") +
  theme(legend.position = "top")



