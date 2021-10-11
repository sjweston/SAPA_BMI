# ----- load packages ----


packages = c("tidyverse", "knitr", "kableExtra", "papaja", "here","ggpubr")
lapply(packages, library, character.only = TRUE)
rm(packages)

source(here("scripts/personality_scales.R"))

names(SPI_27_names) = gsub("135_27_5_", "", names(SPI_27_names))
names(SPI_5_names) = gsub("135_27_5_", "", names(SPI_5_names))


# ----- load  -----

# load main results
load(here("data/logistic_output.Rdata"))
female_log_main = female_log
male_log_main = male_log

# load edu results
load(here("data/logistic_output_eduOnly.Rdata.Rdata"))
female_log_edu = female_log
male_log_edu = male_log

female_log_main = female_log_main %>%
  mutate(coef = map(final_mod, broom::tidy, conf.int = TRUE)) %>%
  dplyr::select(trait_name, model, coef) %>%
  unnest(cols = c(coef)) %>%
  mutate(gender = "female",
         version = "main")

female_log_edu = female_log_edu %>%
  mutate(coef = map(final_mod, broom::tidy, conf.int = TRUE)) %>%
  dplyr::select(trait_name, model, coef) %>%
  unnest(cols = c(coef)) %>%
  mutate(gender = "female",
         version = "edu")


female_all = full_join(female_log_main, female_log_edu) %>%
  gather("key", "value", -gender, -version, -trait_name, -model, -y.level, -term) %>%
  unite(key, key, version) %>%
  spread(key, value) %>%
  mutate(sig = case_when(
    p.value_main < .05 & p.value_edu >= .05 ~ "All SES variables",
    p.value_main >= .05 & p.value_edu < .05 ~ "Education only",
    p.value_main < .05 & p.value_edu < .05 ~ "Both",
    p.value_main >= .05 & p.value_edu >= .05 ~ "Neither"
  )) %>%
  mutate(difference = estimate_main-estimate_edu)

male_log_main = male_log_main %>%
  mutate(coef = map(final_mod, broom::tidy, conf.int = TRUE)) %>%
  dplyr::select(trait_name, model, coef) %>%
  unnest(cols = c(coef)) %>%
  mutate(gender = "male",
         version = "main")

male_log_edu = male_log_edu %>%
  mutate(coef = map(final_mod, broom::tidy, conf.int = TRUE)) %>%
  dplyr::select(trait_name, model, coef) %>%
  unnest(cols = c(coef)) %>%
  mutate(gender = "male",
         version = "edu")

male_all = full_join(male_log_main, male_log_edu) %>%
  gather("key", "value", -gender, -version, -trait_name, -model, -y.level, -term) %>%
  unite(key, key, version) %>%
  spread(key, value) %>%
  mutate(sig = case_when(
    p.value_main < .05 & p.value_edu >= .05 ~ "Complete Only",
    p.value_main >= .05 & p.value_edu < .05 ~ "Imputed Only",
    p.value_main < .05 & p.value_edu < .05 ~ "Both",
    p.value_main >= .05 & p.value_edu >= .05 ~ "Neither"
  )) %>%
  mutate(difference = estimate_main-estimate_edu)

# ----- female statistical significance ---------------------------------------------

female_all %>%
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

female_all %>%
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

# ----- female different results ---------------------------------------------

W = .8
main_only = female_all %>%
  filter(sig %in% c("Complete Only", "Imputed Only"),
         term == "trait_score",
         model == "cov") %>%
  gather("key","value",estimate_main, estimate_edu, conf.high_main, conf.low_main, conf.high_edu, conf.low_edu) %>%
  separate(key, into = c("key", "version"), sep = "_") %>%
  spread(key, value) %>%
  arrange(desc(estimate)) %>%
  mutate(estimate = 1-estimate,
         conf.low = 1-conf.low,
         conf.high = 1-conf.high) %>%
  filter(sig == "Complete Only") %>%
  ggplot(aes(x = trait_name, y = estimate, fill = version)) +
  geom_bar(stat = "identity",position = position_dodge(W), color = "black", alpha = .7) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(W), width = .5, color = "black")+ 
  coord_flip() +
  guides(fill = F) +
  scale_x_discrete("") +
  scale_y_continuous("Odds Ratio",
                     breaks = seq(from = -1, 1.5, by = .5), 
                     labels = seq(-2, 0.5, by = .5)) +
  ggtitle("Coefficients significant\nonly in complete data") +
  facet_grid(y.level~., 
             scales = "free_y")

edu_only = female_all %>%
  filter(sig %in% c("Complete Only", "Imputed Only"),
         term == "trait_score",
         model == "cov") %>%
  gather("key","value",estimate_main, estimate_edu, conf.high_main, conf.low_main, conf.high_edu, conf.low_edu) %>%
  separate(key, into = c("key", "version"), sep = "_") %>%
  spread(key, value) %>%
  arrange(desc(estimate)) %>%
  mutate(estimate = 1-estimate,
         conf.low = 1-conf.low,
         conf.high = 1-conf.high) %>%
  filter(sig == "Imputed Only") %>%
  ggplot(aes(x = trait_name, y = estimate, fill = version)) +
  geom_bar(stat = "identity",position = position_dodge(W), color = "black", alpha = .7) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(W), width = .5, color = "black")+ 
  coord_flip() +
  #guides(fill = F) +
  scale_x_discrete("") +
  scale_y_continuous("Odds Ratio",
                     breaks = seq(from = -1.5, 1.5, by = .5), 
                     labels = seq(-2.5, 0.5, by = .5)) +
  scale_fill_discrete("Data", labels = c("Complete", "Imputed"))+
  ggtitle("Coefficients significant\nonly in imputed data") +
  facet_grid(y.level~., 
             scales = "free_y")

ggarrange(main_only, edu_only, nrow = 1)

# ----- female same results ---------------------------------------------

W = .8
female_both = female_all %>%
  filter(sig %in% c("Both"),
         term == "trait_score",
         model == "cov") %>%
  gather("key","value",estimate_main, estimate_edu, conf.high_main, conf.low_main, conf.high_edu, conf.low_edu) %>%
  separate(key, into = c("key", "version"), sep = "_") %>%
  spread(key, value) %>%
  arrange(desc(estimate)) %>%
  mutate(estimate = 1-estimate,
         conf.low = 1-conf.low,
         conf.high = 1-conf.high) %>%
  ggplot(aes(x = trait_name, y = estimate, fill = version)) +
  geom_bar(stat = "identity",position = position_dodge(W), color = "black", alpha = .7) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(W), width = .5, color = "black")+ 
  coord_flip() +
  guides(fill = F) +
  scale_x_discrete("") +
  scale_y_continuous("Odds Ratio",
                     breaks = seq(from = -1, 1.5, by = .5), 
                     labels = seq(-2, 0.5, by = .5)) +
  ggtitle("Coefficients significant\nonly in complete data") +
  facet_grid(y.level~., 
             scales = "free_y")


# ----- male statistical significance ---------------------------------------------

male_all %>%
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

male_all %>%
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

# ----- male different results ---------------------------------------------

W = .8
main_only = male_all %>%
  filter(sig %in% c("Complete Only", "Imputed Only"),
         term == "trait_score",
         model == "cov") %>%
  gather("key","value",estimate_main, estimate_edu, conf.high_main, conf.low_main, conf.high_edu, conf.low_edu) %>%
  separate(key, into = c("key", "version"), sep = "_") %>%
  spread(key, value) %>%
  arrange(desc(estimate)) %>%
  mutate(estimate = 1-estimate,
         conf.low = 1-conf.low,
         conf.high = 1-conf.high) %>%
  filter(sig == "Complete Only") %>%
  ggplot(aes(x = trait_name, y = estimate, fill = version)) +
  geom_bar(stat = "identity",position = position_dodge(W), color = "black", alpha = .7) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(W), width = .5, color = "black")+ 
  coord_flip() +
  guides(fill = F) +
  scale_x_discrete("") +
  scale_y_continuous("Odds Ratio",
                     breaks = seq(from = -1, 1.5, by = .5), 
                     labels = seq(-2, 0.5, by = .5)) +
  ggtitle("Coefficients significant\nonly in complete data") +
  facet_grid(y.level~., 
             scales = "free_y")

edu_only = male_all %>%
  filter(sig %in% c("Complete Only", "Imputed Only"),
         term == "trait_score",
         model == "cov") %>%
  gather("key","value",estimate_main, estimate_edu, conf.high_main, conf.low_main, conf.high_edu, conf.low_edu) %>%
  separate(key, into = c("key", "version"), sep = "_") %>%
  spread(key, value) %>%
  arrange(desc(estimate)) %>%
  mutate(estimate = 1-estimate,
         conf.low = 1-conf.low,
         conf.high = 1-conf.high) %>%
  filter(sig == "Imputed Only") %>%
  ggplot(aes(x = trait_name, y = estimate, fill = version)) +
  geom_bar(stat = "identity",position = position_dodge(W), color = "black", alpha = .7) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(W), width = .5, color = "black")+ 
  coord_flip() +
  #guides(fill = F) +
  scale_x_discrete("") +
  scale_y_continuous("Odds Ratio",
                     breaks = seq(from = -1.5, 1.5, by = .5), 
                     labels = seq(-2.5, 0.5, by = .5)) +
  scale_fill_discrete("Data", labels = c("Complete", "Imputed"))+
  ggtitle("Coefficients significant\nonly in imputed data") +
  facet_grid(y.level~., 
             scales = "free_y")

ggarrange(main_only, edu_only, nrow = 1)


# ----- male same results ---------------------------------------------

W = .8
male_both = male_all %>%
  filter(sig %in% c("Both"),
         term == "trait_score",
         model == "cov") %>%
  gather("key","value",estimate_main, estimate_edu, conf.high_main, conf.low_main, conf.high_edu, conf.low_edu) %>%
  separate(key, into = c("key", "version"), sep = "_") %>%
  spread(key, value) %>%
  arrange(desc(estimate)) %>%
  mutate(estimate = 1-estimate,
         conf.low = 1-conf.low,
         conf.high = 1-conf.high) %>%
  ggplot(aes(x = trait_name, y = estimate, fill = version)) +
  geom_bar(stat = "identity",position = position_dodge(W), color = "black", alpha = .7) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(W), width = .5, color = "black")+ 
  coord_flip() +
  guides(fill = F) +
  scale_x_discrete("") +
  scale_y_continuous("Odds Ratio",
                     breaks = seq(from = -1, 1.5, by = .5), 
                     labels = seq(-2, 0.5, by = .5)) +
  ggtitle("Coefficients significant\nonly in complete data") +
  facet_grid(y.level~., 
             scales = "free_y")


