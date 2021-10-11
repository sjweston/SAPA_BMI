#load packages
packages = c("tidyverse", "broom", "sjPlot", "rsample", "here", "ggpubr")
lapply(packages, library, character.only = TRUE)
rm(packages)

load("data/cleaned.Rdata")

# ----  set up bootstraps   -----


# number of bootstrap samples
boot.n = 10000

# ----  wrangle data for iteration   ------


#end goal of wrangling is a data frame of data frames
# nested dataframes correspond to a single personality trait
# score refers to a participant's score on that trait
# we also standardize each of our variables within gender


sapa_male_trait = sapa_male[train_male, ] %>%
  dplyr::select(-starts_with("p1"), -starts_with("p2")) %>%
  mutate(set = ifelse(row_number() %in% train_male[,1], "train", "test")) %>% #identify which rows in test and training
  gather("trait_name", "trait_score", -ses, -BMI_c, -BMI, -BMI_p, -set) %>% # gather all personality variables
  group_by(trait_name, set) %>% # group by trait and also by whether in test/train
  mutate(trait_score = as.numeric(scale(trait_score))) %>% #standardize
  mutate(ses = as.numeric(scale(ses))) %>% #standardize
  ungroup() 

sapa_female_trait = sapa_female[train_female, ] %>%
  dplyr::select(-starts_with("p1"), -starts_with("p2")) %>%
  mutate(set = ifelse(row_number() %in% train_male[,1], "train", "test")) %>% #identify which rows in test and training
  gather("trait_name", "trait_score", -ses, -BMI_c, -BMI, -BMI_p, -set) %>% # gather all personality variables
  group_by(trait_name, set) %>% # group by trait and also by whether in test/train
  mutate(trait_score = as.numeric(scale(trait_score))) %>% #standardize
  mutate(ses = as.numeric(scale(ses))) %>% #standardize
  ungroup()


# conservatism ------------------------------------------------------------

female_conservatism = sapa_female_trait %>%
  filter(set == "train") %>%
  filter(str_detect(trait_name, "Conse")) %>%
  lm(BMI_p ~ ses*trait_score, data = .) %>% 
  plot_model(type = "pred", terms =c ("trait_score", "ses[meansd]"))

male_conservatism = sapa_male_trait %>%
  filter(set == "train") %>%
  filter(str_detect(trait_name, "Conse")) %>%
  lm(BMI_p ~ ses*trait_score, data = .) %>% 
  plot_model(type = "pred", terms =c ("trait_score", "ses[meansd]"))

female_conservatism = female_conservatism$data %>% mutate(gender = "Adolescent Girls")
male_conservatism = male_conservatism$data %>% mutate(gender = "Adolescent Boys")

female_conservatism %>%
  full_join(male_conservatism) %>%
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group_col), alpha = .2) +
  geom_line(aes(, color = group_col)) +
  labs(x = "Conservatism (z-score)", 
       y = "BMI Percentile", 
       color = "SES", 
       fill = "SES") +
  scale_fill_brewer(palette = "Set2", 
                    labels = c("Low", "Average", "High")) +
  scale_color_brewer(palette = "Set2", 
                     labels = c("Low", "Average", "High")) +
  facet_wrap(~gender) +
  theme_pubr()
ggsave(here("figures/conservatism_interaction.jpeg"), width = 6, height = 4)
