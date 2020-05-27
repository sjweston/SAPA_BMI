
# ----- load packages ----


packages = c("tidyverse", "knitr", "kableExtra", "papaja", "here")
lapply(packages, library, character.only = TRUE)
rm(packages)

source("scripts/personality_scales.R")

names(SPI_27_names) = gsub("135_27_5_", "", names(SPI_27_names))
names(SPI_5_names) = gsub("135_27_5_", "", names(SPI_5_names))

# ----- Table. Regression model  -----


load("data/logistic_output.Rdata")

female_log = female_log %>%
  mutate(coef = map(final_mod, broom::tidy, conf.int = TRUE)) %>%
  dplyr::select(trait_name, model, coef) %>%
  unnest(cols = c(coef)) %>%
  mutate(gender = "female")

male_log = male_log %>%
  mutate(coef = map(final_mod, broom::tidy, conf.int = TRUE)) %>%
  dplyr::select(trait_name, model, coef) %>%
  unnest(cols = c(coef)) %>%
  mutate(gender = "male")

female_log %>%
  full_join(male_log) %>%
  filter(grepl("trait", term)) %>%
  filter(model == "int") %>%
  mutate(b1_est = printnum(exp(estimate)),  
         b1_est = ifelse(conf.low > 0 | conf.high < 0, paste0(b1_est, "*"), b1_est),
         conf.low = printnum(exp(conf.low)),
         conf.high = printnum(exp(conf.high)), 
         b2_conf = paste0("[", conf.low, ", ", conf.high, "]")) %>%
  dplyr::select(trait_name, y.level, term, b1_est, b2_conf, gender) %>%
  gather("key", "value", b1_est, b2_conf) %>%
  mutate(term = ifelse(term == "trait_score", "main", "moderated")) %>%
  unite(col = "newkey", gender, y.level, term) %>%
  spread(newkey, value) %>%
  ungroup() %>%
  mutate(trait_name = factor(trait_name, levels = c("cog", names(SPI_27_names), names(SPI_5_names)))) %>% 
  arrange(trait_name) %>% 
  mutate(trait_name = rep(c("Cognitive Ability", SPI_27_names, SPI_5_names),each = 2)) %>% 
  mutate(trait_name = ifelse(!(row_number() %% 2), NA, trait_name)) %>%
  dplyr::select(-key) %>% 
  kable(., col.names = rep(c("Trait", rep(c("b", "b x SES"), 6)))) %>%
  kable_styling() %>%
  add_header_above(c(" ", "Obese" = 2, "Overweight" = 2, "Underweight" = 2, "Obese" = 2, "Overweight" = 2, "Underweight" = 2)) %>%
  add_header_above(c(" ", "Female" = 6, "Male" = 6)) %>%
  group_rows("SPI: 27 Factors", 3, 56) %>%
  group_rows("SPI: 5 Factors", 57, 66)
  

# ----- Table. Regression model additive Female ----

load("data/logistic_output.Rdata")

female_log = female_log %>%
  mutate(coef = map(final_mod, broom::tidy, conf.int = TRUE)) %>%
  dplyr::select(trait_name, model, coef) %>%
  unnest(cols = c(coef)) %>%
  mutate(gender = "female")

female_log %>%
  filter(grepl("trait", term)) %>%
  filter(model == "cov") %>%
  mutate(b1_est = printnum(estimate),  
         b1_est = ifelse(conf.low > 1 | conf.high < 1, paste0(b1_est, "*"), b1_est),
         conf.low = printnum(conf.low),
         conf.high = printnum(conf.high), 
         b2_conf = paste0("[", conf.low, ", ", conf.high, "]")) %>%
  dplyr::select(trait_name, y.level, term, b1_est, b2_conf, gender) %>%
  gather("key", "value", b1_est, b2_conf) %>%
  spread(y.level, value) %>%
  ungroup() %>%
  mutate(trait_name = factor(trait_name, levels = c("cog", names(SPI_27_names), names(SPI_5_names)))) %>% 
  arrange(trait_name) %>% 
  mutate(trait_name = rep(c("Cognitive Ability", SPI_27_names, SPI_5_names),each = 2)) %>% 
  mutate(trait_name = ifelse(!(row_number() %% 2), NA, trait_name)) %>%
  dplyr::select(-term, -gender, -key) %>%
  mutate(Obese = ifelse(grepl("\\*", Obese),
                       cell_spec(Obese, "html", color = "red", bold = T),
                       cell_spec(Obese, "html")), 
         Overweight = ifelse(grepl("\\*", Overweight),
                        cell_spec(Overweight, "html", color = "red", bold = T),
                        cell_spec(Overweight, "html")), 
         Underweight = ifelse(grepl("\\*", Underweight),
                        cell_spec(Underweight, "html", color = "red", bold = T),
                        cell_spec(Underweight, "html"))) %>%
  kable(., col.names = c("Trait", "Obese", "Overweight", "Underweight"), escape = F) %>%
  kable_styling() %>%
  group_rows("SPI: 27 Factors", 3, 56) %>%
  group_rows("SPI: 5 Factors", 57, 66)

# ----- Table. Regression model additive Male ----

load("data/logistic_output.Rdata")
load("data/logistic_output.Rdata")

male_log = male_log %>%
  mutate(coef = map(final_mod, broom::tidy, conf.int = TRUE)) %>%
  dplyr::select(trait_name, model, coef) %>%
  unnest(cols = c(coef)) %>%
  mutate(gender = "male")

male_log %>%
  filter(grepl("trait", term)) %>%
  filter(model == "cov") %>%
  mutate(b1_est = printnum(estimate),  
         b1_est = ifelse(conf.low > 1 | conf.high < 1, paste0(b1_est, "*"), b1_est),
         conf.low = printnum(conf.low),
         conf.high = printnum(conf.high), 
         b2_conf = paste0("[", conf.low, ", ", conf.high, "]")) %>%
  dplyr::select(trait_name, y.level, term, b1_est, b2_conf, gender) %>%
  gather("key", "value", b1_est, b2_conf) %>%
  spread(y.level, value) %>%
  ungroup() %>%
  mutate(trait_name = factor(trait_name, levels = c("cog", names(SPI_27_names), names(SPI_5_names)))) %>% 
  arrange(trait_name) %>% 
  mutate(trait_name = rep(c("Cognitive Ability", SPI_27_names, SPI_5_names),each = 2)) %>% 
  mutate(trait_name = ifelse(!(row_number() %% 2), NA, trait_name)) %>%
  dplyr::select(-term, -gender, -key) %>%
  mutate(Obese = ifelse(grepl("\\*", Obese),
                        cell_spec(Obese, "html", color = "red", bold = T),
                        cell_spec(Obese, "html")), 
         Overweight = ifelse(grepl("\\*", Overweight),
                             cell_spec(Overweight, "html", color = "red", bold = T),
                             cell_spec(Overweight, "html")), 
         Underweight = ifelse(grepl("\\*", Underweight),
                              cell_spec(Underweight, "html", color = "red", bold = T),
                              cell_spec(Underweight, "html"))) %>%
  kable(., col.names = c("Trait", "Obese", "Overweight", "Underweight"), escape = F) %>%
  kable_styling() %>%
  group_rows("SPI: 27 Factors", 3, 56) %>%
  group_rows("SPI: 5 Factors", 57, 66)