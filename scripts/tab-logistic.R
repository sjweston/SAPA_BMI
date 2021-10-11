
# ----- load packages ----


packages = c("tidyverse", "knitr", "kableExtra", "papaja", "here")
lapply(packages, library, character.only = TRUE)
rm(packages)

source(here("scripts/personality_scales.R"))

names(SPI_27_names) = gsub("135_27_5_", "", names(SPI_27_names))
names(SPI_5_names) = gsub("135_27_5_", "", names(SPI_5_names))

# ----- Table. Regression model  -----


load(here("data/logistic_output.Rdata"))

female_log = female_log %>%
  filter(str_detect(trait_name, "SPI") | trait_name == "cog") %>%
  mutate(coef = map(final_mod, broom::tidy, conf.int = TRUE)) %>%
  dplyr::select(trait_name, model, coef) %>%
  unnest(cols = c(coef)) %>%
  mutate(gender = "female")

male_log = male_log %>%
  filter(str_detect(trait_name, "SPI") | trait_name == "cog") %>%
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

load(here("data/logistic_output.Rdata"))

female_log = female_log %>%
  filter(str_detect(trait_name, "SPI") | trait_name == "cog") %>%
  mutate(coef = map(final_mod, broom::tidy, conf.int = TRUE)) %>%
  dplyr::select(trait_name, model, coef) %>%
  unnest(cols = c(coef)) %>%
  mutate(gender = "female")

female_log_tab = female_log %>%
  filter(grepl("trait", term)) %>%
  filter(model == "cov") %>%
  mutate(b1_est = printnum(exp(estimate)),  
         b1_est = ifelse(conf.low > 0 | conf.high < 0, paste0(b1_est, "*"), b1_est),
         conf.low = printnum(exp(conf.low)),
         conf.high = printnum(exp(conf.high)), 
         b2_conf = paste0("[", conf.low, ", ", conf.high, "]"), 
         b1_est = paste(b1_est, b2_conf)) %>%
  dplyr::select(trait_name, y.level, term, b1_est, gender) %>%
  #gather("key", "value", b1_est, b2_conf) %>%
  spread(y.level, b1_est) %>%
  ungroup() %>%
  mutate(trait_name = factor(trait_name, levels = c("cog", names(SPI_27_names), names(SPI_5_names)))) %>% 
  arrange(trait_name) %>% 
  mutate(trait_name = c("Cognitive Ability", SPI_27_names, SPI_5_names)) %>% 
  #mutate(trait_name = ifelse(!(row_number() %% 2), NA, trait_name)) %>%
  dplyr::select(-term, -gender) 

f_tab_obese_color = ifelse(
  str_detect(female_log_tab$Obese, "\\*"), "red", "black")
f_tab_obese_boldf = ifelse(
  str_detect(female_log_tab$Obese, "\\*"), TRUE, FALSE)

f_tab_overw_color = ifelse(
  str_detect(female_log_tab$Overweight, "\\*"), "red", "black")
f_tab_overw_boldf = ifelse(
  str_detect(female_log_tab$Overweight, "\\*"), TRUE, FALSE)

f_tab_underw_color = ifelse(
  str_detect(female_log_tab$Underweight, "\\*"), "red", "black")
f_tab_underw_boldf = ifelse(
  str_detect(female_log_tab$Underweight, "\\*"), TRUE, FALSE)

female_log_tab %>%
  kable(., 
        booktabs = T,
        col.names = c("Trait", "Obese", "Overweight", "Underweight"), 
        escape = F) %>%
  kable_styling() %>%
  column_spec(2, color = f_tab_obese_color, bold = f_tab_obese_boldf) %>%
  column_spec(3, color = f_tab_overw_color, bold = f_tab_overw_boldf) %>%
  column_spec(4, color = f_tab_underw_color, bold = f_tab_underw_boldf) %>% 
  group_rows("SPI: 27 Factors", 2, 28) %>%
  group_rows("SPI: 5 Factors", 29, 33)

# ----- Table. Regression model additive Male ----

load(here("data/logistic_output.Rdata"))

male_log = male_log %>%
  filter(str_detect(trait_name, "SPI") | trait_name == "cog") %>%
  mutate(coef = map(final_mod, broom::tidy, conf.int = TRUE)) %>%
  dplyr::select(trait_name, model, coef) %>%
  unnest(cols = c(coef)) %>%
  mutate(gender = "male")

male_log_tab = male_log %>%
  filter(grepl("trait", term)) %>%
  filter(model == "cov") %>%
  mutate(b1_est = printnum(exp(estimate)),  
         b1_est = ifelse(conf.low > 0 | conf.high < 0, paste0(b1_est, "*"), b1_est),
         conf.low = printnum(exp(conf.low)),
         conf.high = printnum(exp(conf.high)), 
         b2_conf = paste0("[", conf.low, ", ", conf.high, "]"), 
         b1_est = paste(b1_est, b2_conf)) %>%
  dplyr::select(trait_name, y.level, term, b1_est, gender) %>%
  #gather("key", "value", b1_est, b2_conf) %>%
  spread(y.level, b1_est) %>%
  ungroup() %>%
  mutate(trait_name = factor(trait_name, levels = c("cog", names(SPI_27_names), names(SPI_5_names)))) %>% 
  arrange(trait_name) %>% 
  mutate(trait_name = c("Cognitive Ability", SPI_27_names, SPI_5_names)) %>% 
  #mutate(trait_name = ifelse(!(row_number() %% 2), NA, trait_name)) %>%
  dplyr::select(-term, -gender) 

m_tab_obese_color = ifelse(
  str_detect(male_log_tab$Obese, "\\*"), "red", "black")
m_tab_obese_boldf = ifelse(
  str_detect(male_log_tab$Obese, "\\*"), TRUE, FALSE)

m_tab_overw_color = ifelse(
  str_detect(male_log_tab$Overweight, "\\*"), "red", "black")
m_tab_overw_boldf = ifelse(
  str_detect(male_log_tab$Overweight, "\\*"), TRUE, FALSE)

m_tab_underw_color = ifelse(
  str_detect(male_log_tab$Underweight, "\\*"), "red", "black")
m_tab_underw_boldf = ifelse(
  str_detect(male_log_tab$Underweight, "\\*"), TRUE, FALSE)

male_log_tab %>%
  kable(., 
        booktabs = T,
        col.names = c("Trait", "Obese", "Overweight", "Underweight"), 
        escape = F) %>%kable_styling() %>%
  column_spec(2, color = m_tab_obese_color, bold = m_tab_obese_boldf) %>%
  column_spec(3, color = m_tab_overw_color, bold = m_tab_overw_boldf) %>%
  column_spec(4, color = m_tab_underw_color, bold = m_tab_underw_boldf) %>% 
  group_rows("SPI: 27 Factors", 2, 28) %>%
  group_rows("SPI: 5 Factors", 29, 33)
