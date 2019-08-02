# ------------------------------------
# load packages                      #
# ------------------------------------

packages = c("tidyverse", "knitr", "kableExtra", "papaja")
lapply(packages, library, character.only = TRUE)
rm(packages)

source("scripts/personality_scales.R")



# ------------------------------------
# Table. Regression model            #
# ------------------------------------

load("data/regression_output.Rdata")

female_reg = female_reg %>%
  mutate(gender = "female")

male_reg = male_reg %>%
  mutate(gender = "male")

female_reg %>%
  full_join(male_reg) %>%
  filter(grepl("trait", term)) %>%
  mutate(b1_est = printnum(estimate),  
         b1_est = ifelse(conf.low > 0 | conf.high < 0, paste0(b1_est, "*"), b1_est),
         conf.low = printnum(conf.low),
         conf.high = printnum(conf.high), 
         b2_conf = paste0("[", conf.low, ", ", conf.high, "]")) %>%
  dplyr::select(trait_name, model, term, b1_est, b2_conf, gender) %>%
  gather("key", "value", b1_est, b2_conf) %>%
  unite(col = "newkey", gender, model, term) %>%
  spread(newkey, value) %>%
  mutate(trait_name = factor(trait_name, levels = c("cog", names(SPI_27_names), names(SPI_5_names)))) %>% 
  arrange(trait_name) %>% 
  mutate(trait_name = rep(c("Cognitive Ability", SPI_27_names, SPI_5_names),each =2)) %>% 
  mutate(trait_name = ifelse(!(row_number() %% 2), NA, trait_name)) %>%
  dplyr::select(-key) %>%
  kable(., col.names = rep(c("Trait", rep(c("b", "b", "b x SES"), 2)))) %>%
  kable_styling() %>%
  add_header_above(c(" ", "Additive Model" = 1, "Interaction Model" = 2, "Additive Model" = 1, "Interaction Model" = 2)) %>%
  add_header_above(c(" ", "Female" = 3, "Male" = 3)) %>%
  group_rows("SPI: 27 Factors", 3, 56) %>%
  group_rows("SPI: 5 Factors", 57, 66)
