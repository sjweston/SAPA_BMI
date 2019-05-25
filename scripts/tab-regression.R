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

female_reg$gender = "female"
male_reg$gender = "male"

female_reg %>%
  full_join(male_reg) %>%
  filter(model %in% c("ses_edu", "ses_inc")) %>%
  filter(grepl("trait", term)) %>%
  mutate(conf.low = printnum(conf.low),
         conf.high = printnum(conf.high), 
         b2_conf = paste0("[", conf.low, ", ", conf.high, "]"),
         b1_est = printnum(estimate), 
         b1_est = ifelse(p.value < .05, paste0(b1_est, "*"), b1_est)) %>%
  dplyr::select(trait_name, model, term, b1_est, b2_conf, gender) %>%
  gather("key", "value", b1_est, b2_conf) %>%
  unite(col = "newkey", gender, model, term) %>%
  spread(newkey, value) %>%
  mutate(trait_name = rep(c(SPI_27_names, SPI_5_names), each = 2)) %>%
  mutate(trait_name = ifelse(!(row_number() %% 2), NA, trait_name)) %>%
  dplyr::select(-key) %>%
  kable(., col.names = rep(c("Trait", rep(c("b", "b x SES"), 4)))) %>%
  kable_styling() %>%
#col.names = c("Trait", rep(c("Est", "95% CI"), 8))
  add_header_above(c(" ", "Parental Education" = 2, "Parental Income" = 2, 
                     "Parental Education" = 2, "Parental Income" = 2)) %>%
  add_header_above(c(" ", "Female" = 4, "Male" = 4)) %>%
  group_rows("SPI: 27 Factors", 1, 54) %>%
  group_rows("SPI: 5 Factors", 55, 64)
