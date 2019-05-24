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
  filter(model == "ses_mod") %>%
  filter(term == "trait_score"| term == "trait_score:ses") %>%
  mutate(conf.low = printnum(conf.low),
         conf.high = printnum(conf.high), 
         b2_conf = paste0("[", conf.low, ", ", conf.high, "]"),
         b1_est = printnum(estimate), 
         b1_est = ifelse(p.value < .05, paste0(b1_est, "*"), b1_est)) %>%
  dplyr::select(trait_name, term, b1_est, b2_conf, gender) %>%
  gather("key", "value", b1_est, b2_conf) %>%
  unite(col = "key", gender, term, key) %>%
  spread(key, value) %>%
  mutate(trait_name = c(SPI_27_names, SPI_5_names)) %>%
  kable(., col.names = c("Trait", rep(c("Est", "95% CI"),4))) %>%
  kable_styling() %>%
  add_header_above(c(" ", "Main Effect" = 2, "Moderated by SES" = 2, "Main Effect" = 2, "Moderated by SES" = 2)) %>%
  add_header_above(c(" ", "Female" = 4, "Male" = 4)) %>%
  group_rows("SPI: 27 Factors", 1, 27) %>%
  group_rows("SPI: 5 Factors", 28, 32)
