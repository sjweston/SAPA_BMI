# ------------------------------------
# load packages                      #
# ------------------------------------

packages = c("tidyverse")
lapply(packages, library, character.only = TRUE)
rm(packages)


# -------------------------------------------------------
# Figure. SES effect histogram (regression models)      #
# -------------------------------------------------------

load("data/regression_output.Rdata")

female_reg$Gender = "Female"
male_reg$Gender = "Male"

female_reg %>%
  full_join(male_reg) %>%
  filter(model == "cov") %>%
  filter(term == "ses") %>%
  ggplot(aes(estimate, fill = Gender)) +
  geom_histogram(color = "white") +
  facet_grid(Gender~.) +
  guides(fill = FALSE) +
  scale_x_continuous("Socioeconomic Status Coefficient Estimate") 


