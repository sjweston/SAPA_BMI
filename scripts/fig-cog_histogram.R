# ------------------------------------
# load packages                      #
# ------------------------------------

packages = c("tidyverse")
lapply(packages, library, character.only = TRUE)
rm(packages)


# ------------------------------------
# Figure. Cognition effect histogram #
# ------------------------------------

load("data/regression_output.Rdata")

female_reg$Gender = "Female"
male_reg$Gender = "Male"

female_reg %>%
  full_join(male_reg) %>%
  filter(model == "cov_mod") %>%
  filter(term == "cog") %>%
  ggplot(aes(estimate, fill = Gender)) +
  geom_histogram(color = "white") +
  facet_wrap(~Gender) +
  guides(fill = FALSE) +
  scale_x_continuous("Cognitive Ability Coefficient Estimate") +
  theme_minimal()
