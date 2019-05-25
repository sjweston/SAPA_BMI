# ------------------------------------
# load packages                      #
# ------------------------------------

packages = c("tidyverse")
lapply(packages, library, character.only = TRUE)
rm(packages)


# ------------------------------------
# Figure. SES effect histogram       #
# ------------------------------------

load("data/regression_output.Rdata")

female_reg$Gender = "Female"
male_reg$Gender = "Male"

female_reg %>%
  full_join(male_reg) %>%
  filter(model %in% c("cov_edu", "cov_inc")) %>%
  mutate(model = factor(model, labels = c("Parental Education", "Parental Income"))) %>%
  filter(term %in% c("edu", "income")) %>%
  ggplot(aes(estimate, fill = Gender)) +
  geom_histogram(color = "white") +
  facet_grid(model~Gender) +
  guides(fill = FALSE) +
  scale_x_continuous("Socioeconomic Status Coefficient Estimate") +
  theme_minimal()
