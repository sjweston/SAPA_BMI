# ------------------------------------
# load packages                      #
# ------------------------------------

packages = c("tidyverse")
lapply(packages, library, character.only = TRUE)
rm(packages)


# ------------------------------------
# Figure. Cognition effect histogram #
# ------------------------------------

load("data/logistic_output.Rdata")

female_log = female_log %>%
  mutate(Gender = "Female")

male_log = male_log %>%
  mutate(Gender = "Male") 

female_log %>%
  full_join(male_log) %>%
  filter(model %in% c("cov_edu", "cov_inc")) %>%
  mutate(model = factor(model, labels = c("Parental Education", "Parental Income"))) %>%
  filter(term %in% c("edu", "income")) %>%
  mutate(estimate = exp(estimate)) %>%
  ggplot(aes(estimate, fill = Gender)) +
  geom_histogram(color = "white") +
  facet_grid(model~Gender+ y.level) +
  guides(fill = FALSE) +
  scale_x_continuous("Socioeconomic Status Odds Ratio Estimate") +
  theme_minimal()
