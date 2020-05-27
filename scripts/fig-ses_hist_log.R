# ---- load packages                      ----


packages = c("tidyverse")
lapply(packages, library, character.only = TRUE)
rm(packages)

colors = RColorBrewer::brewer.pal(n = 3, "Dark2")

load("data/logistic_output.Rdata")

female_log = female_log %>%
  mutate(Gender = "Female") %>%
  select(trait_name, model, coef, Gender) %>%
  unnest(cols = c(coef))



male_log = male_log %>%
  mutate(Gender = "Male") %>%
  select(trait_name, model, coef, Gender) %>%
  unnest(cols = c(coef))

# ---- Figure. SES effect histogram ----



avg = female_log %>%
  full_join(male_log) %>%
  filter(term == "ses") %>%
  mutate(estimate = exp(estimate)) %>%
  group_by(Gender, y.level) %>%
  summarize(m = mean(estimate))
  
female_log %>%
  full_join(male_log) %>%
  filter(term == "ses") %>%
  mutate(estimate = exp(estimate)) %>%
  ggplot(aes(estimate, fill = Gender)) +
  geom_histogram(color = "white", bins = 10) +
  geom_vline(aes(xintercept = 1), linetype = 2) +
  geom_vline(aes(xintercept = m), data = avg) +
  facet_grid(model~Gender + y.level) +
  guides(fill = FALSE) +
  scale_x_continuous("Socioeconomic Status Odds Ratio Estimate") +
  scale_fill_manual(values = colors[2:3])

# ---- Figure. SES effect histogram Female ----

load("data/logistic_output.Rdata")

colors = RColorBrewer::brewer.pal(n = 3, "Dark2")

female_log = female_log %>%
  mutate(Gender = "Female") %>%
  select(trait_name, model, coef, Gender) %>%
  unnest(cols = c(coef))

avg = female_log %>%
  filter(term == "ses") %>%
  #mutate(estimate = exp(estimate)) %>%
  group_by(y.level) %>%
  summarize(m = mean(estimate),
            v = sd(estimate))

female_log %>%
  filter(term == "ses") %>%
  #mutate(estimate = exp(estimate)) %>%
  ggplot(aes(estimate)) +
  geom_histogram(color = "white", bins = 10, fill = colors[2]) +
  geom_vline(aes(xintercept = 1), linetype = 2) +
  geom_vline(aes(xintercept = m), data = avg) +
  geom_label(aes(x = m, y = 24, label = paste("M =", round(m,2))), data = avg)+ 
  geom_label(aes(x = m, y = 20, label = paste("SD =", round(v,2))), data = avg)+ 
  facet_grid(model ~ y.level) +
  guides(fill = FALSE) +
  labs(
    title = "SES coefficient (Females)",
    x = "Socioeconomic Status Odds Ratio Estimate")


# ---- Figure. SES effect histogram Male ----

load("data/logistic_output.Rdata")

colors = RColorBrewer::brewer.pal(n = 3, "Dark2")

male_log = male_log %>%
  mutate(Gender = "Male") %>%
  select(trait_name, model, coef, Gender) %>%
  unnest(cols = c(coef))

avg = male_log %>%
  filter(term == "ses") %>%
  #mutate(estimate = exp(estimate)) %>%
  group_by(y.level) %>%
  summarize(m = mean(estimate),
            v = sd(estimate))

male_log %>%
  filter(term == "ses") %>%
  #mutate(estimate = exp(estimate)) %>%
  ggplot(aes(estimate)) +
  geom_histogram(color = "white", bins = 10, fill = colors[3]) +
  geom_vline(aes(xintercept = 1), linetype = 2) +
  geom_vline(aes(xintercept = m), data = avg) +
  geom_label(aes(x = m, y = 24, label = paste("M =", round(m,2))), data = avg)+ 
  geom_label(aes(x = m, y = 20, label = paste("SD =", round(v,2))), data = avg)+ 
  facet_grid(model ~ y.level) +
  guides(fill = FALSE) +
  labs(
    title = "SES coefficient (Males)",
    x = "Socioeconomic Status Odds Ratio Estimate") 
