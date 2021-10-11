# ---- load packages  ----


packages = c("tidyverse", "here", "ggpubr")
lapply(packages, library, character.only = TRUE)
rm(packages)

colors = RColorBrewer::brewer.pal(n = 3, "Dark2")

load(here("data/regression_output_male.Rdata"))
load(here("data/regression_output_female.Rdata"))

female_reg = female_reg %>%
  mutate(Gender = "Female")  %>%
  ungroup()

male_reg = male_reg %>%
  mutate(Gender = "Male")  %>%
  ungroup()

# ---- Avg Female SES effect specification additive ----

avg_female = female_reg %>%
  filter(term == "ses") %>%
  filter(model == "cov") %>%
  summarize(mean = mean(estimate))

# ---- Figure. Female SES effect specification additive ----

female_plot_1 = female_reg %>%
  filter(term == "ses") %>%
  filter(model == "cov") %>%
  mutate(psig = ifelse(p.value < .05, "yes", "no")) %>%
  arrange(estimate) %>%
  mutate(spec = row_number()) %>%
  ggplot(aes(x = spec, y = conf.low)) +
  geom_segment(aes(xend = spec, yend = conf.high, color = psig)) + 
  #geom_point(aes(y = estimate, color = "grey")) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  geom_hline(aes(yintercept = mean), data = avg_female) +
  #geom_label(aes(x = 25, y = 1.25, label = round(mean,2)), data = avg_female )+
  scale_color_manual(values = c("red", "grey")) +
  scale_y_continuous(limits = c(-5.5, 0.25), breaks = c(-5:0))+
  labs(x = "Specification",
       y = "SES coefficient, controlling for personality", 
       title = "Adolescent Girls") +
  guides(color = F) +
  theme_pubr()

female_plot_1

# ---- Avg Female SES effect specification interaction ----

avg_female = female_reg %>%
  filter(grepl(":", term)) %>%
  summarize(mean = mean(estimate))

# ---- Figure. Female SES effect specification interaction ----

female_plot_2 = female_reg %>%
  filter(grepl(":", term)) %>%
  mutate(psig = ifelse(p.value < .05, "yes", "no")) %>%
  arrange(estimate) %>%
  mutate(spec = row_number()) %>%
  ggplot(aes(x = spec, y = conf.low)) +
  geom_segment(aes(xend = spec, yend = conf.high, color = psig)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
  geom_hline(aes(yintercept = mean), data = avg_female, color = "black") +
  #geom_label(aes(x = 25, y = 1.25, label = round(mean,2)), data = avg_female )+
  scale_color_manual(values = c("grey", "red")) +
  #scale_y_continuous(limits = c(0.30, 2.20))+
  labs(x = "Specification",
       y = "SES x perosnality term in model", title = "Adolescent Girls") +
  guides(color = F) +
  theme_pubr()

female_plot_2

# ---- Avg Male SES effect specification additive ----

avg_male = male_reg %>%
  filter(term == "ses") %>%
  filter(model == "cov") %>%
  summarize(mean = mean(estimate))

# ---- Figure. Male SES effect specification additive ----

male_plot_1 = male_reg %>%
  filter(term == "ses") %>%
  filter(model == "cov") %>%
  mutate(psig = ifelse(p.value < .05, "yes", "no")) %>%
  arrange(estimate) %>%
  mutate(spec = row_number()) %>%
  ggplot(aes(x = spec, y = conf.low)) +
  geom_segment(aes(xend = spec, yend = conf.high, color = psig)) + 
  #geom_point(aes(y = estimate)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
  geom_hline(aes(yintercept = mean), data = avg_male, color = "black") +
  #geom_label(aes(x = 25, y = 1.25, label = round(mean,2)), data = avg_male )+
  scale_color_manual(values = c("red", "grey")) +
  scale_y_continuous(limits = c(-5.5, .25), breaks = c(-5:0))+
  labs(x = "Specification", title = "Adolescent Boys", y = NULL) +
  guides(color = F) +
  theme_pubr()

male_plot_1

# ---- Avg Male SES effect specification interaction ----

avg_male = male_reg %>%
  filter(grepl(":", term)) %>%
  summarize(mean = mean(estimate))

# ---- Figure. Male SES effect specification interaction ----

male_plot_2 = male_reg %>%
  filter(grepl(":", term)) %>%
  mutate(psig = ifelse(p.value < .05, "yes", "no")) %>%
  arrange(estimate) %>%
  mutate(spec = row_number()) %>%
  ggplot(aes(x = spec, y = conf.low)) +
  geom_segment(aes(xend = spec, yend = conf.high, color = psig)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
  geom_hline(aes(yintercept = mean), data = avg_male, color = "black") +
  #geom_label(aes(x = 25, y = 1.25, label = round(mean,2)), data = avg_male )+
  scale_color_manual(values = c("grey", "red")) +
  #scale_y_continuous(limits = c(-5.5, .25))+
  labs(x = "Specification",
       y = "Odds Ratio of SES x perosnality term in model") +
  guides(color = F) +
  theme_pubr()

male_plot_2


# both plots together -----------------------------------------------------

ggarrange(female_plot_1, male_plot_1)
ggsave(here("figures/SES_specification.jpeg"), width = 6, height = 4)

