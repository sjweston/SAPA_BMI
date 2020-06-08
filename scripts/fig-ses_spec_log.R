# ---- load packages  ----


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

# ---- Avg Female SES effect specification additive ----

avg_female = female_log %>%
  filter(term == "ses") %>%
  filter(model == "cov") %>%
  group_by(y.level) %>% 
  summarize(mean = mean(estimate))
  
# ---- Figure. Female SES effect specification additive ----

female_log %>%
  filter(term == "ses") %>%
  filter(model == "cov") %>%
  mutate(psig = ifelse(p.value < .05, "yes", "no")) %>%
  arrange(estimate) %>%
  group_by(y.level) %>%
  mutate(spec = row_number()) %>%
  ggplot(aes(x = spec, y = conf.low)) +
  geom_segment(aes(xend = spec, yend = conf.high, color = psig)) + 
  geom_hline(aes(yintercept = 1), linetype = "dashed") +
  geom_hline(aes(yintercept = mean), data = avg_female) +
  #geom_label(aes(x = 25, y = 1.25, label = round(mean,2)), data = avg_female )+
  scale_color_manual(values = c("gray", "red")) +
  scale_y_continuous(limits = c(0.30, 2.20))+
  labs(x = "Specification",
       y = "Odds Ratio of SES term, controlling for personality") +
  guides(color = F)+
  facet_grid(~y.level) 


# ---- Avg Female SES effect specification interaction ----

avg_female = female_log %>%
  filter(grepl(":", term)) %>%
  group_by(y.level) %>% 
  summarize(mean = mean(estimate))

# ---- Figure. Female SES effect specification interaction ----

female_log %>%
  filter(grepl(":", term)) %>%
  mutate(psig = ifelse(p.value < .05, "yes", "no")) %>%
  arrange(estimate) %>%
  group_by(y.level) %>%
  mutate(spec = row_number()) %>%
  ggplot(aes(x = spec, y = conf.low)) +
  geom_segment(aes(xend = spec, yend = conf.high, color = psig)) + 
  geom_hline(aes(yintercept = 1), linetype = "dashed", color = "black") +
  geom_hline(aes(yintercept = mean), data = avg_female, color = "black") +
  #geom_label(aes(x = 25, y = 1.25, label = round(mean,2)), data = avg_female )+
  scale_color_manual(values = c("gray", "red")) +
  scale_y_continuous(limits = c(0.30, 2.20))+
  labs(x = "Specification",
       y = "Odds Ratio of SES x perosnality term in model") +
  guides(color = F)+
  facet_grid(~y.level) 

# ---- Avg Male SES effect specification additive ----

avg_male = male_log %>%
  filter(term == "ses") %>%
  filter(model == "cov") %>%
  group_by(y.level) %>% 
  summarize(mean = mean(estimate))

# ---- Figure. Male SES effect specification additive ----

male_log %>%
  filter(term == "ses") %>%
  filter(model == "cov") %>%
  mutate(psig = ifelse(p.value < .05, "yes", "no")) %>%
  arrange(estimate) %>%
  group_by(y.level) %>%
  mutate(spec = row_number()) %>%
  ggplot(aes(x = spec, y = conf.low)) +
  geom_segment(aes(xend = spec, yend = conf.high, color = psig)) + 
  geom_hline(aes(yintercept = 1), linetype = "dashed", color = "black") +
  geom_hline(aes(yintercept = mean), data = avg_male, color = "black") +
  #geom_label(aes(x = 25, y = 1.25, label = round(mean,2)), data = avg_male )+
  scale_color_manual(values = c("gray", "red")) +
  scale_y_continuous(limits = c(0.30, 2.20))+
  labs(x = "Specification",
       y = "Odds Ratio of SES term, controlling for personality") +
  guides(color = F)+
  facet_grid(~y.level) 


# ---- Avg Male SES effect specification interaction ----

avg_male = male_log %>%
  filter(grepl(":", term)) %>%
  group_by(y.level) %>% 
  summarize(mean = mean(estimate))

# ---- Figure. Male SES effect specification interaction ----

male_log %>%
  filter(grepl(":", term)) %>%
  mutate(psig = ifelse(p.value < .05, "yes", "no")) %>%
  arrange(estimate) %>%
  group_by(y.level) %>%
  mutate(spec = row_number()) %>%
  ggplot(aes(x = spec, y = conf.low)) +
  geom_segment(aes(xend = spec, yend = conf.high, color = psig)) + 
  geom_hline(aes(yintercept = 1), linetype = "dashed") +
  geom_hline(aes(yintercept = mean), data = avg_male) +
  #geom_label(aes(x = 25, y = 1.25, label = round(mean,2)), data = avg_male )+
  scale_color_manual(values = c("gray", "red")) +
  scale_y_continuous(limits = c(0.30, 2.20))+
  labs(x = "Specification",
       y = "Odds Ratio of SES x perosnality term in model") +
  guides(color = F)+
  facet_grid(~y.level) 
