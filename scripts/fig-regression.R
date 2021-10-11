# ---- allcode  ----


packages = c("tidyverse", "here", "ggpubr")
lapply(packages, library, character.only = TRUE)
rm(packages)

colors = RColorBrewer::brewer.pal(n = 3, "Dark2")

load(here("data/regression_output_male.Rdata"))
load(here("data/regression_output_female.Rdata"))
source("scripts/personality_scales.R")

names(SPI_5_names) = str_remove(names(SPI_5_names), "135_27_5_")
names(SPI_27_names) = str_remove(names(SPI_27_names), "135_27_5_")

female_reg = female_reg %>%
  mutate(Gender = "Female")  %>%
  ungroup()

male_reg = male_reg %>%
  mutate(Gender = "Male")  %>%
  ungroup()

female_plot = female_reg %>%
  filter(term == "trait_score") %>%
  filter(model == "cov") %>%
  mutate(psig = ifelse(p.value < .05, "yes", "no"),
         est_r = papaja::printnum(estimate),
         yloc = ifelse(estimate > 0, conf.high + .5, conf.low - .5),
         trait_name = factor(trait_name, 
                             levels = c("cog", names(SPI_27_names), names(SPI_5_names)),
                             labels = c("Cognitive Ability", SPI_27_names, SPI_5_names))) %>%
  ggplot(aes(x = reorder(trait_name, estimate), y = estimate)) +
  geom_bar(stat = "identity", aes(fill = psig)) +
  geom_hline(aes(yintercept = 0), color = "grey") +
  geom_text(aes(label = est_r, y = yloc)) +
  scale_fill_manual(values = c("grey", "orange")) +
  scale_y_continuous(limits = c(-4.5, 4.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .3)+
  guides(fill = F) +
  labs(y = "Regression Coefficient", x = NULL) +
  coord_flip() +
  theme_pubr(base_size = 10)

male_plot = male_reg %>%
  filter(term == "trait_score") %>%
  filter(model == "cov") %>%
  mutate(psig = ifelse(p.value < .05, "yes", "no"),
         est_r = papaja::printnum(estimate),
         yloc = ifelse(estimate > 0, conf.high + .5, conf.low - .5),
         trait_name = factor(trait_name, 
                             levels = c("cog", names(SPI_27_names), names(SPI_5_names)),
                             labels = c("Cognitive Ability", SPI_27_names, SPI_5_names))) %>%
  ggplot(aes(x = reorder(trait_name, estimate), y = estimate)) +
  geom_bar(stat = "identity", aes(fill = psig)) +
  geom_hline(aes(yintercept = 0), color = "grey") +
  geom_text(aes(label = est_r, y = yloc)) +
  scale_fill_manual(values = c("grey", "orange")) +
  scale_y_continuous(limits = c(-4.5, 4.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .3)+
  guides(fill = F) +
  labs(y = "Regression Coefficient", x = NULL) +
  coord_flip() +
  theme_pubr(base_size = 10)

ggarrange(female_plot, male_plot, ncol = 2, labels = c("Female", "Male"))
ggsave(here("figures/regression_personality.jpeg"), width = 18, height = 10)
