# ---- allcode  ----


packages = c("tidyverse", "here", "ggpubr", "ggridges")
lapply(packages, library, character.only = TRUE)
rm(packages)

colors = RColorBrewer::brewer.pal(n = 3, "Dark2")

load(here("data/regression_output_male.Rdata"))
load(here("data/regression_output_female.Rdata"))
female_reg_original = mutate(female_reg, version = "original")
male_reg_original = mutate(male_reg, version = "original")


load(here("data/regression_impute_male.Rdata"))
load(here("data/regression_impute_female.Rdata"))
female_reg_impute = mutate(female_reg, version = "impute")
male_reg_impute = mutate(male_reg, version = "impute")

source("scripts/personality_scales.R")

names(SPI_5_names) = str_remove(names(SPI_5_names), "135_27_5_")
names(SPI_27_names) = str_remove(names(SPI_27_names), "135_27_5_")

female_reg = female_reg_original %>%
  full_join(female_reg_impute) %>%
  mutate(Gender = "Female")  %>%
  ungroup()

male_reg = male_reg_original %>%
  full_join(male_reg_impute) %>%
  mutate(Gender = "Male")  %>%
  ungroup()

female_reg %>%
  full_join(male_reg) %>%
  filter(
    (term == "trait_score" & model == "cov")) %>%
  select(trait_name, term, model, version, estimate, Gender) %>%
  spread(version, estimate) %>%
  mutate(difference = impute-original,
         diff_ratio = abs(difference/original)) %>%
  gather(metric, value, contains("diff")) %>%
  filter(metric == "difference") %>%
  ggplot(aes(x = value, 
             y = Gender)) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  guides(fill = F)+
  labs(x = "Change in parameter after imputation", y = NULL) +
  theme_pubr()

ggsave(here("figures/sensitivity_impute_ridge.jpeg"), width = 6, height = 4)

female_reg %>%
  full_join(male_reg) %>%
  filter(
    (term == "trait_score" & model == "cov") ) %>%
  select(trait_name, term, model, version, estimate, Gender) %>%
  spread(version, estimate) %>%
  mutate(difference = impute-original,
         diff_ratio = abs(difference/original),
         trait_name = factor(trait_name, 
                             levels = c("cog", names(SPI_27_names), names(SPI_5_names)),
                             labels = c("Cognitive Ability", SPI_27_names, SPI_5_names))) %>%
  gather(metric, value, contains("diff")) %>%
  filter(metric == "difference") %>%
  ggplot(aes(x = reorder(trait_name, original))) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey") +
  geom_segment(aes(xend = trait_name, y = original, yend=impute)) +
  geom_point(aes(y = original), color = "black") +
  geom_point(aes(y = impute, color = value)) +
  scale_color_gradient2(low = "midnightblue", 
                        mid = "grey", 
                        high = "orange", 
                        midpoint = 0) +
  guides(color = F) +
  labs(x = NULL, 
       y = "Coefficient Estimate") +
  coord_flip() +
  facet_grid(.~Gender) +
  theme_pubr(base_size = 8)

ggsave(here("figures/sensitivity_impute_dots.jpeg"), width = 6, height = 8)

female_reg %>%
  full_join(male_reg) %>%
  filter(
    (term == "trait_score" & model == "cov") ) %>%
  select(trait_name, term, model, version, estimate, p.value, Gender) %>%
  gather(stat, stat_v, estimate, p.value) %>%
  unite(version, version, stat) %>%
  spread(version, stat_v) %>%
  mutate(difference = impute_estimate-original_estimate,
         diff_ratio = abs(difference/original_estimate),
         trait_name = factor(trait_name, 
                             levels = c("cog", names(SPI_27_names), names(SPI_5_names)),
                             labels = c("Cognitive Ability", SPI_27_names, SPI_5_names)),
         impute_sig = ifelse(impute_p.value < .05, "yes", "no")) %>%
  gather(metric, value, contains("diff")) %>%
  filter(metric == "difference") %>%
  ggplot(aes(x = reorder(trait_name, original_estimate))) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey") +
  geom_bar(aes(y = value, fill = impute_sig), stat = "identity")+
  # scale_fill_gradient2(low = "midnightblue", 
  #                       mid = "grey", 
  #                       high = "orange", 
  #                       midpoint = 0) +
  scale_fill_manual(values = c("grey", "red")) +
  coord_flip() +
  labs(x = NULL, y = "Change in regression coefficient") +
  guides(fill = F)+
  facet_grid(.~Gender) +
  theme_pubr(base_size = 8)

#ggsave(here("figures/sensitivity_impute_dots.jpeg"), width = 6, height = 8)


female_plot = female_reg %>%
  filter(
    (term == "trait_score" & model == "cov") ) %>%
  select(-std.error, -statistic) %>%
  gather(stat, stat_v, estimate, p.value, contains("conf")) %>%
  unite(version, version, stat) %>%
  spread(version, stat_v)  %>%
  mutate(psig = ifelse(impute_p.value < .05, "yes", "no"),
         osig = ifelse(original_p.value < .05, "significant", "not significant"),
         est_r = papaja::printnum(impute_estimate),
         yloc = ifelse(impute_estimate > 0, impute_conf.high + .5, impute_conf.low - .5),
         trait_name = factor(trait_name, 
                             levels = c("cog", names(SPI_27_names), names(SPI_5_names)),
                             labels = c("Cognitive Ability", SPI_27_names, SPI_5_names))) %>% 
  ggplot(aes(x = reorder(trait_name, original_estimate), y = impute_estimate)) +
  geom_bar(stat = "identity", aes(fill = psig)) +
  geom_hline(aes(yintercept = 0), color = "grey") +
  #geom_text(aes(label = est_r, y = yloc)) +
  scale_fill_manual(values = c("grey", "orange")) +
  scale_y_continuous(limits = c(-4.5, 4.5)) +
  geom_errorbar(aes(ymin = impute_conf.low, ymax = impute_conf.high), width = .3)+
  geom_point(aes(y = original_estimate, color = osig)) +
  scale_color_manual(values = c("grey", "red")) +
  guides(fill = F) +
  labs(y = "Regression coefficient after imputation", x = NULL, color = "Original coefficient") +
  coord_flip() +
  theme_pubr(base_size = 10)

male_plot = male_reg %>%
  filter(
    (term == "trait_score" & model == "cov") ) %>%
  select(-std.error, -statistic) %>%
  gather(stat, stat_v, estimate, p.value, contains("conf")) %>%
  unite(version, version, stat) %>%
  spread(version, stat_v)  %>%
  mutate(psig = ifelse(impute_p.value < .05, "yes", "no"),
         osig = ifelse(original_p.value < .05, "significant", "not significant"),
         est_r = papaja::printnum(impute_estimate),
         yloc = ifelse(impute_estimate > 0, impute_conf.high + .5, impute_conf.low - .5),
         trait_name = factor(trait_name, 
                             levels = c("cog", names(SPI_27_names), names(SPI_5_names)),
                             labels = c("Cognitive Ability", SPI_27_names, SPI_5_names))) %>% 
  ggplot(aes(x = reorder(trait_name, original_estimate), y = impute_estimate)) +
  geom_bar(stat = "identity", aes(fill = psig)) +
  geom_hline(aes(yintercept = 0), color = "grey") +
  #geom_text(aes(label = est_r, y = yloc)) +
  scale_fill_manual(values = c("grey", "orange")) +
  scale_y_continuous(limits = c(-4.5, 4.5)) +
  geom_errorbar(aes(ymin = impute_conf.low, ymax = impute_conf.high), width = .3)+
  geom_point(aes(y = original_estimate, color = osig)) +
  scale_color_manual(values = c("grey", "red")) +
  guides(fill = F) +
  labs(y = "Regression coefficient after imputation", x = NULL, color = "Original coefficient") +
  coord_flip() +
  theme_pubr(base_size = 10)

ggarrange(female_plot, male_plot, ncol = 2, labels = c("Female", "Male"))
ggsave(here("figures/regression_imputed_compare.jpeg"), width = 18, height = 10)
