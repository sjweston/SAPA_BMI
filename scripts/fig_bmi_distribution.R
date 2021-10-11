library(tidyverse)
library(here)
library(ggpubr)

load("data/cleaned.Rdata")

dens_f = density(sapa_female$BMI_p)
df_f = data.frame(BMI = dens_f$x, 
                  y = dens_f$y,
                  gender = "Adolescent Girls")

dens_m = density(sapa_male$BMI_p)
df_m = data.frame(BMI = dens_m$x, 
                  y = dens_m$y,
                  gender = "Adolescent Boys")
df_f = df_f %>%
  full_join(df_m) %>%
  mutate(quantile = case_when(
    BMI < 5 ~ "Underweight",
    BMI < 85 ~ "Normal",
    BMI < 95 ~ "Overweight",
    TRUE ~ "Obese"))

df_f %>%
  ggplot(aes(x = BMI, y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quantile)) + 
  scale_fill_brewer() +
  scale_x_continuous(limits = c(0,100)) +
  labs(x = "BMI Percentile", y = "Density", fill = "CDC weight categories") +
  facet_wrap(~gender) +
  theme_pubr()
ggsave(here("figures/BMI distributions.jpeg"), width = 7.5, height = 4.5)
