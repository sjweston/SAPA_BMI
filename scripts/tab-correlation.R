# ------------------------------------
# load packages                      #
# ------------------------------------

packages = c("tidyverse", "knitr", "kableExtra", "papaja")
lapply(packages, library, character.only = TRUE)
rm(packages)

source("scripts/personality_scales.R")

# ------------------------------------
# Table. Correlations              #
# ------------------------------------

load("data/cor_output.Rdata")

# format names of predictors
names = cor.data$pred
names = gsub("cog", "Cognitive Ability", names)
names = gsub("edu", "Parental Education", names)
names = gsub("income", "Parental Income", names)
names[grepl("spi", names)] = c(SPI_27_names, SPI_5_names)

spi27_rows = which(grepl("27", cor.data$pred))
spi5_rows = which(grepl("spi5", cor.data$pred))


cor.data %>%
  mutate(pred = names) %>%
  mutate(female_rp = printp(female_rp)) %>%
  mutate(male_rp = printp(male_rp)) %>%
  kable(., digits = 2, col.names = c("Predictor", rep(c("r", "p"),2)),
        row.names = FALSE) %>%
  kable_styling() %>%
  add_header_above(c(" ", "Female" = 2, "Male" = 2)) %>%
  group_rows("SPI: 27 Factors", min(spi27_rows), max(spi27_rows)) %>%
  group_rows("SPI: 5 Factors", min(spi5_rows), max(spi5_rows))

  