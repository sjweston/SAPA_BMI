# ------------------------------------
# load packages                      #
# ------------------------------------

packages = c("tidyverse", "knitr", "kableExtra", "papaja")
lapply(packages, library, character.only = TRUE)
rm(packages)

# ------------------------------------
# Table. Correlations              #
# ------------------------------------

load("data/cor_output.Rdata")

# format names of predictors
names = cor.data$pred
names = gsub("cog", "Cognitive Ability", names)
names = gsub("edu", "Parental Education", names)
names = gsub("income", "Parental Income", names)
names[] = gsub("_135_27_5_", "27: ", names)

names = str_replace(names, "27: Extra", "5: Extraversion")
names = str_replace(names, "27: Agree", "5: Agreeableness")
names = str_replace(names, "27: Consc", "5: Conscientiousness")
names = str_replace(names, "27: Neuro", "5: Neuroticism")
names = str_replace(names, "27: Open", "5: Openness")

cor.data %>%
  mutate(pred = names) %>%
  mutate(female_rp = printp(female_rp)) %>%
  mutate(male_rp = printp(male_rp)) %>%
  arrange(pred) %>%
  mutate(pred = gsub("SPI27: ","", pred),
         pred = gsub("SPI5: ","", pred)) %>%
  kable(., digits = 2, col.names = c("Predictor", rep(c("r", "p"),2)),
        row.names = FALSE) %>%
  kable_styling() %>%
  add_header_above(c(" ", "Female" = 2, "Male" = 2)) %>%
  group_rows("SPI: 27 Factors", 4, 30) %>%
  group_rows("SPI: 5 Factors", 31, 35)

  