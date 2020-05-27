# ----- load packages and data -------
packages = c("here", "tidyverse", "broom", "nnet", "rsample", "caret", "ROSE")
lapply(packages, library, character.only = TRUE)
rm(packages)

load(here("data/accuracy.Rdata"))

b5 = data.frame(gender = c("Female", "Male"))

b5$varImp = list(
  varImp(female_ses_27$finalModel),
  varImp(male_ses_27$finalModel))

b5 = b5 %>%
  mutate(varImp = map(varImp, function(x) x = cbind(x, variable = rownames(x)))) %>%
  unnest(cols = c(varImp)) %>%
  group_by(gender) %>%
  arrange(gender, Overall) %>%
  ungroup() %>%
  mutate(number = row_number()) %>%
  mutate(variable = gsub("SPI_", "", variable))

b5 %>%
  ggplot(aes(x = number, y = Overall)) +
  geom_point(size = 3) +
  geom_segment(aes(xend = number, yend = 0)) +
  facet_wrap(~gender, scales = "free") +
  scale_x_continuous(
    breaks = b5$number,
    labels = b5$variable, 
    expand = c(0, 0)) +
  coord_flip()
