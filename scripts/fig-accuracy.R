# ----- load packages and data -------
packages = c("here", "tidyverse", "broom", "nnet", "rsample", "caret", "ROSE", "pROC")
lapply(packages, library, character.only = TRUE)
rm(packages)

load(here("data/accuracy.Rdata"))

acc_data = expand.grid(gender = c("Female", "Male"),
                       model = c("SES Only", "SES + Big Five", "SES + SPI 27"))
acc_data$CM = list(acc_female_ses, acc_male_ses,
                   acc_female_ses_b5, acc_male_ses_b5,
                   acc_female_ses_27, acc_male_ses_27)


# ---- accuracy figure -------

acc_fig = acc_data %>%
  mutate(Overall = map(CM, "overall")) %>%
  mutate(Accuarcy = map_dbl(Overall, "Accuracy"),
         Acc_lower = map_dbl(Overall, "AccuracyLower"),
         Acc_upper = map_dbl(Overall, "AccuracyUpper")) %>%
  ggplot(aes(x = model, y = Accuarcy, fill = gender)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = Acc_lower, ymax = Acc_upper), position = position_dodge(.9), width = .2, color = "black") +
  ggtitle("Overall accuracy by model and gender") +
  scale_y_continuous("")+
  theme(legend.position = "top")

# ---- kappa figure -------

kappa_fig = acc_data %>%
  mutate(Overall = map(CM, "overall")) %>%
  mutate(Kappa = map_dbl(Overall, "Kappa")) %>%
  ggplot(aes(x = model, y = Kappa, fill = gender)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Kappa by model and gender") +
  scale_y_continuous("")+
  theme(legend.position = "top")


# ---- sensitivity figure -------

sens_fig = acc_data %>%
  mutate(ByClass = map(CM, "byClass")) %>%
  mutate(Sensitivity = map(ByClass, function(x) x[,"Sensitivity"])) %>%
  select(model, gender, Sensitivity) %>%
  mutate(Class = map(Sensitivity, names)) %>%
  unnest(cols = c(Sensitivity, Class)) %>%
  mutate(Class = gsub("Class\\:", "", Class)) %>%
  ggplot(aes(x = model, y = Sensitivity, fill = gender)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Sensitivity by class, model, and gender") +
  scale_y_continuous("")+
  facet_wrap(~Class) +
  theme(legend.position = "top")

# ---- specificity figure -------

spec_fig = acc_data %>%
  mutate(ByClass = map(CM, "byClass")) %>%
  mutate(Specificity = map(ByClass, function(x) x[,"Specificity"])) %>%
  select(model, gender, Specificity) %>%
  mutate(Class = map(Specificity, names)) %>%
  unnest(cols = c(Specificity, Class)) %>%
  mutate(Class = gsub("Class\\:", "", Class)) %>%
  ggplot(aes(x = model, y = Specificity, fill = gender)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~Class) +
  ggtitle("Specificity by class, model, and gender") +
  scale_y_continuous("")+
  theme(legend.position = "top")

# ---- balanced accuracy -------

balance_fig = acc_data %>%
  mutate(ByClass = map(CM, "byClass")) %>%
  mutate(Balance = map(ByClass, function(x) x[,"Balanced Accuracy"])) %>%
  select(model, gender, Balance) %>%
  mutate(Class = map(Balance, names)) %>%
  unnest(cols = c(Balance, Class)) %>%
  mutate(Class = gsub("Class\\:", "", Class)) %>%
  ggplot(aes(x = model, y = Balance, fill = gender)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~Class) +
  ggtitle("Balanced Accuracy by class, model, and gender") +
  scale_y_continuous("")+
  theme(legend.position = "top")

# ---- AUC figure ------

model_fits = list(female_ses = female_ses, female_ses_b5 = female_ses_b5, female_ses_27 = female_ses_27,
                  male_ses =   male_ses,   male_ses_b5 =   male_ses_b5,   male_ses_27 =   male_ses_27)

pred.v.actual <- data.frame(model = names(model_fits), stringsAsFactors = FALSE) %>% 
  mutate(output = model_fits) %>% 
  mutate(predicted = map(output, predict, type = "prob")) %>% 
  mutate(actual = map(output, "trainingData")) %>%
  mutate(actual = map(actual, function(x) x[,".outcome"])) %>% 
  mutate(multiclass_roc = map2(actual, predicted, multiclass.roc)) %>%
  mutate(auc = map_dbl(multiclass_roc, "auc")) %>%
  mutate(sex = ifelse(grepl("female", model), "female", "male")) %>%
  mutate(model = gsub(".*_", "", model),
         model = factor(model, levels = c("ses", "b5", "27"),
                        labels = c("SES Only", 
                                   "SES + Big Five", 
                                   "SES + SPI27"))) 

pred.v.actual %>% ggplot(aes(x = sex, y = auc)) +
  geom_bar(stat = "identity", 
           alpha = .7, 
           color = "black",
           aes(fill = model),
           position = "dodge") +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "", y = "Area Under Curve (AUC)")
