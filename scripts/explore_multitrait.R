
#load packages
packages = c("tidyverse", "broom", "nnet", "rsample", "caret", "ROSE", "here")
lapply(packages, library, character.only = TRUE)
rm(packages)

load(here("data/cleaned.Rdata"))

# set seed
set.seed(090919)

ctrl <- trainControl(method = "repeatedcv", # cross-validation
                     number = 10,  # 10 fold cross validation
                     repeats = 10, #repeated 10 times
                     verboseIter = FALSE,
                     search = "random",
                     sampling = "smote") 

# parition into training and test sets. objects identify just training rows
train_male = createDataPartition(sapa_male$BMI_c, p = .75, list = FALSE)
train_female = createDataPartition(sapa_female$BMI_c, p = .75, list = FALSE)

#standardize predictors
pred = names(sapa_male)[grepl("SPI", names(sapa_male))]
all_pred = c(pred)
big5_pred = c(pred[1:5])
fa27_pred = c(pred[-c(1:5)])

sapa_male = sapa_male %>%
  mutate(BMI_c = factor(BMI_c, levels = c("Normal Weight", "Underweight", "Overweight", "Obese"))) %>%
  mutate_at(all_pred, scale)

sapa_female = sapa_female %>%
  mutate(BMI_c = factor(BMI_c, levels = c("Normal Weight", "Underweight", "Overweight", "Obese"))) %>%
  mutate_at(all_pred, scale)

#model 27 factors
pred_27_male = train(as.formula(paste("BMI_c ~", paste(fa27_pred, collapse = " + "))), 
                     data = sapa_male, 
                     subset = train_male, 
                     method = "multinom",
                     maxit= 1000,
                     na.action = "na.exclude", 
                     trControl = ctrl)

pred_27_female = train(as.formula(paste("BMI_c ~", paste(fa27_pred, collapse = " + "))), 
                     data = sapa_female, 
                     subset = train_female, 
                     method = "multinom",
                     maxit= 1000,
                     na.action = "na.exclude", 
                     trControl = ctrl)

#model 5 factors
pred_b5_male = train(as.formula(paste("BMI_c ~", paste(big5_pred, collapse = " + "))), 
                     data = sapa_male, 
                     subset = train_male, 
                     method = "multinom",
                     maxit= 1000,
                     na.action = "na.exclude", 
                     trControl = ctrl)

pred_b5_female = train(as.formula(paste("BMI_c ~", paste(big5_pred, collapse = " + "))), 
                       data = sapa_female, 
                       subset = train_female, 
                       method = "multinom",
                       maxit= 1000,
                       na.action = "na.exclude", 
                       trControl = ctrl)

# accuracy in test model
accuracy_27_male = predict(pred_27_male, newdata = sapa_male[-train_male, ], na.action = "na.pass")
accuracy_27_male = confusionMatrix(accuracy_27_male, sapa_male[-train_male,"BMI_c"])

accuracy_27_female = predict(pred_27_female, newdata = sapa_female[-train_female, ], na.action = "na.pass")
accuracy_27_female = confusionMatrix(accuracy_27_female, sapa_female[-train_female,"BMI_c"])

accuracy_b5_male = predict(pred_b5_male, newdata = sapa_male[-train_male, ], na.action = "na.pass")
accuracy_b5_male = confusionMatrix(accuracy_b5_male, sapa_male[-train_male,"BMI_c"])

accuracy_b5_female = predict(pred_b5_female, newdata = sapa_female[-train_female, ], na.action = "na.pass")
accuracy_b5_female = confusionMatrix(accuracy_b5_female, sapa_female[-train_female,"BMI_c"])

# most important variables 

imp_27_male = varImp(pred_27_male, scale = F)
imp_27_female = varImp(pred_27_female, scale = F)

imp_5_male = varImp(pred_b5_male, scale = F)
imp_5_female = varImp(pred_b5_female, scale = F)

save(accuracy_27_female, accuracy_27_male, accuracy_b5_female, accuracy_b5_male, 
     imp_27_female, imp_27_male, imp_5_female, imp_5_male, 
     file = here("data/multitrait.Rdata"))
