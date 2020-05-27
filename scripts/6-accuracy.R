# ------------------------------------
# load packages and data             #
# ------------------------------------

#load packages
packages = c("tidyverse", "broom", "nnet", "rsample", "caret", "ROSE", "pROC")
lapply(packages, library, character.only = TRUE)
rm(packages)

load("data/cleaned.Rdata")

# set seed
set.seed(090919)

ctrl <- trainControl(method = "repeatedcv", # cross-validation
                     number = 10,  # 10 fold cross validation
                     repeats = 10, #repeated 10 times
                     verboseIter = FALSE,
                     search = "random",
                     sampling = "smote") 
# ------------------------------------
# build model: SES                   #
# ------------------------------------

male_ses = train(BMI_c ~ ses, data = sapa_male, 
                 subset = train_male, 
                 method = "multinom",
                 maxit= 1000,
                 na.action = "na.exclude", 
                 trControl = ctrl)

female_ses = train(BMI_c ~ ses, data = sapa_female, 
                   subset = train_male, 
                   method = "multinom",
                   maxit= 1000,
                   na.action = "na.exclude", 
                   trControl = ctrl)

# ------------------------------------
# build model: SES + Big Five        #
# ------------------------------------

male_ses_b5 = train(BMI_c ~ ses + SPI_Agree + SPI_Extra + SPI_Consc + SPI_Neuro + SPI_Open, 
                    data = sapa_male, 
                    subset = train_male, 
                    method = "multinom",
                    maxit= 1000,
                    na.action = "na.exclude", 
                    trControl = ctrl)

female_ses_b5 = train(BMI_c ~ ses + SPI_Agree + SPI_Extra + SPI_Consc + SPI_Neuro + SPI_Open, 
                      data = sapa_female, 
                      subset = train_male, 
                      method = "multinom",
                      maxit= 1000,
                      na.action = "na.exclude", 
                      trControl = ctrl)

# ------------------------------------
# build model: SES + SPI:27          #
# ------------------------------------

male_ses_27 = train(BMI_c ~ ses + SPI_Compassion + SPI_Irritability + SPI_Sociability + 
                      SPI_WellBeing + SPI_SensationSeeking + SPI_Anxiety + SPI_Honesty + 
                      SPI_Industry + SPI_Intellect + SPI_Creativity + SPI_Impulsivity + 
                      SPI_AttentionSeeking + SPI_Order + SPI_Authoritarianism + 
                      SPI_Charisma + SPI_Trust + SPI_Humor + SPI_EmotionalExpressiveness + 
                      SPI_ArtAppreciation + SPI_Introspection + SPI_Perfectionism + 
                      SPI_SelfControl + SPI_Conformity + SPI_Adaptability + 
                      SPI_EasyGoingness + SPI_EmotionalStability + SPI_Conservatism, 
                    data = sapa_male, 
                    subset = train_male, 
                    method = "multinom",
                    maxit= 1000,
                    na.action = "na.exclude", 
                    trControl = ctrl)

female_ses_27 = train(BMI_c ~ ses + SPI_Compassion + SPI_Irritability + SPI_Sociability + 
                        SPI_WellBeing + SPI_SensationSeeking + SPI_Anxiety + SPI_Honesty + 
                        SPI_Industry + SPI_Intellect + SPI_Creativity + SPI_Impulsivity + 
                        SPI_AttentionSeeking + SPI_Order + SPI_Authoritarianism + 
                        SPI_Charisma + SPI_Trust + SPI_Humor + SPI_EmotionalExpressiveness + 
                        SPI_ArtAppreciation + SPI_Introspection + SPI_Perfectionism + 
                        SPI_SelfControl + SPI_Conformity + SPI_Adaptability + 
                        SPI_EasyGoingness + SPI_EmotionalStability + SPI_Conservatism, 
                      data = sapa_female, 
                      subset = train_male, 
                      method = "multinom",
                      maxit= 1000,
                      na.action = "na.exclude", 
                      trControl = ctrl)


# ------------------------------------
# predict in test sets               #
# ------------------------------------

pred_male_ses = predict(male_ses, newdata = sapa_male[-train_male,])
pred_male_ses_b5 = predict(male_ses_b5, newdata = sapa_male[-train_male,])
pred_male_ses_27 = predict(male_ses_27, newdata = sapa_male[-train_male,])

pred_female_ses = predict(female_ses, newdata = sapa_female[-train_female,])
pred_female_ses_b5 = predict(female_ses_b5, newdata = sapa_female[-train_female,])
pred_female_ses_27 = predict(female_ses_27, newdata = sapa_female[-train_female,])

# ------------------------------------
# accuracy and kappa                 #
# ------------------------------------
male_ref = as.factor(sapa_male$BMI_c[-train_male])
full_data = complete.cases(sapa_male[-train_male, grepl("SPI", names(sapa_male))])
acc_male_ses = confusionMatrix(pred_male_ses, reference = male_ref)
acc_male_ses_b5 = confusionMatrix(pred_male_ses_b5, reference = male_ref)
acc_male_ses_27 = confusionMatrix(pred_male_ses_27, reference = male_ref[full_data])

female_ref = as.factor(sapa_female$BMI_c[-train_female])
full_data = complete.cases(sapa_female[-train_female, grepl("SPI", names(sapa_female))])
acc_female_ses = confusionMatrix(pred_female_ses, reference = female_ref)
acc_female_ses_b5 = confusionMatrix(pred_female_ses_b5, reference = female_ref)
acc_female_ses_27 = confusionMatrix(pred_female_ses_27, reference = female_ref[full_data])

save(male_ses, male_ses_b5, male_ses_27,
     acc_male_ses, acc_male_ses_b5, acc_male_ses_27,
     female_ses, female_ses_b5, female_ses_27,
     acc_female_ses, acc_female_ses_b5, acc_female_ses_27, 
     file = here("data/accuracy.Rdata"))
