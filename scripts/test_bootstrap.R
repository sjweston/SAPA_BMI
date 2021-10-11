library(caret)
library(ROSE)
ctrl <- trainControl(method = "repeatedcv", # cross-validation
                     number = 10,  # 10 fold cross validation
                     repeats = 10, #repeated 10 times
                     verboseIter = FALSE,
                     sampling = "up") #resample from minority categories to create balanced classes

set.seed(42)
model_rf_over <- caret::train(BMI_c ~ cog*income, 
                              data = sapa_male[train_male, ],
                              method = "multinom",
                              na.action = "na.pass",
                              trControl = ctrl)

predicted = predict(model_rf_over, newdata = sapa_male[-train_male, ], na.action = "na.pass")
confusionMatrix(sapa_male[-train_male, "BMI_c"], predicted)


#-------------------------------------------------

male_log = sapa_male_trait %>%
  # use 
  mutate(cov_edu = map(data, ~caret::train(BMI_c ~ trait_score + edu, data = ., 
                                    subset = train_male, 
                                    method = "multinom",
                                    na.action = "na.exclude", 
                                    trControl = ctrl))) %>%
  mutate(int_edu = map(data, ~train(BMI_c ~ trait_score*edu, data = ., 
                                    subset = train_male, 
                                    method = "multinom",
                                    na.action = "na.exclude", 
                                    trControl = ctrl))) %>%
  mutate(cov_inc = map(data, ~train(BMI_c ~ trait_score + income, data = ., 
                                    subset = train_male, 
                                    method = "multinom",
                                    na.action = "na.exclude", 
                                    trControl = ctrl))) %>%
  mutate(int_inc = map(data, ~train(BMI_c ~ trait_score*income, data = ., 
                                    subset = train_male, 
                                    method = "multinom",
                                    na.action = "na.exclude", 
                                    trControl = ctrl))) 

male_log = male_log %>%
  gather("model", "output", cov_edu, int_edu, cov_inc, int_inc) %>%
  # create test data from all rows not used in training
  mutate(test_data = map(data, .f = function(x) x[-train_male, ]),
         #extract reference (true) BMI categories from test data
         test_reference = map(test_data, "BMI_c"),
         # predict categories from model output; na.pass puts NAs in any row with missing data
         predicted = map2(output, test_data, predict, na.action = "na.pass"),
         # calculate accuracy, sensitivity, specificity, etc
         confusion = map2(predicted, test_reference, confusionMatrix),
         # extract final model coefficients
         final_mod = map(output, "finalModel"),
         # tidy output for printing
         coef = map(final_mod, broom::tidy, coef = TRUE))

test = male_log %>%
  separate(model, into = c("model", "ses")) %>%
  dplyr::select(trait_name, model, ses, final_mod) %>%
  spread(ses, final_mod) %>%
  mutate(plot_edu = map(edu, ggeffects::ggpredict, terms = c("trait_score", "edu[meansd]"))) %>%
  mutate(plot_inc = map(inc, sjPlot::plot_model, type = "pred", terms = c("trait_score", "income[meansd]"))) %>%
  mutate(plot_edu = map(plot_edu, "data")) %>%
  mutate(plot_inc = map(plot_inc, "data")) 
