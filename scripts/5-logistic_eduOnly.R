# ---- load packages and data             ----


#load packages
packages = c("tidyverse", "broom", "nnet", "rsample", "caret", "ROSE")
lapply(packages, library, character.only = TRUE)
rm(packages)

load("data/cleaned_edu.Rdata")

# set seed
set.seed(090919)


ctrl <- trainControl(method = "repeatedcv", # cross-validation
                     number = 10,  # 10 fold cross validation
                     repeats = 10, #repeated 10 times
                     verboseIter = FALSE,
                     search = "random",
                     sampling = "smote") 

# ---- wrangle data for iteration         ----

# end goal of wrangling is a data frame of data frames
# nested dataframes correspond to a single personality trait
# score refers to a participant's score on that trait
# we also standardize each of our variables within gender


sapa_male_trait = sapa_male %>%
  dplyr::select(-starts_with("p1"), -starts_with("p2"), -starts_with("edu")) %>%
  mutate(BMI_c = factor(BMI_c, levels = c("Normal Weight", "Underweight", "Overweight", "Obese"))) %>%
  mutate(set = ifelse(row_number() %in% train_male[,1], "train", "test")) %>%
  gather("trait_name", "trait_score", -ses, -BMI_c, -BMI, -BMI_p, -set) %>%
  group_by(trait_name, set) %>%
  mutate(trait_score = scale(trait_score)) %>%
  ungroup() %>%
  group_by(trait_name) %>%
  nest()

sapa_female_trait = sapa_female %>%
  dplyr::select(-starts_with("p1"), -starts_with("p2"), -starts_with("edu")) %>%
  mutate(BMI_c = factor(BMI_c, levels = c("Normal Weight", "Underweight", "Overweight", "Obese"))) %>%
  mutate(set = ifelse(row_number() %in% train_male[,1], "train", "test")) %>%
  gather("trait_name", "trait_score", -ses, -BMI_c, -BMI, -BMI_p, -set) %>%
  group_by(trait_name, set) %>%
  mutate(trait_score = scale(trait_score)) %>%
  ungroup() %>%
  group_by(trait_name) %>%
  nest()



# ---- ordered logistic regression iteration (males)         ----

male_ses_only = train(BMI_c ~ ses, data = sapa_male, 
                      subset = train_male, 
                      method = "multinom",
                      maxit= 1000,
                      na.action = "na.exclude", 
                      trControl = ctrl)

accuracy = predict(male_ses_only, type="raw", newdata=sapa_male[-train_male, ])
postResample(sapa_male[-train_male, "BMI_c"], accuracy)

male_log = sapa_male_trait %>%
  # train models on training subset; use mulintomial logistic regression; use specific formula
  mutate(
    cov = map(data, ~train(BMI_c ~ trait_score + ses, data = ., 
                           subset = train_male, 
                           method = "multinom",
                           na.action = "na.exclude", 
                           trControl = ctrl)),
    int = map(data, ~train(BMI_c ~ trait_score*ses, data = ., 
                           subset = train_male, 
                           method = "multinom",
                           na.action = "na.exclude", 
                           trControl = ctrl))) %>%
  gather("model", "output", cov, int) %>%
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
         coef = map(final_mod, broom::tidy, conf.int = TRUE))

# ---- ordered logistic regression iteration (females)           ----

female_ses_only = train(BMI_c ~ ses, data = sapa_female, 
                        subset = train_female, 
                        method = "multinom",
                        na.action = "na.exclude", 
                        trControl = ctrl)


female_log = sapa_female_trait %>%
  # train models on training subset; use mulintomial logistic regression; use specific formula
  mutate(
    cov = map(data, ~train(BMI_c ~ trait_score + ses, data = ., 
                           subset = train_female, 
                           method = "multinom",
                           na.action = "na.exclude", 
                           trControl = ctrl)),
    int = map(data, ~train(BMI_c ~ trait_score*ses, data = ., 
                           subset = train_female, 
                           method = "multinom",
                           na.action = "na.exclude", 
                           trControl = ctrl))) %>%
  gather("model", "output", cov, int) %>%
  # create test data from all rows not used in training
  mutate(test_data = map(data, .f = function(x) x[-train_female, ]),
         #extract reference (true) BMI categories from test data
         test_reference = map(test_data, "BMI_c"),
         # predict categories from model output; na.pass puts NAs in any row with missing data
         predicted = map2(output, test_data, predict, na.action = "na.pass"),
         # calculate accuracy, sensitivity, specificity, etc
         confusion = map2(predicted, test_reference, confusionMatrix),
         # extract final model coefficients
         final_mod = map(output, "finalModel"),
         # tidy output for printing
         coef = map(final_mod, broom::tidy, conf.int = TRUE))


# ---- save output                                  


save(train_male, male_ses_only, male_log, train_female, female_ses_only, female_log, file = "data/logistic_output_eduOnly.Rdata")
