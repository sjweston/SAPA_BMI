# ------------------------------------
# load packages and data             #
# ------------------------------------

#load packages
packages = c("tidyverse", "broom", "nnet", "rsample", "caret", "ROSE")
lapply(packages, library, character.only = TRUE)
rm(packages)

load("data/cleaned.Rdata")


# ----------------------------------------------
# set up train/test              #
# ----------------------------------------------

# set seed
set.seed(073117)

ctrl <- trainControl(method = "repeatedcv", # cross-validation
                     number = 10,  # 10 fold cross validation
                     repeats = 10, #repeated 10 times
                     verboseIter = FALSE,
                     sampling = "up") #resample from minority categories to create balanced classes

# parition into training and test sets. objects identify just training rows
train_male = createDataPartition(sapa_male$BMI_c, p = .75, list = FALSE)
train_female = createDataPartition(sapa_female$BMI_c, p = .75, list = FALSE)

# ------------------------------------
# wrangle data for iteration         #
# ------------------------------------

#end goal of wrangling is a data frame of data frames
# nested dataframes correspond to a single personality trait
# score refers to a participant's score on that trait
# we also standardize each of our variables within gender


sapa_male_trait = sapa_male %>%
  dplyr::select(-starts_with("p1"), -starts_with("p2")) %>%
  mutate(edu = scale(ses)) %>%
  mutate(BMI_c = factor(BMI_c, levels = c("Normal Weight", "Underweight", "Overweight", "Obese"))) %>%
  gather("trait_name", "trait_score", -ses, -BMI_c, -BMI, -BMI_p) %>%
  group_by(trait_name) %>%
  mutate(trait_score = scale(trait_score)) %>%
  nest()

sapa_female_trait = sapa_female %>%
  dplyr::select(-starts_with("p1"), -starts_with("p2")) %>%
  mutate(edu = scale(ses)) %>%
  mutate(BMI_c = factor(BMI_c, levels = c("Normal Weight", "Underweight", "Overweight", "Obese"))) %>%
  gather("trait_name", "trait_score", -ses, -BMI_c, -BMI, -BMI_p) %>%
  group_by(trait_name) %>%
  mutate(trait_score = scale(trait_score)) %>%
  nest()

# -----------------------------------------------------------
# ordered logistic regression iteration (males)         #
# -----------------------------------------------------------

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

# -----------------------------------------------------------
# ordered logistic regression iteration (females)           #
# -----------------------------------------------------------

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

# ----------------------------------------------
# save output                                  #
# ----------------------------------------------

save(male_log, female_log, file = "data/logistic_output.Rdata")
