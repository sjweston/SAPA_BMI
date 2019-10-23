
#load packages
packages = c("tidyverse", "broom", "nnet", "rsample", "caret", "ROSE", "here")
lapply(packages, library, character.only = TRUE)
rm(packages)

load(here("data/cleaned.Rdata"))

# sapa_male = sapa_male %>%
#   mutate(BMI_c = gsub(" ", "", BMI_c),
#     BMI_c = factor(BMI_c, levels = c("NormalWeight", "Underweight", "Overweight", "Obese")))


# set seed
set.seed(090919)

ctrl <- trainControl(method = "repeatedcv", # cross-validation
                     number = 10,  # 10 fold cross validation
                     repeats = 10, #repeated 10 times
                     verboseIter = FALSE,
                     search = "random",
                     sampling = "smote") #resample from minority categories to create balanced classes

# parition into training and test sets. objects identify just training rows
train_male = createDataPartition(sapa_male$BMI_c, p = .75, list = FALSE)
train_female = createDataPartition(sapa_female$BMI_c, p = .75, list = FALSE)
  
male_ses = train(BMI_c ~ ses , data = sapa_male, 
                 subset = train_male, 
                 method = "multinom",
                 maxit= 1000,
                 na.action = "na.exclude", 
                 trControl = ctrl)

male_cog = train(BMI_c ~ cog , data = sapa_male, 
                     subset = train_male, 
                     method = "multinom",
                     maxit= 1000,
                     na.action = "na.exclude", 
                     trControl = ctrl)

male_cog_ses = train(BMI_c ~ cog +ses, data = sapa_male, 
                 subset = train_male, 
                 method = "multinom",
                 maxit= 1000,
                 na.action = "na.exclude", 
                 trControl = ctrl)

accuracy = predict(male_ses, type="raw", newdata=sapa_male[-train_male, ])
postResample(sapa_male[-train_male, "BMI_c"], accuracy)

accuracy3 = predict(male_cog, type="raw", newdata=sapa_male[-train_male, ])
postResample(sapa_male[-train_male, "BMI_c"], accuracy3)

accuracy2 = predict(male_cog_ses, type="raw", newdata=sapa_male[-train_male, ])
postResample(sapa_male[-train_male, "BMI_c"], accuracy2)



accuracy = predict(male_ses, type="raw", newdata=sapa_male[train_male, ])
postResample(sapa_male[train_male, "BMI_c"], accuracy)

accuracy2 = predict(male_cog_ses, type="raw", newdata=sapa_male[train_male, ])
postResample(sapa_male[train_male, "BMI_c"], accuracy2)
