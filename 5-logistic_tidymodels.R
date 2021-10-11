# ---- load packages and data             ----
# setwd("Documents/research PIElab/projects/SAPA_BMI/")

#load packages
packages = c("tidyverse", "broom", "tidymodels", "themis")
lapply(packages, library, character.only = TRUE)
rm(packages)

library(doParallel)

load("data/cleaned.Rdata")

# set seed
set.seed(090919)


# recipe ------------------------------------------------------------------

lr_recipe = function(dataframe, form, int = FALSE) {
  out = recipe(form, data = dataframe) %>% 
    step_novel(all_nominal(), -all_outcomes()) %>%  
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_unknown(all_nominal(), -all_outcomes()) 
  
  if(int){
    out = out %>% step_interact(terms =~ ses:trait_score)
  }
  
  out = out %>%
    step_zv(all_numeric()) %>%
    step_normalize(all_predictors(), -all_nominal()) %>% 
    step_naomit(all_outcomes(), all_predictors(), skip = T) %>%
    step_smote(all_outcomes())
  
  return(out)
}


lr_mod <- 
  multinom_reg(penalty = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

lambda_grid <- grid_regular(penalty(), levels = 50)

build_lr_workflow = function(rec){
  out =  
    workflow() %>% 
    add_recipe(rec) %>%
    add_model(lr_mod) 
  
  return(out)
}

fit_lr_train = function(rec, validation_data, tg){
  out = tune_grid(
    lr_mod,
    rec,
    resamples = validation_data,
    grid = tg,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, accuracy, sens, spec))
}

# ---- wrangle data for iteration         ----

# end goal of wrangling is a data frame of data frames
# nested dataframes correspond to a single personality trait
# score refers to a participant's score on that trait
# we also standardize each of our variables within gender


sapa_male_trait = sapa_male %>%
  dplyr::select(-starts_with("p1"), -starts_with("p2"), -starts_with("edu")) %>%
  filter(!is.na(BMI_c)) %>%
  mutate(BMI_c = factor(BMI_c, levels = c("Normal Weight", "Underweight", "Overweight", "Obese"))) %>%
  gather("trait_name", "trait_score", -ses, -BMI_c, -BMI, -BMI_p) %>%
  group_by(trait_name) %>%
  nest() %>%
  ungroup() 

sapa_female_trait = sapa_female %>%
  dplyr::select(-starts_with("p1"), -starts_with("p2"), -starts_with("edu")) %>%
  filter(!is.na(BMI_c)) %>%
  mutate(BMI_c = factor(BMI_c, levels = c("Normal Weight", "Underweight", "Overweight", "Obese"))) %>%
  gather("trait_name", "trait_score", -ses, -BMI_c, -BMI, -BMI_p) %>%
  group_by(trait_name) %>%
  nest() %>%
  ungroup() 



# ---- ordered logistic regression iteration (males)         ----
no_cores <- detectCores() - 2
cl <- makeCluster(no_cores)
registerDoParallel(cl)

set.seed(04072021)

sapa_male_trait = sapa_male_trait %>%
  mutate(splits = map(data, ~initial_split(.x, prop = 3/4, strata = "BMI_c"))) %>%
  mutate(data_train = map(splits, training)) %>%
  mutate(data_cv = map(data_train, vfold_cv, v = 10, strata = BMI_c)) %>%
  mutate(
    recipe_add = map(data, lr_recipe, form = BMI_c ~ ses + trait_score),
    recipe_jnt = map(data, lr_recipe, form = BMI_c ~ ses + trait_score, int = T)) %>%
  gather(model, recipe, contains("recipe")) %>%
  mutate(model = str_remove(model, "recipe_")) %>%
  mutate(workflow = map(recipe, build_lr_workflow)) %>%
  mutate(tune_results = map2(workflow, data_cv,
                             ~.x %>% tune_grid(
                               resamples = .y,
                               grid = lambda_grid,
                               metrics = metric_set(accuracy, roc_auc)
                             )))

save(sapa_male_trait, file = "data/sapa_male_trait.Rdata")
rm(sapa_male_trait, sapa_male)
# 
set.seed(04072021)
load("data/sapa_male_trait.Rdata")

sapa_male_trait %>%
  mutate(metrics = map(tune_results, collect_metrics)) %>%
  mutate(metrics = map(metrics, ~dplyr::filter(.x, .metric == "roc_auc"))) %>%
  mutate(metrics = map(metrics, ~filter(.x, mean == max(mean)))) %>%
  mutate(metrics = map(metrics, ~filter(.x, row_number() == 1))) %>%
  mutate(roc_est = map_dbl(metrics, "mean"))


sapa_male_final = sapa_male_trait %>%
  mutate(best = map(tune_results, ~select_best(.x, metric = "roc_auc"))) %>%
  mutate(decay_val = map(best, 1)) %>%
  mutate(workflow = map2(workflow, best, ~.x %>% finalize_workflow(.y))) %>%
  mutate(fit_test = map2(workflow, splits, last_fit)) %>%
  mutate(fit_final = map2(workflow, data, fit)) %>%
  mutate(data_bake = map(recipe, ~.x %>% prep() %>% juice())) %>%
  mutate(fit_mulitnom = ifelse(
    model == "add",
    map2(data_bake, decay_val, ~multinom(BMI_c ~ ses + trait_score, data = .x, decay = .y)),
    map2(data_bake, decay_val, ~multinom(BMI_c ~ ses*trait_score, data = .x, decay = .y))
  ))

 
# # ---- ordered logistic regression iteration (females)           ----


set.seed(04072021)

sapa_female_trait = sapa_female_trait %>%
  mutate(splits = map(data, ~initial_split(.x, prop = 3/4, strata = "BMI_c"))) %>%
  mutate(data_train = map(splits, training)) %>%
  mutate(data_cv = map(data_train, vfold_cv, v = 10, strata = BMI_c)) %>%
  mutate(
    recipe_add = map(data, lr_recipe, form = BMI_c ~ ses + trait_score),
    recipe_jnt = map(data, lr_recipe, form = BMI_c ~ ses + trait_score, int = T)) %>%
  gather(model, recipe, contains("recipe")) %>%
  mutate(model = str_remove(model, "recipe_")) %>%
  mutate(workflow = map(recipe, build_lr_workflow)) %>%
  mutate(tune_results = map2(workflow, data_cv,
                             ~.x %>% tune_grid(
                               resamples = .y,
                               grid = lambda_grid,
                               metrics = metric_set(accuracy, roc_auc)
                             )))
save(sapa_female_trait, file = "data/sapa_female_trait.Rdata")

set.seed(04072021)
load("data/sapa_female_trait.Rdata")

sapa_female_trait %>%
  mutate(metrics = map(tune_results, collect_metrics)) %>%
  mutate(metrics = map(metrics, ~dplyr::filter(.x, .metric == "roc_auc"))) %>%
  mutate(metrics = map(metrics, ~filter(.x, mean == max(mean)))) %>%
  mutate(metrics = map(metrics, ~filter(.x, row_number() == 1))) %>%
  mutate(roc_est = map_dbl(metrics, "mean"))


sapa_female_final = sapa_female_trait %>%
  mutate(best = map(tune_results, ~select_best(.x, metric = "roc_auc"))) %>%
  mutate(workflow = map2(workflow, best, ~.x %>% finalize_workflow(.y))) %>%
  mutate(finalfit = map2(workflow, splits, last_fit))
# 
# # # ---- save output                                  
# 
# 
# save(sapa_male_final, sapa_female_final, file = "data/logistic_tidymodels_output.Rdata")
