# ---- load packages ---------------------

packages = c("glmnet", "caret", "ggpubr", "knitr", "kableExtra")
lapply(packages, library, character.only = TRUE)
rm(packages)

load("data/cleaned.Rdata")


# ---- split data --------------------------------------------------------------

female_train = sapa_female[train_female, ] %>% select(BMI_p, ses, cog, contains("SPI")) %>% filter(complete.cases(.))
female_test = sapa_female[-train_female, ] %>% select(BMI_p, ses, cog, contains("SPI")) %>% filter(complete.cases(.))
female_bmi = female_train$BMI_p

male_train = sapa_male[train_male, ] %>% select(BMI_p, ses, cog, contains("SPI")) %>% filter(complete.cases(.))
male_test = sapa_male[-train_male, ] %>% select(BMI_p, ses, cog, contains("SPI")) %>% filter(complete.cases(.))
male_bmi = male_train$BMI_p


# ---- function for fitting ----------------------------------------------------

fit_model = function(data, outcome){
  cv_value = model.matrix(BMI_p ~ ., 
                 data = data) %>%
    cv.glmnet(x = .,
              y = outcome, 
              alpha = 1)
  model =  model.matrix(BMI_p ~ ., data = data) %>%
    glmnet(y = outcome, 
           alpha = 1, 
           lambda = cv_value$lambda.min)
  return(model)
}


# ---- model for predictions ---------------------------------------------------

pred_model = function(model, test.data){
  
  if(length(model$coefficients) == 2){
    x = test.data
    }else{ 
      x = test.data[, c("BMI_p", rownames(model$beta)[-1])]
      x <- model.matrix(BMI_p~., x)
      }
  predictions = model %>% predict(x) %>% as.vector()
  # Model performance metrics
  fit = data.frame(
    RMSE = RMSE(predictions, test.data$BMI_p),
    Rsquare = R2(predictions, test.data$BMI_p)
  )
  return(fit)
}



# ---- female fits -------------------------------------------------------------

set.seed(060821)

mod1_f = lm(BMI_p ~ ses, data = female_train)
mod2_f = female_train %>%
  select(BMI_p, ses, cog) %>%
  fit_model(data = ., outcome = female_bmi)
mod3_f = female_train %>%
  select(BMI_p, ses, contains("SPI")) %>%
  select(1:7) %>%
  fit_model(data = ., outcome = female_bmi)
mod4_f = female_train %>%
  select(BMI_p, ses, contains("SPI")) %>%
  select(1:2,8:34) %>%
  fit_model(data = ., outcome = female_bmi)
mod5_f = female_train %>%
  select(BMI_p, ses, cog, contains("SPI")) %>%
  select(1:8) %>%
  fit_model(data = ., outcome = female_bmi)
mod6_f = female_train %>%
  select(BMI_p, ses, cog, contains("SPI")) %>%
  select(1:3,9:35) %>%
  fit_model(data = ., outcome = female_bmi)

female_fits = data.frame(
  vars = c(
    "SES only",
    "SES + Cog",
    "SES + Big Five",
    "SES + Narrow 27",
    "SES + Cog + Big Five",
    "SES + Cog + Narrow 27"))
female_fits$model = list(mod1_f, mod2_f, mod3_f, mod4_f, mod5_f, mod6_f)
female_fits = mutate(female_fits, fits = map(model, pred_model, test.data = female_test))

female_fits = female_fits %>%
  select(-model) %>%
  unnest(cols = c(fits)) %>%
  mutate(gender = "Adolescent Girls")

# ---- male fits -------------------------------------------------------------

set.seed(060821)

mod1_m = lm(BMI_p ~ ses, data = male_train)
mod2_m = male_train %>%
  select(BMI_p, ses, cog) %>%
  fit_model(data = ., outcome = male_bmi)
mod3_m = male_train %>%
  select(BMI_p, ses, contains("SPI")) %>%
  select(1:7) %>%
  fit_model(data = ., outcome = male_bmi)
mod4_m = male_train %>%
  select(BMI_p, ses, contains("SPI")) %>%
  select(1:2,8:34) %>%
  fit_model(data = ., outcome = male_bmi)
mod5_m = male_train %>%
  select(BMI_p, ses, cog, contains("SPI")) %>%
  select(1:8) %>%
  fit_model(data = ., outcome = male_bmi)
mod6_m = male_train %>%
  select(BMI_p, ses, cog, contains("SPI")) %>%
  select(1:3,9:35) %>%
  fit_model(data = ., outcome = male_bmi)

male_fits = data.frame(
  vars = c(
    "SES only",
    "SES + Cog",
    "SES + Big Five",
    "SES + Narrow 27",
    "SES + Cog + Big Five",
    "SES + Cog + Narrow 27"))
male_fits$model = list(mod1_m, mod2_m, mod3_m, mod4_m, mod5_m, mod6_m)
male_fits = mutate(male_fits, fits = map(model, pred_model, test.data = male_test))

male_fits = male_fits %>%
  select(-model) %>%
  unnest(cols = c(fits)) %>%
  mutate(gender = "Adolescent Boys")


# ---- figure ------------------------------------------------------------------

figure_data  = female_fits %>%
  full_join(male_fits) %>%
  mutate(vars = factor(vars, levels = c("SES only",
                                        "SES + Cog",
                                        "SES + Big Five",
                                        "SES + Narrow 27",
                                        "SES + Cog + Big Five",
                                        "SES + Cog + Narrow 27"))) 
ses_only = figure_data %>%
  filter(vars == "SES only") 
  
figure_data %>%
  ggplot(aes(x = reorder(vars, desc(vars)), 
             y = Rsquare, fill = Rsquare)) +
  geom_bar(stat = "identity") +
  geom_hline(aes(yintercept = Rsquare), 
             data = ses_only, 
             linetype = "dashed") +
  facet_grid(gender ~ ., scales = "free") +
  coord_flip() +
  labs(x = NULL, y = NULL, title = "R-Squared on Test Set") +
  guides(fill = F) +
  theme_pubr() +
  theme(plot.title.position = "plot")
ggsave(here("figures/compare_rsq.jpeg"))

# ---- table -------------------------------------------------------------------


female_fits %>%
  full_join(male_fits) %>%
  mutate(gender = str_remove(gender, "Adolescent ")) %>%
  gather(stat, value, starts_with("R")) %>%
  unite(stat, gender, stat) %>%
  spread(stat, value) %>%
  mutate(vars = factor(vars, levels = c("SES only",
                                        "SES + Cog",
                                        "SES + Big Five",
                                        "SES + Narrow 27",
                                        "SES + Cog + Big Five",
                                        "SES + Cog + Narrow 27"))) %>%
  arrange(vars) %>%
  kable(col.names = c("Model", rep(c("RMSE", "R-squared"), 2)),
        booktabs = T,
        digits = c(0,2,3,2,3)) %>%
  kable_styling() %>%
  add_header_above(c(" ", "Adolescent Boys" = 2, "Adolescent Girls" = 2))

