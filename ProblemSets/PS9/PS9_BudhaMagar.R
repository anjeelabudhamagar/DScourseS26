library(tidyverse)
library(tidymodels)
library(magrittr)
library(glmnet)
library(dplyr)

set.seed(123456)

#Load data
housing <- read_table(
  "http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data",
  col_names = FALSE
)

names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis",
                    "rad","tax","ptratio","b","lstat","medv")

# Split data
housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)

#7.Recipe
housing_recipe <- recipe(medv ~ ., data = housing) %>%
  step_log(all_outcomes()) %>%
  step_bin2factor(chas) %>%
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:
                  ptratio:b:lstat:dis:nox) %>%
  step_poly(crim, zn, indus, rm, age, rad, tax, ptratio,
            b, lstat, dis, nox, degree = 6)

# Prep data
housing_prep <- housing_recipe %>%
  prep(training = housing_train, retain = TRUE)

housing_train_prepped <- juice(housing_prep)
housing_test_prepped  <- bake(housing_prep, new_data = housing_test)

#X matrices
housing_train_x <- housing_train_prepped %>% select(-medv)

#Cross-validation setup (6-fold)
folds <- vfold_cv(housing_train_prepped, v = 6)

lambda_grid <- grid_regular(penalty(), levels = 50)

#Dimension f training data
dim(housing_train_prepped)

#Number of more X variables than in the orgiginal housing data
ncol(housing_train_x)

#8. Estimate a LASSO model to predict log median house value. 

lasso_spec <- linear_reg(
  penalty = tune(),
  mixture = 1
) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

lasso_wf <- workflow() %>%
  add_formula(medv ~ .) %>%
  add_model(lasso_spec)

lasso_res <- lasso_wf %>%
  tune_grid(
    resamples = folds,
    grid = lambda_grid,
    metrics = metric_set(rmse)
  )

best_lasso <- select_best(lasso_res, metric = "rmse")

final_lasso <- finalize_workflow(lasso_wf, best_lasso)

lasso_fit <- fit(final_lasso, data = housing_train_prepped)

# Train RMSE
lasso_train_pred <- predict(lasso_fit, housing_train_prepped) %>%
  bind_cols(housing_train_prepped)

lasso_train_rmse <- rmse(lasso_train_pred, truth = medv, estimate = .pred)

# Test RMSE
lasso_test_pred <- predict(lasso_fit, housing_test_prepped) %>%
  bind_cols(housing_test_prepped)

lasso_test_rmse <- rmse(lasso_test_pred, truth = medv, estimate = .pred)


#9. Estimate a ridge regression model 

ridge_spec <- linear_reg(
  penalty = tune(),
  mixture = 0
) %>%
  set_engine("glmnet")

ridge_wf <- workflow() %>%
  add_formula(medv ~ .) %>%
  add_model(ridge_spec)

ridge_res <- tune_grid(
  ridge_wf,
  resamples = folds,
  grid = lambda_grid,
  metrics = metric_set(rmse)
)

best_ridge <- select_best(ridge_res, metric = "rmse")

final_ridge <- finalize_workflow(ridge_wf, best_ridge)

ridge_fit <- fit(final_ridge, data = housing_train_prepped)

# Train RMSE
ridge_train_pred <- predict(ridge_fit, housing_train_prepped) %>%
  bind_cols(housing_train_prepped)

ridge_train_rmse <- rmse(ridge_train_pred, truth = medv, estimate = .pred)

# Test RMSE
ridge_test_pred <- predict(ridge_fit, housing_test_prepped) %>%
  bind_cols(housing_test_prepped)

ridge_test_rmse <- rmse(ridge_test_pred, truth = medv, estimate = .pred)

# Print Outputs
# For RMSE for #8 and #9
lasso_train_rmse
lasso_test_rmse
ridge_train_rmse
ridge_test_rmse

#Optimal lambda
best_lasso$penalty
best_ridge$penalty