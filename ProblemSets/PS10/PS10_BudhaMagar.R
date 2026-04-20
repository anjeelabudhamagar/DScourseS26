library(tidyverse)
library(tidymodels)
library(magrittr)
library(modelsummary)
library(rpart)
library(e1071)
library(kknn)
library(nnet)
library(kernlab)

set.seed(100)

######################
# Load data
######################
income <- read_csv(
  "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",
  col_names = FALSE
)

names(income) <- c("age","workclass","fnlwgt","education","education.num",
                   "marital.status","occupation","relationship","race","sex",
                   "capital.gain","capital.loss","hours","native.country","high.earner")

######################
# Clean data
######################

# Drop unnecessary variables
income %<>% select(-native.country, -fnlwgt)

# Convert numeric variables
income %<>% mutate(across(
  c(age, hours, capital.gain, capital.loss),
  as.numeric
))

# Convert categorical variables
income %<>% mutate(across(
  c(high.earner, education, marital.status, race,
    workclass, occupation, relationship, sex),
  as.factor
))

# Trim whitespace
income <- income %>%
  mutate(across(where(is.character), trimws))

# Collapse factor levels
income %<>% mutate(
  education = fct_collapse(education,
                           Advanced    = c("Masters","Doctorate","Prof-school"),
                           Bachelors   = c("Bachelors"),
                           SomeCollege = c("Some-college","Assoc-acdm","Assoc-voc"),
                           HSgrad      = c("HS-grad","12th"),
                           HSdrop      = c("11th","9th","7th-8th","1st-4th","10th","5th-6th","Preschool")
  ),
  marital.status = fct_collapse(marital.status,
                                Married      = c("Married-civ-spouse","Married-spouse-absent","Married-AF-spouse"),
                                Divorced     = c("Divorced","Separated"),
                                Widowed      = c("Widowed"),
                                NeverMarried = c("Never-married")
  ),
  race = fct_collapse(race,
                      White = c("White"),
                      Black = c("Black"),
                      Asian = c("Asian-Pac-Islander"),
                      Other = c("Other","Amer-Indian-Eskimo")
  ),
  workclass = fct_collapse(workclass,
                           Private = c("Private"),
                           SelfEmp = c("Self-emp-not-inc","Self-emp-inc"),
                           Gov     = c("Federal-gov","Local-gov","State-gov"),
                           Other   = c("Without-pay","Never-worked","?")
  ),
  occupation = fct_collapse(occupation,
                            BlueCollar  = c("?","Craft-repair","Farming-fishing","Handlers-cleaners",
                                            "Machine-op-inspct","Transport-moving"),
                            WhiteCollar = c("Adm-clerical","Exec-managerial","Prof-specialty",
                                            "Sales","Tech-support"),
                            Services    = c("Armed-Forces","Other-service","Priv-house-serv","Protective-serv")
  )
)

######################
# Train/test split
######################
income_split <- initial_split(income, prop = 0.8)
income_train <- training(income_split)
income_test  <- testing(income_split)

rec_folds <- vfold_cv(income_train, v = 3)

#####################
# LOGISTIC REGRESSION
#####################
tune_logit_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

lambda_grid <- grid_regular(penalty(), levels = 50)

logit_wf <- workflow() %>%
  add_model(tune_logit_spec) %>%
  add_formula(high.earner ~ .)

logit_res <- tune_grid(logit_wf, rec_folds, grid = lambda_grid)

logit_top  <- show_best(logit_res, "accuracy")
logit_best <- select_best(logit_res, "accuracy")

final_logit <- finalize_workflow(logit_wf, logit_best)

logit_test <- last_fit(final_logit, income_split) %>%
  collect_metrics()

logit_ans <- logit_top %>%
  slice(1) %>%
  left_join(logit_test %>% slice(1), by=c(".metric",".estimator")) %>%
  mutate(alg="logit") %>%
  select(-starts_with(".config"))

#####################
# TREE
#####################
tune_tree_spec <- decision_tree(
  min_n = tune(),
  tree_depth = tune(),
  cost_complexity = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

tree_grid <- tidyr::crossing(
  cost_complexity = seq(.001,.2,by=.05),
  min_n = seq(10,100,by=10),
  tree_depth = seq(5,20,by=5)
)

tree_wf <- workflow() %>%
  add_model(tune_tree_spec) %>%
  add_formula(high.earner ~ .)

tree_res <- tune_grid(tree_wf, rec_folds, grid = tree_grid)

tree_top  <- show_best(tree_res, "accuracy")
tree_best <- select_best(tree_res, "accuracy")

final_tree <- finalize_workflow(tree_wf, tree_best)

tree_test <- last_fit(final_tree, income_split) %>%
  collect_metrics()

tree_ans <- tree_top %>%
  slice(1) %>%
  left_join(tree_test %>% slice(1), by=c(".metric",".estimator")) %>%
  mutate(alg="tree") %>%
  select(-starts_with(".config"))

#####################
# NEURAL NET
#####################
tune_nnet_spec <- mlp(hidden_units = tune(), penalty = tune()) %>%
  set_engine("nnet") %>%
  set_mode("classification")

nnet_grid <- tidyr::crossing(
  hidden_units = 1:10,
  penalty = seq(0.001, 0.1, length.out = 10)
)

nnet_wf <- workflow() %>%
  add_model(tune_nnet_spec) %>%
  add_formula(high.earner ~ .)

nnet_res <- tune_grid(nnet_wf, rec_folds, grid = nnet_grid)

nnet_top  <- show_best(nnet_res, "accuracy")
nnet_best <- select_best(nnet_res, "accuracy")

final_nnet <- finalize_workflow(nnet_wf, nnet_best)

nnet_test <- last_fit(final_nnet, income_split) %>%
  collect_metrics()

nnet_ans <- nnet_top %>%
  slice(1) %>%
  left_join(nnet_test %>% slice(1), by=c(".metric",".estimator")) %>%
  mutate(alg="nnet") %>%
  select(-starts_with(".config"))

#####################
# KNN
#####################
tune_knn_spec <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")

knn_grid <- tibble(neighbors = 1:30)

knn_wf <- workflow() %>%
  add_model(tune_knn_spec) %>%
  add_formula(high.earner ~ .)

knn_res <- tune_grid(knn_wf, rec_folds, grid = knn_grid)

knn_top  <- show_best(knn_res, "accuracy")
knn_best <- select_best(knn_res, "accuracy")

final_knn <- finalize_workflow(knn_wf, knn_best)

knn_test <- last_fit(final_knn, income_split) %>%
  collect_metrics()

knn_ans <- knn_top %>%
  slice(1) %>%
  left_join(knn_test %>% slice(1), by=c(".metric",".estimator")) %>%
  mutate(alg="knn") %>%
  select(-starts_with(".config"))

#####################
# SVM
#####################
svm_rec <- recipe(high.earner ~ ., data = income_train) %>%
  step_zv(all_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

tune_svm_spec <- svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

svm_grid <- tidyr::crossing(
  cost = 2^(-2:2),
  rbf_sigma = 2^(-2:2)
)

svm_wf <- workflow() %>%
  add_recipe(svm_rec) %>%
  add_model(tune_svm_spec)

svm_res <- tune_grid(svm_wf, rec_folds, grid = svm_grid)

svm_best <- select_best(svm_res, "accuracy")

final_svm <- finalize_workflow(svm_wf, svm_best)

svm_test <- last_fit(final_svm, income_split) %>%
  collect_metrics()

svm_ans <- svm_test %>%
  slice(1) %>%
  mutate(alg="svm")

#####################
# FINAL TABLE
#####################
all_ans <- bind_rows(logit_ans, tree_ans, nnet_ans, knn_ans, svm_ans)

datasummary_df(
  all_ans %>% select(-.metric, -.estimator, -mean, -n, -std_err),
  output = "markdown"
)