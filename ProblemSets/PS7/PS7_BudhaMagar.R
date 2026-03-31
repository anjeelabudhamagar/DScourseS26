library(mice)
library(modelsummary)
library(tidyverse)

#Load data and inspect
wages <- read.csv("ProblemSets/PS7/wages.csv")
head(wages)
tail(wages)

#Dropping observations where hgc and tenure are missing 
wages<- wages %>%
  filter(!is.na(hgc), !is.na(tenure))

# Convert categorical variables to factors
wages <- wages %>%
  mutate(
    college = as.factor(college),
    married = as.factor(married)
  )

#Summary Table
datasummary_skim(wages, output = "latex")

#Missing rate of logwage
missing_rate<-mean(is.na((wages$logwage)))
missing_rate 

#Regression Model
model <- lm(logwage ~ hgc + tenure + age + college + married,
            data = wages,
            na.action = na.omit)
#Regression table
modelsummary(model, output = "latex")

#Method 1: Regression using only complete cases
run_model<- function(data) {
  lm(logwage~hgc+college+tenure+I(tenure^2)+age+married, data=data)
}

data_cc<- wages%>% drop_na(logwage)
model_cc<- run_model(data_cc)
summary(model_cc)

#Method 2: Mean Imputation
data_mean<- wages
data_mean$logwage[is.na(data_mean$logwage)]<-
  mean(data_mean$logwage, na.rm = TRUE)
model_mean<- run_model(data_mean)
summary(model_mean)

#Method 3: Regression Imputation Using Complete Cases
model_pred<- model_cc 
data_reg<- wages
missing_index<- is.na(data_reg$logwage)

data_reg$logwage[missing_index]<-
  predict(model_pred, newdata = data_reg[missing_index,])
model_reg<- run_model(data_reg)
summary(model_reg)

#Method 4: Multiple Imputation (MICE)
imp<- mice(wages, m=5, method = "pmm", seed = 123)
model_mice <- with(imp,
 lm(logwage~hgc+college+tenure+I(tenure^2)+age+married)
)
multipleimp<- pool(model_mice)
summary(multipleimp)

modelsummary(
  list(
    "Complete Case"= model_cc,
    "Mean Imputation"= model_mean,
    "Regression Imputation using Comeplete Case Data"=model_reg,
    "Multiple Imputation"= multipleimp
  ),
  output="latex"
)

coef(model_cc)["hgc"]
