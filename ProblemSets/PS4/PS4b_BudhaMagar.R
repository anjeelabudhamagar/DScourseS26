# PS4b_BudhaMagar.R
# Econ 5253 Spring 2026
# Sparklyr Exercise

# Load libraries
library(sparklyr)
library(tidyverse)

#############################################
# Connect to Spark
#############################################

sc <- spark_connect(master = "local")

#############################################
# Load iris dataset
#############################################

# Convert iris dataset to tibble
df1 <- as_tibble(iris)

#############################################
# Copy tibble into Spark
#############################################

df <- copy_to(sc, df1, overwrite = TRUE)

#############################################
# Question 7: Class comparison
#############################################

print("Class of df1")
print(class(df1))

print("Class of df")
print(class(df))

#############################################
# Question 8: Column name comparison
#############################################

print("Column names of df1")
print(colnames(df1))

print("Column names of df")
print(coames(df))

#############################################
# Question 9: Select first 6 rows
#############################################

df %>%
  select(Sepal.Length, Species) %>%
  head(6) %>%
  print()

#############################################
# Question 10: Filter Sepal.Length > 5.5
#############################################

df %>%
  filter(Sepal.Length > 5.5) %>%
  head(6) %>%
  print()

#############################################
# Question 11: Combined pipeline
#############################################

df %>%
  filter(Sepal.Length > 5.5) %>%
  select(Sepal.Length, Species) %>%
  head(6) %>%
  print()

#############################################
# Question 12: Group summary
#############################################

df2 <- df %>%
  group_by(Species) %>%
  summarize(
    mean = mean(Sepal.Length),
    count = n()
  )

print(df2)

#############################################
# Question 13: Sort result
#############################################

df2 %>%
  arrange(Species) %>%
  print()

#############################################
# Disconnect Spark
#############################################

spark_disconnect(sc)
