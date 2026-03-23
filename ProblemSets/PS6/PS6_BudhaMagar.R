#Load Libraries
library(tidyverse)
library(janitor)
library(zoo)
library(ggplot2)

#Load and Clean Data
femalelabor<- read.csv("Femalelabor.csv", skip=4, stringsAsFactors=FALSE)
femalelabor<-femalelabor %>% clean_names()

#Filter for Afghanistan
afgfemale<- femalelabor %>% filter(country_name=="Afghanistan")
afgfemale<- afgfemale %>% select(-x)

#Identify Year Columns
yearcol<- colnames(afgfemale)[5:ncol(afgfemale)]

#Convert Year Columns to Numeric and Interpolate Missing Values
afgfemale[yearcol]<- lapply(afgfemale[yearcol], function(x) as.numeric(x))
afgfemale[yearcol]<- lapply(afgfemale[yearcol], function(x) na.approx(x, na.rm=FALSE))

#Reshape Data to Long Format
afgfemale1<- afgfemale %>%
pivot_longer(cols=all_of(yearcol),
             names_to = "year",
             values_to = "female_labor_participation")%>%
  mutate(year=as.integer(str_remove(year, "x"))) %>%
  arrange(year)
glimpse(afgfemale1)

#Filter Out Remaining NAs
afgfemale2<- afgfemale1 %>%
  filter(!is.na(female_labor_participation))

glimpse(afgfemale2)

#Overall Trend Over Time (TimeSeries/Line Plot)
png("female_labor_time_series.png", width = 800, height = 600)
ggplot(afgfemale2, aes(x = year, y = female_labor_participation)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  labs(title = "Female Labor Force Participation in Afghanistan (1990–2025)",
       x = "Year",
       y = "Participation Rate (%)") +
  theme_minimal(base_size = 14)
dev.off()
#Year over Year Change (Bar Pot)
png("female_labor_change.png", width = 800, height = 600)
afgfemale2 <- afgfemale2 %>%
  arrange(year) %>%
  mutate(change = female_labor_participation - lag(female_labor_participation))

ggplot(afgfemale2, aes(x = year, y = change)) +
  geom_col(fill = "purple") +
  labs(title = "Year-over-Year Change in Female Labor Force Participation",
       x = "Year",
       y = "Change in Participation Rate (%)") +
  theme_minimal(base_size = 14)
dev.off()

# Highlight key periods
png("female_labor_highlight_periods.png", width = 800, height = 600)

ggplot(afgfemale2, aes(x = year, y = female_labor_participation)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  # Highlight Taliban first period (1996–2001, approximate)
  annotate("rect", xmin = 1996, xmax = 2001, ymin = 0, ymax = max(afgfemale2$female_labor_participation),
           alpha = 0.2, fill = "red") +
  # Highlight Taliban return (2021–2022)
  annotate("rect", xmin = 2021, xmax = 2022, ymin = 0, ymax = max(afgfemale2$female_labor_participation),
           alpha = 0.2, fill = "orange") +
  labs(title = "Female Labor Force Participation with Key Taliban Periods Highlighted",
       subtitle = "Red = Taliban first period (1996–2001), Orange = Taliban return (2021–2022)",
       x = "Year",
       y = "Participation Rate (%)") +
  theme_minimal(base_size = 14)

dev.off()