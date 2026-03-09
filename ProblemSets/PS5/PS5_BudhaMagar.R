#CNBC without API key

library(rvest)
library(dplyr)

url <- "https://www.cnbc.com/"

page <- read_html(url)

titles <- page %>%
  html_nodes(".Card-title") %>%
  html_text(trim = TRUE)

data <- data.frame(title = titles)
data<- distinct(data.frame())

head(data)

write.csv(data, "cnbc_headlines.csv", row.names = FALSE)

#FRED with API KEY

library("fredr")
library("dplyr")
library("ggplot2")
library("usethis")

fredr_set_key(Sys.getenv("FRED_API_KEY"))
ppi<- fredr(
  series_id = "PPIACO",
  observation_start = as.Date("2000-01-01")
)
head(ppi)



