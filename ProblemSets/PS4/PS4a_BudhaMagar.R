library(jsonlite)
library(tidyverse)

# Set working directory)
setwd("~/DScourseS26/ProblemSets/PS4")

url <- "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"

# Download JSON 
download.file(url, "dates.json", method="curl")

# Read JSON file
mylist <- fromJSON("dates.json")

# Convert to dataframe
mydf <- bind_rows(mylist[["result"]][-1])

# Output
print("Class of dataframe")
print(class(mydf))

print("Class of date column")
print(class(mydf$date))

print("First 6 rows")
print(head(mydf))
