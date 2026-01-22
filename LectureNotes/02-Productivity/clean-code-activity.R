# Calculate summary stats
setwd("C:/Users/JohnDoe/Documents/MyProject/data")
d <- read.csv("data.csv")
d$x2 <- d$x^2
d$x3 <- d$x^3
d$x4 <- d$x^4
m <- mean(d$y)
s <- sd(d$y)
m2 <- mean(d$x)
s2 <- sd(d$x)
model <- lm(d$y ~ d$x + d$x2 + d$x3 + d$x4)
summary(model)