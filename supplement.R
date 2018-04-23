library(tidyverse)
library(dplyr)
library(BMA)
us_income_data <- read.csv("us_income_dataset.csv")
colnames(us_income_data)[colnames(us_income_data)=="WEEKS_WORKED_NUMERIC"] <- "NUM_HOURS_WEEK"
us_income_data <- us_income_data %>%
  dplyr::select(INCOME, AGE, NUM_CHILD, TRAVEL_TIME, NUM_HOURS_WEEK)

library(BayesFactor)
bf = regressionBF(INCOME ~ ., data = us_income_data)
bf
bf2 = head(bf) / max(bf)
bf2
plot(bf2)

#### Using Bayesian Method Averaging Selecting
y <- us_income_data[,1]
x <- us_income_data[,-1]
# Linear regression with BMA
bma <- bicreg(x=x, y=y)
bma
# Listing the selected model
summary(bma)

### Using step-wise selection
library(MASS)
full <- lm(INCOME~.,data = us_income_data)
model0 <- lm(INCOME~1, data = us_income_data)
# Forward Selection
model.forward <- step(model0, scope = list(lower = model0, upper =full),direction = "forward")
# Backward Selection
model.backward <- step(full, scope = list(lower = model0, upper =full),direction = "backward")
# Selection from both ends
model.both <- step(full, scope = list(lower = model0, upper =full),direction = "both")

# Comparing the result
summary(model.forward)
summary(model.backward)
summary(model.both)
# Comparing with the full model
anova(model.forward, full)
anova(model.backward, full)
anova(model.both, full)
