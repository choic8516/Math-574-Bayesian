## Comparing two models using Bayes Factor

library(LearnBayes)

library(tidyverse)
us_income_data <- read.csv("us_income_dataset.csv")
colnames(us_income_data)[colnames(us_income_data)=="WEEKS_WORKED_NUMERIC"] <- "NUM_HOURS_WEEK"
us_income_data <- us_income_data %>%
        select(INCOME, AGE, NUM_CHILD, TRAVEL_TIME, NUM_HOURS_WEEK)

mode_guess <- mean(us_income_data$NUM_CHILD)

datapar_1=list(data=us_income_data$NUM_CHILD[1:2000],par=c(4.57,1.43))
fit1=laplace(logpoissgamma,.5,datapar_1)
datapar_2=list(data=us_income_data$NUM_CHILD[1:2000],par=c(1,.5))
fit2=laplace(logpoissnormal,.5,datapar_2)
datapar_3=list(data=us_income_data$NUM_CHILD[1:2000],par=c(2,.5))
fit3=laplace(logpoissnormal,.5,datapar_3)
datapar_4=list(data=us_income_data$NUM_CHILD[1:2000],par=c(1,2))
fit4=laplace(logpoissnormal,.5,datapar_4)

postmode=c(fit1$mode,fit2$mode,fit3$mode,fit4$mode)
postsd=sqrt(c(fit1$var,fit2$var,fit3$var,fit4$var))
logmarg=c(fit1$int,fit2$int,fit3$int,fit4$int)
results <- cbind(postmode,postsd,logmarg)
results <- as.data.frame(results)
rownames(results) <- c("fit1", "fit2", "fit3", "fit4")

# normal prior wins
BF12 <- results["fit1", "logmarg"] / results["fit2", "logmarg"]

# normal prior wins
BF13 <- results["fit1", "logmarg"] / results["fit3", "logmarg"]

# conjugate gamma prior wins
BF14 <- results["fit1", "logmarg"] / results["fit4", "logmarg"]

# normal prior wins
BF23 <- results["fit2", "logmarg"] / results["fit3", "logmarg"]

# normal prior wins
BF24 <- results["fit2", "logmarg"] / results["fit4", "logmarg"]

# normal prior wins
BF34 <- results["fit3", "logmarg"] / results["fit4", "logmarg"]

data.frame(BF12, BF13, BF14, BF23, BF24, BF34)        
