rm(list=ls())

# QUESTION 1

jobdata <- read.csv(file.choose())
library(stargazer)
stargazer(jobdata, type = "text", title = "Summary Statistics",digits = 2)

#QUESTION 2 

    # a

library(BSDA)
t.test(jobdata$earn98, mu = 11000, conf.level = 0.95)

    # b 

t.test(jobdata$earn98, mu = 11000, alternative = "greater", conf.level = 0.95)

#Question 3

    # a

table(jobdata$train)
library(epiDisplay)
t.test(earn96 ~ train, data = jobdata)
t.test(earn98 ~ train, data = jobdata)

    # b

t.test(unem98 ~ married, data = jobdata, conf.level = 0.95)


# Question 4

  # a

model1 <- lm(earn98 ~ educ, data = jobdata)
library(tidyverse)
library(stargazer)
stargazer(model1,type = 'text')

  #b

model2 <- lm(earn98 ~ educ, data = subset(jobdata, black ==1))
model3 <- lm(earn98 ~ educ, data = subset(jobdata, black ==0))
model4 <- lm(earn98 ~ educ, data = subset(jobdata, hisp ==1))
model5 <- lm(earn98 ~ educ, data = subset(jobdata, hisp ==0))
stargazer(model2,type = "text",title = "Black")
stargazer(model3,type = "text",title = "Not Black")
stargazer(model4,type = "text",title = "Hispanic")
stargazer(model5,type = "text",title = "Not Hispanic")

  #c

earnings98 <- jobdata$earn98*1000
model6 <- lm(earnings98 ~ educ, data = jobdata)
stargazer(model6,type = "text")

  #d

model6 <- lm(earnings98 ~ educ, data = jobdata)
library(ggplot2)
ggplot(data=jobdata)+
  geom_point(aes(x=model6$fitted.values,y=model6$residuals))