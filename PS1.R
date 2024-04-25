rm(list=ls())


jobdata <- read.csv(file.choose()) # Choose Job Training data
library(stargazer)
stargazer(jobdata, type = "text", title = "Summary Statistics",digits = 2)

 
''' a) A study aims to investigate whether the average earnings in 1998 differ from $10,000. Formulate the appropriate
null and alternative hypotheses for this study. '''

library(BSDA)
t.test(jobdata$earn98, mu = 11000, conf.level = 0.95)

    
''' b) A researcher is interested in determining if the mean earnings in 1998 exceeded $11,000. Formulate the null
and alternative hypotheses for this inquiry, and indicate the direction of the alternative hypothesis.''' 

t.test(jobdata$earn98, mu = 11000, alternative = "greater", conf.level = 0.95)


''' c) A study aims to discern any potential difference in earnings between cohorts of individuals who have undergone
job training and those who have not, utilizing data encompassing earnings from 1996 and 1998.'''

table(jobdata$train)
library(epiDisplay)
t.test(earn96 ~ train, data = jobdata)
t.test(earn98 ~ train, data = jobdata)

''' d) Does there exist a disparity in unemployment rates among different marital status groups in the year 1998?'''

t.test(unem98 ~ married, data = jobdata, conf.level = 0.95)



''' e) Perform a regression analysis to explore the relationship between earnings in 1998 (in $1000) and education
level.'''

model1 <- lm(earn98 ~ educ, data = jobdata)
library(tidyverse)
stargazer(model1,type = 'text')

''' earn98= 3.357 + 0.257(educ) + e   '''

''' f) Regress earnings in 1998 on education level by race. Show your R output and clearly write the estimated
Simple Regression Function. After conducting the regressions, compare the results between the two analyses.
Do you observe any differences? Provide your comments on the findings, considering the influence of
race on the relationship between earnings and education level. '''

model2 <- lm(earn98 ~ educ, data = subset(jobdata, black ==1))
model3 <- lm(earn98 ~ educ, data = subset(jobdata, black ==0))
model4 <- lm(earn98 ~ educ, data = subset(jobdata, hisp ==1))

stargazer(model2,type = "text",title = "Black")
stargazer(model3,type = "text",title = "Not Black")
stargazer(model4,type = "text",title = "Hispanic")
stargazer(model2 ,model3, model4, type='text')
''' The effect of education on earnings is most in hispanics, followed closely by whites and least in black. '''

''' g) Now generate a new variable "earnings 98" where the earnings are in the actual amount in $ instead of 1000$.
Regress earnings98 on education level. Show your R output and clearly write the estimated Simple Regression
Function. '''

earnings98 <- jobdata$earn98*1000
model6 <- lm(earnings98 ~ educ, data = jobdata)
stargazer(model6,type = "text")

''' earnings = 3257 + 608(educ)  ; is the estimated linear regression'''

''' h) After conducting regression analyses, researchers often examine the relationship between the fitted values
and residuals to detect potential issues such as heteroskedasticity. Perform a diagnostic analysis by plotting
the fitted values against the residuals for the regression model of earnings in 1998 on education level. Is
there presence of heteroskedasticity? Why or why not?'''

model6 <- lm(earnings98 ~ educ, data = jobdata)
library(ggplot2)
ggplot(data=jobdata)+
geom_point(aes(x=model6$fitted.values,y=model6$residuals))

''' As can be seen from teh plot there is a huge variance in e with increase in x, var(u|x) != constant. Our estimator 
is heteroskedastic.''' 


