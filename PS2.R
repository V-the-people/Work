# loading data set
rm(list=ls())
nba_data <- read.csv("PS2_nbasal.csv")
sleep_data <- read.csv("PS2_Sleep75.csv")
wage_data <- read.csv("PS2_wage2.csv")

#loading libraries

library(car)
library(stargazer)
library(wooldridge)


# a) Estimate a model relating points-per-game (points) to years in the league (exper),
# age, and years played in college (coll). Include a quadratic in exper; the other variables
# 7should appear in level form. Report the results in the usual way.

View(nba_data)
ppg <- lm(points~exper+expersq+coll+age, data = nba_data)
summary(ppg)


#  b) Holding college years and age fixed, at what value of experience does the next
# year of experience actually reduce points-per-game? Does this make sense? 


#  We could create an equation relating points per game with just exper and expersq,knowing their coefficients, and then differentiate ppg with exper
#  and then find that value of exper for d(ppg)/d(exper)<0 

points<- expression((exper*exper)*-0.07657 + exper*2.34994 ) 
first_order<- deriv(points, 'exper')
print(first_order)
exper= 2.3499/0.15314
print(exper)  # somehow is wrong

# c) Add a quadratic in age to the equation. Is it needed? What does this appear to
# imply about the effects of age, once experience and education are controlled for?
  
ppg2 <- lm(points~exper+expersq+age+agesq+coll, data = nba_data)
stagazer(ppg2, type='text')

# though the age squared coefficient is statistically insignificant at 5% siginificance, yet it is neccesary to be icluded since the effect of age on performancc
#(here points per game) might not be linearly related; agesq helps in capturing that non-linearity.

# d) Now regress log(wage) on points, exper, exper-square, age, and coll. Report the
# results in the usual format.

log_wage <- lm(lwage~points+exper+expersq+age+coll, data = nba_data)
stargazer(log_wage, type='text')
# Increase in the points scored by 1 increases wage by 7.8% on average.

#Test whether age and coll are jointly significant in the regression from part (iv).
# What does this imply about whether age and education have separate effects on wage,
# once productivity and seniority are accounted for?

# H0: B4 = B5=0; H1: B4 or B5 !=0
linearHypothesis(model = log_wage, c("age=0", "coll=0"))

#This results show that age and college don't have separate effects on wage once productivity and seniority are accounted for.

# e) Estimate the model:
# log(wage) = β0 + β1educ + β2exper + β3tenure + β4married + β5black + β6south + β7urban + u
# and report the results in the usual form. Holding other factors fixed, what is the
# approximate difference in monthly salary between blacks and nonblacks? Is this difference statistically significant?


View(wage_data)
log_wage <- lm(lwage~educ+exper+tenure+married+black+south+urban, data = wage_data)
summary(log_wage)

#Blacks earn 18.8% less than non-blacks on average,  value being statistically significant.

#Add the variables exper-square and tenure-square to the equation and show that
#they are jointly insignificant at even the 20% level of significance.

# inserting tenure square and experience square into the data 

wage_data$tenuresq <- wage_data$tenure^2
wage_data$expersq <- wage_data$exper^2

log_wage2 <- lm(lwage~educ+exper+tenure+married+black+south+urban+expersq+tenuresq, data = wage_data)
linearHypothesis(model = log_wage2, c("expersq=0","tenuresq=0" ))

# joint significance test reveals a statistic of 1.4898 with a probability of 22.6% making it insignificant even at the 20% level.

# f) Extend the original model to allow the return to education to depend on race and
# test whether the return to education does depend on race.

# inserting interaction term of education and race into wage data

wage_data$edu_race <- wage_data$educ * wage_data$black
# H0: B( of educ_race)=0 ; H1= B!=0
log_wage3 <- lm(lwage~educ+exper+edu_race+tenure+married+black+south+urban, data = wage_data)
summary(log_wage3)

# the effect of interaction term on log wage is inginificant at 99% confidence, hence returns to education does not 
# depend on race

# g) Again, start with the original model, but now allow wages to differ across four
# groups of people: married and black, married and nonblack, single and black, and
# single and nonblack. What is the estimated wage differential between married blacks
# and married nonblacks?

#inserting interaction terms
wage_data$marriedblack = wage_data$black*wage_data$married

log_wage4 <- lm(lwage~educ+exper+tenure+married+black+marriedblack+south+urban+expersq+tenuresq, data = wage_data)
summary(log_wage4)

# wage differential b/w marriedblacks and married non-blacks = E[lw|married=1 and black=1]- E[lw|married=1 and black=0];
# = (B5+B6); married non blacks earn 18.24% less than married non-blacks.



# #h)The equation interest is:
# sleep = β0 + β1totwrk + β2educ + β3age + β4age2 + β5yngkid + u
# Estimate this equation separately for men and women and report the results in the
# usual form. Are there notable differences in the two estimated equations?

male_subset <- subset(sleepdata, sleep_data$male ==1)
female_subset <- subset(sleepdata, sleep_data$male ==0)

male_sleep <- lm(sleep~totwrk+educ+age+agesq+yngkid, data = male_subset)
female_sleep <- lm(sleep~totwrk+educ+age+agesq+yngkid, data = female_subset)
summary(male_sleep)
summary(female_sleep)

#From the intercepts it is clear that females, negating other factors, have a higher level of sleep on average.
#Interestingly for men the existence of a young kid increases sleep for men while reducing sleep for women, 
#though values ares statistically insignificant in both cases.


# Create five interaction variables such that the dummy variable male is interacted
# with each of the five variables - totwrk, educ, age, age-squared and yngkid
# inserting interaction into dataset male_subset

male_subset$m_totwrk <- male_subset$male*male_subset$totwrk
male_subset$m_educ <- male_subset$male*male_subset$educ
male_subset$m_age <- male_subset$male*male_subset$age
male_subset$m_agesq <- male_subset$male*male_subset$agesq
male_subset$m_yngkid <- male_subset$male*male_subset$yngkid
View(sleep_data)


# inserting interation terms into female_subset
# but for all the corresponding female interaction terms would be zero since dummy female=0 
# female_subset$f_totwrk<- female_subset$feamle(=0)* female_subset$totwrk

male_sleep<- lm(sleep~age+agesq+yngkid+totwrk+educ+m_totwrk+m_educ+m_age+m_agesq+m_yngkid, data=male_subset )
female_sleep<-lm(sleep~age+agesq+yngkid+totwrk+educ, data=female_subset)
summary(male_sleep)
summary(female_sleep)
# slopes and intercepts are different acrss terms between male and female

overall<-lm(sleep~age+agesq+yngkid+totwrk+educ+m_totwrk+m_educ+m_age+m_agesq+m_yngkid, data=sleep_data)
summary(overall)
