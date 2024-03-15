# normal distribution
#1
v<-rnorm(5000, mean=100, sd=35)
#2
sequence<-seq(0, -1000)
den_f<-dnorm(sequence, mean=-500, sd=200)
#3
den_f.2<-dnorm(sequence, mean=-700, sd=150)
#4

#5
mu<-mean(sequence)
sd<-sd(sequence)
std<-(sequence-mu)/sd
std
# estimation
#A
#i
sample<-c(240000,450000,567148,230020,678458,456238,94832,3587,1000245,15000)
sd<-125000
n<-length(sample)
xbar<-mean(sample)
alpha<-0.2
zcritical<-qnorm(1-(alpha/2))
print(zcritical)
est_interval<- xbar+ c(zcritical, -zcritical)
print(est_interval)
#ii
s<-sd(sample)
print(s)
alp<-0.1
tcritical<-qt(alp/2, df=n-1)
print(tcritical)
estimate_interval<- xbar+c(tcritical, -tcritical)
print(estimate_interval)
#B
#i
file_1<-read.csv("Final Exam - 1.csv")
mean_estimator<-mean(file_1$socialmediahours)
print(mean_estimator)
#ii
sd_estimator<-sd(file_1$socialmediahours)
print(sd_estimator)
#iii
sigma<-1.5
sample_mean<- mean(file_1$socialmediahours)
alpha<-0.1
z_critical<- qnorm(1-(alpha/2))
print(z_critical)
est_interval<-sample_mean+ c(z_critical,-z_critical)
print(est_interval)
#iv
alpha.2<-0.01
z_critical.2<- qnorm(1-(alpha.2/2))
est_interval.2<- sample_mean+ c(z_critical.2, - z_critical.2)
print(est_interval.2)

#Hyp Testing
#A

income<-c(240000,450000,567148,230020,678458,456238,94832,3587,1000245,15000)
mu<-387500
n<-10
sd<-125000
alpha<- 0.05
# h0<- mu=387500, h1<- mu!=387500
xbar<- mean(income)
zstats<-(xbar-mu)/(sd/sqrt(n))    # z stats since population sd is known
print(zstats)
zcritical<- qnorm(1-alpha/2)      # test statistic  
print(zcritical)
if(abs(zstats < abs(zcritical))){            # decision criteria
  print("null hypothesis is not rejected")
} else print("null hypothesis is rejected")

#B
#i
mu<-2
alpha<-0.05
# h0=  mu=2 , h1=  mu!=2
xbar<-mean(file_1$socialmediahours)
n<- length(file_1$socialmediahours)
s<-sd(file_1$socialmediahours)
zstats<- (xbar-mu)/(s/sqrt(n))                                #using z stats since n is large enough though population sd is unknown
zcritical<-qnorm(1-(alpha/2))
print(zstats)
print(zcritical)
if(abs(zstats) < abs(zcritical)){
  print("null hypothesis  can not be rejected")
} else print("null hupothesis can be rejected")

#c

#h0<- mu<= 2.25, h1<- mu> 2.25
mu<- 2.25
sd<-1.5 
alpha<-0.05
zstats<- (xbar-mu)/(sd/sqrt(n))
print(zstats)
#right tailed test
zcrititcal<- qnorm(1-alpha)
print(zcritical)
  if(abs(zstats) < abs(zcritical)){
    print("null hypothesis  can not be rejected")
  } else print("null hupothesis can be rejected")
# significance level 1%
alpha<-0.01
zcritical<- qnorm(1-alpha)
print(zcritical)
if(abs(zstats) < abs(zcritical)){
  print("null hypothesis  can not be rejected")
} else print("null hupothesis can be rejected")

#significance 10%
alpha=0.1
zcritical<-qnorm(1-alpha)
print(zcritical)
if(abs(zstats) < abs(zcritical)){
  print("null hypothesis  can not be rejected")
} else print("null hupothesis can be rejected")
