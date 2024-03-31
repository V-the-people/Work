library(ggplot2)
library(dplyr)
library(reshape2)
library(gganimate)

file<-read.csv(file.choose())
Avg_temp<-file$Avg_mean

# rain$mean_ann_rain<- apply(rain[,-1], 1, function(x) mean(x, na.rm=TRUE))
#1

ggplot(data=file)+ geom_line( mapping= aes(x=year, y=Avg_temp), color="blue")+
  geom_line( mapping=aes(x=year, y=Avg_rainfall/50), color="red")+ 
  scale_y_continuous("Temperature in celsius(blue)", sec.axis = sec_axis(~ . *50, name= "Rainfall in cms(red)"))+
  theme_bw() + geom_smooth( mapping=aes(x=year, y=Avg_rainfall/50),color="red", se=FALSE)+
  geom_smooth( mapping= aes(x=year, y=Avg_temp), color="blue", se=FALSE)+ theme_bw()+
  ggtitle("Average trends of rainfall and temperature")

#2
ggplot(file)+ geom_point( mapping= aes(x=year, y=Avg_temp), color="blue")+
  geom_point( mapping=aes(x=year, y=Avg_rainfall/50), color="red")+
  scale_y_continuous("Temperature in celsius(blue)", sec.axis = sec_axis(~ . *50, name= "Rainfall in cms(red)"))+
  theme_bw() + stat_smooth( mapping=aes(x=year, y=Avg_rainfall/50),color="red", se=FALSE, method = "lm")+
  stat_smooth( mapping= aes(x=year, y=Avg_temp), color="blue", se=FALSE, method = 'lm')+
  ggtitle("Average trends of rainfall and temperature")

#3
''' ggplot(file)+ geom_line(mapping= aes(x=year, y=Avg_temp), color="blue")+
  geom_line( mapping=aes(x=year, y=Avg_rainfall/50), color="red")+ 
  scale_y_continuous("Temperature in celsius(blue)", sec.axis = sec_axis(~ . *50, name= "Rainfall in cms(red)"))+
  theme_bw() +ggtitle("Average trends of rainfall and temperature")+  transition_reveal(year)'''

# R does not allow for two independent Y axis, it allows only a transformation of primary or first axis to be 
# used as a second axis, hence the transformation of '*50' in sec_axis(), also 'y=mean_ann_rain' is divided by
# 50 in order to reduce the range of mean_ann_rain to fall near the range of temperature mean , this just changes the
# numerical value and the variation and relative proportion remains conserved and hence the pattern too. There could 
# still be lingering confusion about the role of 'sec_axis'. The maximum value in Temp or the range of Temp is around
# 50 to 60 times smaller than the maximum value in mean_ann_rain and therefore the division by 50, what sec_axis does 
# that it just displays the transformation of primary axis and since the rainfall values were 50 to 60 times larger than
# Temp values hence the second axis was transformed into 50 times larger by sec_axis to be able to display those ranges.
                     
                     
