# Central Limit Theorem Exercises

library(downloader) 
library(rafalib)
library(dplyr)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- na.omit( read.csv(filename) )

#1
#If a list of numbers has a distribution that is well approximated by the normal distribution, 
#what proportion of these numbers are within one standard deviation away from the list's average? 
# 0.68

#2 What proportion of these numbers are within two standard deviations away from the list's average? 
# 0.95

#3 What proportion of these numbers are within three standard deviations away from the list's average? 
# 0.99

#4 Define y to be the weights of males on the control diet. 
# What proportion of the mice are within one standard deviation away from the average weight 
#(remember to use popsd for the population sd)? 
head(dat)
y <- filter(dat, Sex %in% "M" & Diet == "chow") %>%
  select(Bodyweight) %>% 
  unlist
#To find out the proportion 1 sd away we want to know z 
#so which z<=1 and z>=-1 or just abs(z)<=1. Then divide that by the number of total cases. 
#So we want want sum(abs(z)<=1)/length(z) or just do mean(abs(z)<=1). 
z <- (y - mean(y)) / popsd(y) 
mean(abs(z) <= 1)
# 0.6950673

#5
#What proportion of these numbers are within two standard deviations away from the list's average? 

mean(abs(z) <= 2)
#0.9461883

#6
#What proportion of these numbers are within three standard deviations away from the list's average? 
mean(abs(z) <= 3)
#0.9910314

#7
#Note that the numbers for the normal distribution and our weights are relatively close. 
#Also, notice that we are indirectly comparing quantiles of the normal distribution to quantiles 
#of the mouse weight distribution. We can actually compare all quantiles using a qqplot. 
#Which of the following best describes the qq-plot comparing mouse weights to the normal distribution? 

qqnorm(z)
abline(0,1)
#C) The mouse weights are well approximated by the normal distribution, although
#the larger values (right tail) are larger than predicted by the normal. 
#This is consistent with the differences seen between question 3 and 6.

#8
#Create the above qq-plot for the four populations: male/females on each of the two diets. 
#What is the most likely explanation for the mouse weights being well approximated? 
#What is the best explanation for all these being well approximated by the normal distribution? 

mypar(2,2)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
# B) This just happens to be how nature behaves. 
#Perhaps the result of many biological factors averaging out.

#9
# We will now take a sample of size 25 from the population of males on the chow diet. 
# The average of this sample is our random variable. We will use the replicate to 
# observe 10,000 realizations of this random variable. Set the seed at 1, 
# generate these 10,000 averages. 
# Make a histogram and qq-plot of these 10,000 numbers against the normal distribution.
# We can see that, as predicted by the CLT, the distribution of the random variable 
# is very well approximated by the normal distribution.

y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
set.seed(1)
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
# What is the average of the distribution of the sample average? 
mean(avgs)
#30.95581

#10
# What is the standard deviation of the distribution of sample averages? 
sd(avgs)
# 0.8368611


















