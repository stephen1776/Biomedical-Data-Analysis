#CLT and t-distribution in Practice Exercises
#Exercises 3-13 use the mouse data set we have previously downloaded:
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename)

#1
# Suppose we are interested in the proportion of times we see a 6 when rolling n=100 die. 
# This is a random variable which we can simulate with x=sample(1:6, n, replace=TRUE) 
# and the proportion we are interested in can be expressed as an average: mean(x==6). 
# Because the die rolls are independent, the CLT applies.
# We want to roll n dice 10,000 times and keep these proportions. 

# This random variable (proportion of 6s) has mean p=1/6 and variance p*(1-p)/n. 
# So according to CLT z = (mean(x==6) - p) / sqrt(p*(1-p)/n) should be normal with mean 0 and SD 1. 
# Set the seed to 1, then use replicate to perform the simulation, 
# and report what proportion of times z was larger than 2 in absolute value (CLT says it should be about 0.05). 
#https://stackoverflow.com/questions/38992531/using-replicate-central-limit-t#39003018
#https://duckduckgo.com/?q=This+random+variable+(proportion+of+6s)+has+mean+p%3D1%2F6+and+variance+p*(1-p)%2Fn.++So+according+to+CLT+z+%3D+(mean(x%3D%3D6)+-+p)+%2F+sqrt(p*(1-p)%2Fn)+should+be+normal+with+mean+0+and+SD+1.+then+use+replicate+to+perform+the+simulation&t=ffnt&atb=v62-3&ia=qa&iax=1
library(dplyr)
library(rafalib)
head(dat)
set.seed(1)
n <- 100
N <- 10000
p <- 1/6
#var <- p*(1-p)/n
#x <- sample(1:6, n, replace=TRUE)
#z <- (mean(x==6) - p) / sqrt(p*(1-p)/n)
sim1 <- replicate(N,{
  x <- sample(1:6, n, replace=TRUE)
  z <- (mean(x==6) - p) / sqrt(p*(1-p)/n)
  
})
#head(sim1)
mean( abs(sim1)> 2)
qqnorm((sim1))
abline(0,1)
#[1] 0.0424
#answer codes:
# set.seed(1)
# n <- 100
# sides <- 6
# p <- 1/sides
# zs <- replicate(10000,{
#   x <- sample(1:sides,n,replace=TRUE)
#   (mean(x==6) - p) / sqrt(p*(1-p)/n)
# }) 
# qqnorm(zs)
# abline(0,1)#confirm it's well approximated with normal distribution
# mean(abs(zs) > 2)

#2
# For the last simulation you can make a qqplot to confirm the normal approximation. 
# Now, the CLT is an asympototic result, meaning it is closer and closer to being a perfect 
# approximation as the sample size increases. 
# In practice, however, we need to decide if it is appropriate for actual sample sizes. Is 10 enough? 15? 30?
# 
# In the example used in exercise 1, the original data is binary (either 6 or not). 
# In this case, the success probability also affects the appropriateness of the CLT. 
# With very low probabilities, we need larger sample sizes for the CLT to "kick in".
# 
# Run the simulation from exercise 1, but for different values of p and n. 
# For which of the following is the normal approximation best?
# 


#sim2
p <- 0.5
n <- 5
sim2 <- replicate(N,{
  x <- sample(1:6, n, replace=TRUE)
  z <- (mean(x==6) - p) / sqrt(p*(1-p)/n)
  
})
qqnorm((sim2))
abline(0,1)

#sim3
p <- 0.5
n <- 30
sim3 <- replicate(N,{
  x <- sample(1:6, n, replace=TRUE)
  z <- (mean(x==6) - p) / sqrt(p*(1-p)/n)
  
})
qqnorm((sim3))
abline(0,1)

#sim4
p <- 0.01
n <- 30
sim4 <- replicate(N,{
  x <- sample(1:6, n, replace=TRUE)
  z <- (mean(x==6) - p) / sqrt(p*(1-p)/n)
  
})
qqnorm((sim4))
abline(0,1)

#sim5
p <- 0.01
n <- 100
sim5 <- replicate(N,{
  x <- sample(1:6, n, replace=TRUE)
  z <- (mean(x==6) - p) / sqrt(p*(1-p)/n)
  
})
qqnorm((sim5))
abline(0,1)
#sim 3 best b/c of n and p. p too far away from 1/6 on sim4-5 see abline(0, 1) 
#answercode
# ps <- c(0.5,0.5,0.01,0.01)
# ns <- c(5,30,30,100)
# library(rafalib)
# mypar(4,2)
# for(i in 1:4){
#   p <- ps[i]
#   sides <- 1/p
#   n <- ns[i]
#   zs <- replicate(10000,{
#     x <- sample(1:sides,n,replace=TRUE)
#     (mean(x==1) - p) / sqrt(p*(1-p)/n)
#   }) 
#   hist(zs,nclass=7)
#   qqnorm(zs)
#   abline(0,1)
# }



#3
# As we have already seen, the CLT also applies to averages of quantitative data. 
# A major difference with binary data, for which we know the variance is
# p(1 - p), is that with quantitative data we need to estimate the population standard deviation.
# 
# In several previous exercises we have illustrated statistical concepts with the unrealistic situation 
# of having access to the entire population. In practice, we do *not* have access to entire populations. 
# Instead, we obtain one random sample and need to reach conclusions analyzing that data. 
# dat is an example of a typical simple dataset representing just one sample. 
# We have 12 measurements for each of two populations:
#   
# X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
# Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
# 
# We think of X as a random sample from the population of all mice in the control diet and
# Y as a random sample from the population of all mice in the high fat diet.
# 
# Define the parameter mu_X as the average of the control population. 
# We estimate this parameter with the sample average . What is the sample average?

X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
X_bar <- mean(X)
X_bar
#23.81333


#4
# We don't know mu_x, but want to use X_bar to understand mu_X. 
# Which of the following uses CLT to understand how well X_bar approximates mu_X? 

# D X_bar ~ N(mu_x, sigma/sqrt(n))


#5
# 5. The result above tells us the distribution of the following random variable: 
# Z = sqrt(12)*(X_bar - mu_X)/sigma_X.
# What does the CLT tell us is the mean of Z (you don’t need code)?
# mean of 0

#6
# The result of 4 and 5 tell us that we know the distribution of the difference between 
# our estimate and what we want to estimate, but don't know. 
# However, the equation involves the population standard deviation sigma_X, which we don't know. 
# Given what we discussed, what is your estimate of sigma_X? 
sigma_X <- popsd(X)/sqrt(12)
sigma_X
#0.8725323 not correct
#sigma_X <- popsd(X)/sqrt(12) 0.8353861 not correct
populationvar <- mean((X-mean(X))^2)
sqrt(populationvar)
# 2.893862 not correct
# 1 not correct
# 1/sqrt(12) = 0.2886751 not correct
sd(X) # sigma_X is the population sd
# 3.022541 correct


#7
# Use the CLT to approximate the probability that our estimate X_bar is off by more than 2 grams from mu_X. 

z <- sqrt(12)*(X_bar)/sd(X)
z
mean(z)
mean( abs( z - mean(X) ) > 2)

mean(X)
mean(X_bar)
#



















#13
# The p-value calculated from t.test is 0.053, however it is 0.0519248 if calculate as the below: 
#   xbar <- mean(X); ybar <- mean(Y); se <- sqrt( (var(X) + var(Y)) / 12 ); 
# tstat <- abs( mean(X) - mean(Y) ) / se; p-val <- pt( tstat, df=22 ) * 2; 
# if the df=22 above was replaced by 20.236, 
# i.e. pt(tstat, df=20.236)*2 we got the same p-value of 0.05299888. 
# Could you please explain a bit on this? Thank you.
# 









