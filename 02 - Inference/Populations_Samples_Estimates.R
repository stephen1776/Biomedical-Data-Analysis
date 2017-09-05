# Populations, Samples, and Estimates
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename) 
#added this in later
dat <- na.omit(dat)
#1 Use dplyr to create a vector x with the body weight of all males on the control (chow) diet. 
#What is this population's average? 
library(dplyr)
head(dat)
x <- filter(dat, Sex %in% "M" & Diet == "chow") %>%
  select(Bodyweight) %>% 
  unlist

head(x)
mean(x)
#[1] 30.96381



#2 Now use the rafalib package and use the popsd function to compute the population standard deviation. 

install.packages("rafalib")
library(rafalib)
popsd(x, na.rm = TRUE)
#[1] 4.420545

#3
#Set the seed at 1. 
#Take a random sample X of size 25 from x. What is the sample average? 

set.seed(1)

rs_X <- sample(x, 25)
mean(rs_X)
#[1] 31.5492 is wrong - asked discussion group
# 32.0956 correct after removing NA's from the original data set

#4 Use dplyr to create a vector y with the body weight of all males on the high fat hf) diet. 
#What is this population's average? 

y <- filter(dat, Sex %in% "M" & Diet == "hf") %>%
  select(Bodyweight) %>% 
  unlist

mean(y)
# 34.84793


#5 Now use the rafalib package and use the popsd function to compute the population standard deviation.
popsd(y)
# 5.574609


#6 Set the seed at 1. Take a random sample of size 25 from y. 
#What is the sample average?
set.seed(1)
rs_Y <- sample(y, 25)
mean(rs_Y)
# 34.768

#7 What is the difference in absolute value between mean(y) - mean(x) and mean(rs_y) - mean(rs_X)? 
abs((mean(y) - mean(x)) - (mean(rs_Y) - mean(rs_X)))
# 1.211716

#8
#Repeat the above for females. 
#Make sure to set the seed to 1 before each sample call. 
#What is the difference in absolute value between mean(y) - mean(x) and mean(rs_y) - mean(rs_X)? 
x <- filter(dat, Sex %in% "F" & Diet == "chow") %>%
  select(Bodyweight) %>% 
  unlist
y <- filter(dat, Sex %in% "F" & Diet == "hf") %>%
  select(Bodyweight) %>% 
  unlist
set.seed(1)
rs_X <- sample(x, 25)
set.seed(1)
rs_Y <- sample(y, 25)
abs((mean(y) - mean(x)) - (mean(rs_Y) - mean(rs_X)))
# 0.7364828













