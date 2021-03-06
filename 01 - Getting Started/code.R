library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv" 
download(url, destfile=filename)

#1 Read in the file femaleMiceWeights.csv and report the exact name of the column containing the weights. 
dat <- read.csv(filename, header = TRUE)

head(dat)
# Diet Bodyweight
# 1 chow      21.51
# 2 chow      28.14
# 3 chow      24.04
# 4 chow      23.45
# 5 chow      23.68
# 6 chow      19.79


#2 The [ and ] symbols can be used to extract specific rows and specific columns of the table.
# What is the entry in the 12th row and second column?
dat[12,2]
#[1] 26.25


#3 You should have learned how to use the $ character to extract a column from a table and return it as a vector.
#Use $ to extract the weight column and report the weight of the mouse in the 11th row. 

dat$Bodyweight
# [1] 21.51 28.14 24.04 23.45 23.68 19.79 28.40 20.98 22.51 20.10 26.91 26.25 25.71 26.37 22.80 25.34 24.97 28.14
# [19] 29.58 30.92 34.02 21.90 31.53 20.73


#4 The length function returns the number of elements in a vector. 
#How many mice are included in our dataset?  
length(dat$Diet)

#5 To create a vector with the numbers 3 to 7, we can use seq(3,7) or, because they are consecutive, 3:7. 
# View the data and determine what rows are associated with the high fat or hf diet. 
# Then use the mean function to compute the average weight of these mice. 

View(dat)
#hf = 13:24
mean(dat$Bodyweight[13:24])
#26.83417


#6 One of the functions we will be using often is sample. 
# Read the help file for sample using ?sample. Now take a random sample of size 1 
# from the numbers 13 to 24 and report back the weight of the mouse represented by that row. 
# Make sure to type set.seed(1) to ensure that everybody gets the same answer. 

?sample
set.seed(1)
x <- 13:24
sample(x,1)
dat$Bodyweight[16]
#[1] 25.34








