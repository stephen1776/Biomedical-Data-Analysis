library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

?sample
#Here x represents the weights for the entire population. 
#1. What is the average of these weights?
mean(x)


#2. After setting the seed at 1, set.seed(1) take a random sample of size 5. 
#What is the absolute value (use abs) of the difference between the average of the sample 
#and the average of all the values? 
set.seed(1)
S <- sample(x,5)
abs(mean(S) - mean(x))