# Normal Dist Ex
#Here x represents the weights for the entire population.

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

#1 
#Using the same process as before (in Null Distribution Exercises), set the seed at 1, 
#then using a for-loop take a random sample of 5 mice 1,000 times. 
#Save these averages.  

set.seed(1)
n <- 1000
norm5 <- vector("numeric",n)
for (i in 1:n) {
  RS5 <- sample(x,5)
  #norm5[i] <- abs(mean(RS5) - mean(x))
  norm5[i] <- mean(RS5)
}
hist(norm5)
#2Set the seed at 1,then using a for-loop take a random sample of 50 mice 1,000 times. 
#Save these averages.
set.seed(1)
n <- 1000
norm50 <- vector("numeric",n)
for (i in 1:n) {
  RS50 <- sample(x,50)
  norm50[i] <- mean(RS50)
}
hist(norm50)
#2 For the last set of averages, the ones obtained from a sample size of 50, 
#what proportion are between 23 and 25? 
mean(x)
#[1] 23.89338
length(which(norm50 < 25 & norm50 > 23))/n
#[1] 0.976
# or mean( averages50 < 25 & averages50 > 23)
mean( norm50 < 25 & norm50 > 23)
#[1] 0.976

#10
#Now ask the same question of a normal distribution with average 23.9 and standard deviation 0.43
?pnorm
rns50 <- rnorm(50, 23.9, 0.43)
mean( rns50 < 25 & rns50 > 23)
# [1] 0.98


















