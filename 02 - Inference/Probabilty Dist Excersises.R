#First, install the gapminder data using:

install.packages("gapminder")

library(gapminder)
?gapminder
data(gapminder)
View(head(gapminder))

#Create a vector 'x' of the life expectancies of each country for the year 1952. 
#Plot a histogram of these life expectancies to see the spread of the different countries.

x <- gapminder$lifeExp
x <- subset(gapminder, year == '1952')$lifeExp
x
hist(x)

# Probability Distributions Exercises #1
#In statistics, the empirical cumulative distribution function 
#(or empirical cdf or empirical distribution function) is the function F(a) for any a, 
#which tells you the proportion of the values which are less than or equal to a.

#We can compute F in two ways: the simplest way is to type mean(x <= a). 
#This calculates the number of values in x which are less than or equal a, 
#divided by the total number of values in x, in other words the proportion of values less than or equal to a.

#The second way, which is a bit more complex for beginners, is to use the ecdf() function. This is a bit complicated because this is a function that doesn't return a value, but a function.
#Let's continue, using the simpler, mean() function.

#What is the proportion of countries in 1952 that have a life expectancy less than or equal to 40?

mean(x <= 40)
#[1] 0.2887324


# Probability Distributions Exercises #2
#What is the proportion of countries in 1952 that have a life expectancy between 40 and 60 years? 
#Hint: this is the proportion that have a life expectancy less than or equal to 60 years, 
#minus the proportion that have a life expectancy less than or equal to 40 years.


mean(x <= 60) - mean(x <= 40)
#[1] 0.4647887



#W custom fctn
prop = function(q) {
  mean(x <= q)
}
prop(40)
#Now let's build a range of q's that we can apply the function to:

qs = seq(from=min(x), to=max(x), length=20)
qs
#Now we can use sapply() to apply the 'prop' function to each element of 'qs':

props = sapply(qs, prop)
props
plot(qs, props)

#Note that we could also have written this in one line, by defining the 'prop' function but without naming it:

props = sapply(qs, function(q) mean(x <= q))
#This last style is called using an "inline" function or an "anonymous" function. 
#Let's compare our homemade plot with the pre-built one in R:

plot(ecdf(x))






















