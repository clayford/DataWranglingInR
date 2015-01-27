#' ---
#' title: "Data Wrangling in R: Generating/Simulating data"
#' author: "Clay Ford"
#' date: "Spring 2015"
#' output: pdf_document
#' ---


# It is often desirable to generate fake data. Sometimes we just want data to 
# play around with. Other times we want to generate data similar to what we 
# expect to collect to see if our proposed analysis works as expected. R has
# excellent facilities for generating random data.


# sampling data -----------------------------------------------------------

# In statistics we usually can't measure every member of population. The best we
# can do is take a sample and then use that sample as a surrogate for the 
# population of interest. A common example is national election polling. It's 
# impractical to ask every registered voter who they will vote for. Therefore 
# pollsters will take a (hopefully random) sample of about 1000 people to draw
# inferences about how the entire population of registered voters will vote.

# R allows us to simulate this kind of random sampling. A basic but powerful 
# function for this is sample(). The syntax is sample(x, size, replace) where x 
# is either a vector of one or more elements from which to choose, or a positive
# integer, size is a non-negative integer giving the number of items to choose, 
# and replace is a logical setting about whether sampling be with replacement
# (The default is FALSE).

# We can use sample to generate the roll of a die
sample(x=1:6, size=1)

# With replicate(), we can simulate the roll of a die 1000 times. The syntax for
# replicate() is replicate(n, expr) where n is the number of replications and 
# expr is the expression to replicate. Here we replicate 1000 rolls of a fair
# die.
replicate(n=1000, sample(x=1:6, size=1))

# Using table, we can simulate a 1000 die rolls and tally up the totals
table(replicate(n=1000,sample(1:6,1)))

# To simulate rolling two dice, or rolling a die two times, we need to set size
# = 2 and replace = TRUE. If we don't set replace = TRUE, then we would never
# roll a pair.
sample(x=1:6, size=2, replace=T)

# again we can use replicate to simulate rolling two dice a 1000 times. The
# result is a matrix.
diceRolls <- replicate(n=1000, sample(x=1:6, size=2, replace = TRUE))
# First 10 rolls:
diceRolls[,1:10]

# and then we can summarize the distribution of the sum of the dice:
table(apply(diceRolls, 2, sum))

# and graph if we want:
barplot(table(apply(diceRolls, 2, sum)))

# generating random data from a probability distribution ------------------

# Theoretical probability distributions are often used to model distributions of
# data in real life. For example, let's look at the airquality dataset that
# comes with R:
head(airquality)

# How are the Temperature values distributed? We can use the hist() function to
# get an idea:
hist(airquality$Temp)

# We can think of the ranges on the x-axis as bins and the bars representing 
# items stacked in the bins. Notice the y axis says "Frequency". We can also 
# think of the bars as representing proportions such that they all sum to 1 or 
# 100%. We can have R create the histogram with this representation using
# freq=FALSE.
hist(airquality$Temp, freq=FALSE)

# Notice the y-axis has changed to Density. To calculate the approximate 
# proportion of values in, say, the 65-70 bin, multiply the bin width by the bar
# height. We can do this by saving the histogram to an object:
ho <- hist(airquality$Temp, freq=FALSE)

# investigate the object. Notice it contains the bin breaks ("breaks") and bar
# heights, ("density"):
str(ho)

# we can calculate the proportions exactly as follows:
diff(ho$breaks) * ho$density

# notice they sum to 1
sum(diff(ho$breaks) * ho$density)

# Histograms with bars as proportions can be thought of as an empirical
# distribution. It turns out that many such distributions can be modelled or
# approximated with theoretical distributions. Below is one way to superimpose a
# theoretical distribution over an empirical distribution:
x <- 55:100
lines(x, y = dnorm(x, mean = mean(airquality$Temp), sd=sd(airquality$Temp)))

# We see the empirical distribution of Temps is pretty close to a theoretical 
# Normal distribution with mean and standard deviation equal to the mean and
# standard deviation of the Temps data.

# The lines() function plots lines by plugging the values in x into the 
# expression in y. The mathematical expression for a normal curve can be called 
# with the dnorm() function. It's arguments are x (numbers on the x-axis), mean 
# and standard deviation. The mean is the value at the hump and the standard 
# deviation is the distance from the mean to the inflection point on the graph.
# Formally we say the normal distribution has two parameters.

# R lets us easily explore the Normal distribution
plot(x=seq(1,20,length.out = 200), y = seq(0,0.5, length.out = 200), 
     type="n", xlab="", ylab="")
x <- seq(1,20,length.out = 200)
lines(x, y = dnorm(x, mean = 10, sd=3), col=1)
lines(x, y = dnorm(x, mean = 10, sd=2), col=2)
lines(x, y = dnorm(x, mean = 15, sd=2), col=3)
lines(x, y = dnorm(x, mean = 15, sd=1), col=4)
legend("topleft",legend = c("N(10,3)","N(10,2)","N(15,2)","N(15,1)"),col = 1:4, lty=1)

# Now that we know a little about the normal distribution, let's explore how we 
# can randomly generate data from it. Using the rnorm() function, we can 
# generate values from a Normal distribution of our choosing. For instance, 
# maybe I want some fake data representing the weights of male college freshmen.
# It's not a stretch to assume these data would be normally distributed, say 
# with a mean of 175 and a standard deviation of 15. We can generate a 100
# values from this hypothetical distribution as follows:
weights <- rnorm(n = 1000, mean = 175, sd = 15)
head(weights)
# The histogram of this data should look normal:
hist(weights, freq=F)
# It does. How does the theoretical normal curve fit?
x <- seq(120, 220, length.out = 200)
lines(x, y=dnorm(x, 175, 15))

# In simple linear regression we assume errors are normally distributed with 
# mean = 0 and some finite standard deviation. We can use rnorm() to generate 
# random errors and hence reverse engineer data suitable for simple linear
# regression:
x <- seq(10,50,length.out = 100)
y <- 10 + 5*x + rnorm(100,sd=10)
plot(x,y)
mod <- lm(y ~ x)
summary(mod)
abline(mod)

# simulate two-sample t tests to find power

# there is a function for this:
power.t.test(n = 20, delta = 1)

# Let's do a t-test with some sample data
tout <- t.test(rnorm(20,5,1),rnorm(20,6,1),alternative = "two.sided")
# note the structure of tout
str(tout)
# pull out just the p-value
tout$p.value

# Now do 1000 t-tests:
out <- replicate(1000, t.test(rnorm(20,5,1),rnorm(20,6,1),alternative = "two.sided")$p.value)
# count the number of p-values less than 0.05 and divide by 1000 to estimate "power"
sum(out < 0.05)/1000


# We can pretty much do this for any model.

# simulate two-sample t tests to find sample size

power.t.test(delta = 1, power = 0.8)

tpower <- function(n, N=1000){
  out <- replicate(N, t.test(rnorm(n,5,1),rnorm(n,6,1),alternative = "two.sided")$p.value)
  sum(out < 0.05)/1000
}

pest <- sapply(10:30,tpower)
plot(10:30,pest, type="b")
abline(h=0.8) # add line for 80% power


nout <- numeric(length(10:30))
for(i in 10:30){
  nout[i] <- tpower(n=12)
}



# set seed to replicate results; doesn't matter what the "seed" is.
set.seed(123)

# generating known data or fixed levels

rep(c("male","female"), times=4)
rep(c("male","female"), each=4)
rep(c("male","female"), length.out=4)

# Notice these are character class.
class(rep(c("male","female"), length.out=4))

# If we wanted a factor we would need to explicitly declare it a factor
factor(rep(c("male","female"), each=4))

# The gl() function can also be used to generate a factor. The basic syntax is 
# gl(n, k, length, labels), where n is number of levels, k is number of 
# replications, length is length of result, and labels is a character vector of
# options factor level names.

# 3 levels repeated twice
gl(n = 3, k = 2)

# 3 levels repeated twice until filling a vector of size 20
gl(n = 3, k = 2, length=20)

# levels repeated once each until filling a vector of size 20
gl(n = 3, k = 1, length=20)

# same as previous with labels
gl(n = 3, k = 1, length=20, labels=c("win","lose","draw"))



# Bootstrapping -----------------------------------------------------------


