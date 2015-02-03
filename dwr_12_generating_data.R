#' ---
#' title: "Data Wrangling in R: Generating/Simulating data"
#' author: "Clay Ford"
#' date: "Spring 2015"
#' output: pdf_document
#' ---


# It is often desirable to generate fake data. Sometimes we just want data to 
# play around with. Other times we want to generate data similar to what we 
# expect to collect to see if our proposed analysis works as expected. Or we 
# want to simulate data collection many times over in order to estimate a 
# statistical measure such as standard error. R has excellent facilities for
# generating random data.


# generating fixed levels -------------------------------------------------

# Often generating data means creating a series of fixed levels, such as 100 
# males and 100 females. The rep() function can be useful for this. Below we
# create 100 each of "M" and "F":
rep(c("M","F"), each=100)
# Note this generated a character vector. To use as a "factor", we would need to
# wrap in the factor() function.

# Perhaps a better function for creating factors is the gl() function. gl = 
# "generate levels". Below we generate a factor with 2 levels of 100 each and
# labels of "M" and "F". Notice the result is a factor.
gl(n = 2, k = 100, labels = c("M","F"))

# A more common occurence is combinations of fixed levels, say gender, 
# treatment, and race. A function that helps create every combination of levels 
# is expand.grid(). Below we generate every combination the levels provided for
# gender, treatment and race:
expand.grid(gender=c("M","F"), 
            treatmen=c("Treat","Placebo"), 
            race=c("White","Black","Asian","Multiracial"))

# Create a experimental design plan and write out to csv file.

# In this experiment, 3 people throw 3 different kinds of paper airplanes, made of 3
# paper types (3x3 = 9 planes), throwing each plane 8 times.

# > 3*3*3*8 
# [1] 216

schedule <- expand.grid(thrower=c("Clay","John","Mark"),
            paper=c(15,20,22),
            design=c("a","b","c"),
            rep=1:8)
# randomize and drop rep:
schedule <- schedule[sample(nrow(schedule)),1:3]
# output to csv file for logging data
write.csv(schedule, file="throwLog.csv", row.names=FALSE)

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
x <- seq(10,15,length.out = 100)
y <- 10 + 5*x + rnorm(100,sd=4)
plot(x,y)
mod <- lm(y ~ x)
summary(mod)
abline(mod)


# Two-sample t tests compare the means of two normally distributed populations. 
# An appropriate sample size for such a test depends on the hypothesized 
# difference between the means, the standard deviation of the populations, and 
# the significance level of our test, and our desired power. Power is simply the
# probability of correctly rejecting the null hypothesis (no difference between 
# means) when it is actually false. There is a function in R that allows you to
# calculate power and sample size for a t-test:

# calculate power for n=20 in each group, an SD=1 and sig level=0.05
power.t.test(n = 20, delta = 1)
# calculate sample size for power=0.80, an SD=1 and sig level=0.05
power.t.test(power = 0.80, delta = 1)

# Now let's do a t-test with some sample data to estimate power via simulation:
tout <- t.test(rnorm(20,5,1),rnorm(20,6,1),alternative = "two.sided")
# note the structure of tout
str(tout)
# pull out just the p-value
tout$p.value

# We can run 1000 t-tests:
out <- replicate(1000, t.test(rnorm(20,5,1),rnorm(20,6,1),alternative = "two.sided")$p.value)
# count the number of p-values less than 0.05 and divide by 1000 to estimate "power"
mean(out < 0.05)


# We can also use simulation of two-sample t tests to find sample size

# define a function called tpower that runs a 1000 t-tests and outputs power
# given n (sample size in each group):
tpower <- function(n, N=1000){
  out <- replicate(N, t.test(rnorm(n,5,1),rnorm(n,6,1),alternative = "two.sided")$p.value)
  sum(out < 0.05)/1000
}

# Now run the tpower function for increasing levels of sample size
n <- 10:30
pest <- sapply(n,tpower) # this may take a moment
plot(n,pest, type="b")
abline(h=0.8) # add line for 80% power
# smallest value n such that power is > 0.80
n[pest>0.8][1]


# We can pretty much do this for any model, it just gets a little more complicated.

# Let's say we have a drug for some condition. We conduct an experiment on 100 
# people, 50 treated and 50 not-treated. We measure the difference in the 
# response before and after treatment to get our final response measure. We also
# want to control for a continuous covariate, age. This is a classic ANCOVA 
# (analysis of covariance). Say we believe the treatment increases the response 
# effect by 2 units and we don't want to miss that effect. Is our sample size
# sufficient if we assume a standard error of 1?

# Create a function to generate data
genData <- function(n,sd){
  treat <- rep(c(0,1),each=n)
  age <- round(runif(n*2,20,60))
  resp <- c(rnorm(n,10,sd),rnorm(n,12,sd))
  data.frame(resp,age, treat)
}
# test the function
genData(n=50, sd=1)

# now use the function in the linear model function and call summary();
# summary() produces a list with a matrix element called "coefficient"; the 3rd
# row, 4th column of that matrix contains the p-value of the treatment
# coeffient.
summary(lm(resp ~ age*treat, data=genData(n=50, sd=1)))$coef[3,4]

# now generate 1000 data sets and run the model 100 times
out <- replicate(1000,summary(lm(resp ~ age*treat, data=genData(n=50, sd=1)))$coef[3,4])
# percent of times significance achieved (ie, estimated power)
mean(out < 0.05)

# with our function allowing different n and sd, we can try different settings:
out <- replicate(1000,summary(lm(resp ~ age*treat, data=genData(n=50, sd=2)))$coef[3,4])
# estimate power assuming SD=2 and n=50
mean(out < 0.05)

out <- replicate(1000,summary(lm(resp ~ age*treat, data=genData(n=200, sd=2)))$coef[3,4])
# estimated power assuming SD=2 and n=200
mean(out < 0.05)

# we can also loop through various settings and graph the results. I change 1000
# to 200 below in the interest of time:
power <- numeric(length(seq(50,300,by=25)))
j <- 1
for(i in seq(50,300,by=25)){
  out <- replicate(200,summary(lm(resp ~ age*treat, data=genData(n=i, sd=2)))$coef[3,4])
  power[j] <- mean(out < 0.05)
  j <- j + 1
}
# graph the result
plot(seq(50,300,by=25), power,type="b", xlab="sample size", ylab="power", ylim=c(0,1))
abline(h=0.8, lty=2)

# Instead of a "for" loop we could do it the R way: write a function and then
# use sapply.
powerFun <- function(x){
  out <- replicate(200,summary(lm(resp ~ age*treat, data=genData(n=x, sd=2)))$coef[3,4])
  mean(out < 0.05)
} 
# Now apply the function seq(50,300,by=25)
power <- sapply(seq(50,300,by=25), powerFun)

# and again graph the result
plot(seq(50,300,by=25), power,type="b", xlab="sample size", ylab="power", ylim=c(0,1))
abline(h=0.8, lty=2)



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

# The sample function also has a prob argument that allows you to assign 
# unequal probabilities to your items. For example to simulate the flip of a
# loaded coin, with Tails having probability 0.65:
sample(c("H","T"),100,replace=TRUE,prob = c(0.35,0.65))




# Bootstrapping -----------------------------------------------------------


# Bootstrapping means using our original data for resampling. Previously we 
# generated data from a theoretical distribution. Now we use our original 
# sample. The main reason for doing this is to estimate the standard error of 
# the sampling distribution. For a statistic such as the mean, we have an easy 
# formula for the standard error. For other statsitics, such as a ratio or 
# correlation, the formula for the standard error is more difficult and relies
# on possibly incorrect assumptions.

# The basic idea is to use your original sample as a surrogate for the 
# population and sample from it with replacement many times (say B times), 
# generating new samples the same size as the original. We call these bootstrap 
# samples. For each bootstrap sample we calculate the statistic of interest. 
# When done we calculate the standard deviation of the B statistics to estimate
# the standard error of the sampling distribution.

# Recall our tree data:
trees <- read.csv("139_treecores_rings.txt")

# The ratio of the mean diameter at breast height to the mean diameter at core
# height is easily calculated:
mean(trees$DBH.cm./trees$DCH.cm.)

# But how accurate is this estimate? That's what the standard error tells us. 
# Instead of googling the formula for the standard error of the ratio of means,
# we could bootstrap our original sample (assuming it is random).

ratio <- function(){
  i <- sample(nrow(trees),nrow(trees),replace=T)
  mean(trees$DBH.cm.[i]/trees$DCH.cm.[i])  
}
ratio()
# run 1000 times
bout <- replicate(1000,ratio())
sd(bout)

# Bootstrapping with the boot package
bootRatio <- function(dat,i){
  mean(dat$DBH.cm.[i]/dat$DCH.cm.[i])  
}
boot(trees, bootRatio, 999)
bout2 <- boot(trees, bootRatio, 999)
boot.ci(bout2, type = "perc")


# mapply can also be used to apply varying argument levels to function.
# Generate random normal samples of sizes ranging from 5:50
outn <- mapply(rnorm, n=5:50, mean=10, sd=5)
# then use sapply to find the standard error
se <- sapply(outn, function(x)sd(x)/sqrt(length(x)))
# and plot standard error versus sample size to see the decreasing trend.
plot(5:50,se,xlab="N",ylab="SE")

# I'm not sure what this is useful for, but I like it.
mapply(seq, from=1:10, to=11:20)
