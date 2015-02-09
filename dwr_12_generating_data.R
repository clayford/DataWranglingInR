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
# statistical measure such as standard error. 


# generating fixed levels -------------------------------------------------

# Often generating data means creating a series of fixed levels, such as 10 
# males and 10 females. The rep() function can be useful for this. Below we
# replicate 10 each of "M" and "F":
rep(c("M","F"), each=10)

# we can also specify number of times the vector is replicated:
rep(c("M","F"), times=10)

# finally we can replicate until a certain length is achieved
rep(c("M","F"), length.out = 15)
# or just length, for short
rep(c("M","F"), length = 15)


# Notice that all these generated a character vector. To use as a "factor", we
# would need to wrap it in the factor() function.
factor(rep(c("M","F"), each=10))

# A function specifically for creating factors is the gl() function. gl = 
# "generate levels". Below we generate a factor with 2 levels of 10 each and 
# labels of "M" and "F". Notice the result is a factor.
gl(n = 2, k = 10, labels = c("M","F"))

# Recall that for categorical data, factors are more efficient for memory than
# character.

# names of US states replicated 10,000 times, stored as character:
chr1 <- rep(state.name, times=1e5)
format(object.size(chr1), units="Mb")
# names of US states replicated 10,000 times, stored as factor:
chr2 <- factor(rep(state.name, times=1e5))
format(object.size(chr2), units="Mb")

rm(chr1, chr2)

# A more common occurence is combinations of fixed levels, say gender, 
# education, and status. A function that helps create every combination of 
# levels is expand.grid(). Below we generate every combination of the levels
# provided for gender, education, and status:
expand.grid(gender=c("M","F"), 
            education=c("HS","College","Advanced"), 
            status=c("Single","Married","Divorced","Widowed"))

# Notice that creates a data frame that we can save:
DF <- expand.grid(gender=c("M","F"), 
            education=c("HS","College","Advanced"), 
            status=c("Single","Married","Divorced","Widowed"))
str(DF) # factors automatically created
class(DF)

# Create a experimental design plan and write out to csv file.

# In this experiment, 3 people throw 3 different kinds of paper airplanes, made of 3
# paper types (3x3 = 9 planes), throwing each plane 8 times.

# > 3*3*3*8 
# [1] 216

schedule <- expand.grid(thrower=c("Clay","Rod","Frank"),
            paper=c(18, 20, 24),
            design=c("a","b","c"),
            rep=1:8)

# Randomize and drop the rep column. The sample(nrow(schedule)) code scrambles 
# the numbers 1 through 216, which I then use to randomly shuffle the schedule
# of throws.
schedule <- schedule[sample(nrow(schedule)),1:3]
# output to csv file for logging "distance flown" data
write.csv(schedule, file="throwLog.csv", row.names=FALSE)


# generating numerical sequences ------------------------------------------

# The seq() function allows you to generate sequences of numbers:
seq(from = 1, to = 10, by = 2)
seq(1, 10, 0.2)

# The seq() function has a length.out argument that allows you to specify the 
# size of the vector you want to create. It automatically calculates the
# increment.
seq(1, 10, length.out = 30)

# The colon operator(:) also allows you to generate regular sequences in steps
# of 1.
1:10
10:-10

# When used with factors, the colon operator generates interactions:
f1 <- gl(n = 2, k = 3); f1
f2 <- gl(n = 3, k = 2); f2
f1:f2 # a factor, the "cross"  f1 x f2

# Two related functions are seq_along() and seq_len(). seq_along() returns the
# indices of a vector while seq_len(n) returns an integer vector of 1:n.
seq_along(100:120)
seq_len(10)
 

# generating random data from a probability distribution ------------------

# A central idea in inferential statistics is that the distribution of data can 
# often be approximated by a theoretical distribution. R provides functions for 
# working with several well-known theoretical distributions, including the 
# ability to generate data from those distributions. One we've used several 
# times in the lectures is the rnorm() function which generates data from a
# Normal distribution.

# Example: generate data from a Normally distributed pop'n with a mean of 150
# and a standard deviation of 10.
ex <- rnorm(n = 100, mean = 150, sd = 10)
mean(ex); sd(ex)

# In R, the functions for theoretical distributions take the form of dxxx, pxxx,
# qxxx and rxxx, respectively. dxxx is for the density/mass function, pxxx is 
# for the cumulative distribution function, qxxx is for the quantile function, 
# and rxxx is for random variate generation. For this lecture we're mostly
# interested in the rxxx variety, but let's briefly review the others. NOTE: see
# help(Distributions) for all distributions available with base R.

# dxxx - density/mass function
# This is basically the formula that draws the distribution.
# Here we use dnorm for the standard Normal distribution: N(0,1).
X <- seq(-3,3,0.01)
Y <- dnorm(X)
plot(X,Y,type="l")

# We can do the same with a chi-square distribution with 3 degrees of freedom.
X <- seq(0,10,0.01)
Y <- dchisq(X,df = 3)
plot(X,Y,type="l")

# For discrete distributions such as the Binomial, you usually draw histograms
# instead of curves since we're dealing with discrete values.
X <- seq(0,10)
Y <- dbinom(X,size = 10,prob = 0.35)
plot(X,Y,type="h")

# pxxx - cumulative distribution function 
# This is basically the probability of a value being less than (or equal to) a 
# certain point in the theoretical distribution.

# Say we have a N(100,5); 
# Probability of drawing a value less than 95:
pnorm(q = 95, mean = 100, sd = 5)
# Probability of drawing a value greater than 95:
pnorm(q = 95, mean = 100, sd = 5, lower.tail = FALSE)
# or 
1 - pnorm(q = 95, mean = 100, sd = 5)

# Same idea with a discrete distribution, except we say less than or equal to. 
# Binomial distribution with 10 trials and probability of 0.35: b(10,0.35) 
# Probability of seeing 4 or fewer "successes" out of 10 trials:
pbinom(q = 4, size = 10, prob = 0.35)


# qxxx - the quantile function
# This is basically the opposite of pxxx.
# This returns the point (or the quantile) for a given probability.

# Say we have a N(100,5); 
# In what quantile can we expect to see values 15% of the time:
qnorm(p = 0.15, mean = 100, sd = 5)

# Say we have a b(10,0.35);
# How many successes can we expect to see 70% of the time:
qbinom(p = 0.7, size = 10, prob = 0.35)

# qnorm can be helpful when shading in areas under curves
# Normal curve for N(100,5)
X <- seq(85,115,0.01)
Y <- dnorm(X, mean = 100, sd = 5)
plot(X,Y,type="l")
# quantile for p=0.15
q <- qnorm(p = 0.15, mean = 100, sd = 5)
# create vectors of x,y coordinates for polygon function;
# rev() reverses vectors: rev(1:3) = 3 2 1 
xx <- c(seq(85,q,length.out = 100),rev(seq(85,q,length.out = 100)))
yy <- c(rep(0,100),dnorm(rev(seq(85,q,length.out = 100)), mean = 100, sd = 5))
# use polygon to fill area under curve
polygon(x=xx,y=yy,col="grey")
# annotate graph
text(x = 93, y = 0.005,labels = pnorm(q,mean = 100,sd = 5))
text(x = q, y = 0.06, labels = round(q,2))


# rxxx - random variate generation.
# Draw random values from a theoretical distribution.
# 10 random draws from N(100,5)
rnorm(n = 10, mean = 100, sd = 5)

# 10 random draws from b(1,0.5)
# AKA, 10 coin flips
rbinom(n = 10, size = 1, prob = 0.5)

# 10 random draws from b(10,0.5)
# AKA, 10 results of 10 coin flips
rbinom(n = 10, size = 10, prob = 0.5)

# A demonstration of the Central Limit Theorem. 

# The Central Limit Theorem states that the sum of a large number of independent
# random variables will be approximately normally distributed almost regardless
# of their individual distributions.

clt  <- rexp(n = 1e5, rate = 1) + rbinom(1e5,10,0.4) + rchisq(1e5,df = 6) + 
  rnorm(1e5,12,12) + rpois(1e5,lambda = 3) + rt(1e5,df = 7)
hist(clt, freq=FALSE)
X <- seq(min(clt),max(clt),length.out = 500)
Y <- dnorm(X, mean = mean(clt), sd = sd(clt))
lines(X,Y,type = "l")


# Generating data from population models ----------------------------------

# When doing things like linear modeling (regression) we're essentially trying 
# to determine the mathematical process that gave rise to the data. For example,
# in simple linear regression we postulate the data came from a model that 
# produces a straight line with random errors that come from a Normal
# distribution with mean 0 and some finite standard deviation.

# R allows us to easily create such models and use them to generate data.

# Example: Simple linear regression

# independent variable
x <- seq(10,15,length.out = 100)

# population parameters
intercept <- 10
slope <- 5
sigma <- 4

# dependent variable
y <- intercept + slope*x + rnorm(100,sd=sigma)
plot(x,y)

# Of course then simple linear regression should work well on this data and
# closely estimate our population parameters:
mod <- lm(y ~ x)
summary(mod)
abline(mod)


# Estimating Power --------------------------------------------------------


# Two-sample t tests compare the means of two normally distributed populations. 
# An appropriate sample size for such a test depends on the hypothesized 
# difference between the means, the standard deviation of the populations, the
# significance level of our test, and our desired power. Power is simply the 
# probability of correctly rejecting the null hypothesis (no difference between 
# means) when it is actually false. There is a function in R that allows you to 
# calculate power and sample size for a t-test:

# calculate power for n=20 in each group, an SD=1 and sig level=0.05
power.t.test(n = 20, delta = 1)
# calculate sample size for power=0.80, an SD=1 and sig level=0.05
power.t.test(power = 0.80, delta = 1)
# Always round n to next largest integer

# Now let's do a t-test with some sample data to estimate power via simulation:
tout <- t.test(rnorm(20,5,1),rnorm(20,6,1),alternative = "two.sided")
# note the structure of tout
str(tout)
# pull out just the p-value
tout$p.value

# We can do all that in one shot:
t.test(rnorm(20,5,1),rnorm(20,6,1),alternative = "two.sided")$p.value

# Let's run 1000 such t-tests using the replicate function. The replicate() 
# function allows to replicate an expression as many times as you specify. For 
# example, to replicate 100 times sampling 10 items from a N(100,5) distribution
# and taking the mean:
replicate(25, median(rnorm(10,100,5)))

# replciate t-tests:
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

# Estimated power with n = 5 
tpower(5)

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
head(genData(n=50, sd=1))

# now use the function in the linear model function and call summary();
# summary() produces a list with a matrix element called "coefficient"; the 3rd
# row, 4th column of that matrix contains the p-value of the treatment
# coeffient.
summary(lm(resp ~ age*treat, data=genData(n=50, sd=1)))$coef[3,4]

# now generate 1000 data sets and run the model 1000 times
out <- replicate(1000,summary(lm(resp ~ age*treat, data=genData(n=50, sd=1)))$coef[3,4])
# percent of times significance achieved (ie, estimated power)
mean(out < 0.05)

# with our function allowing different n and sd, we can try different settings:
# estimate power assuming SD=2 and n=50
out <- replicate(1000,summary(lm(resp ~ age*treat, data=genData(n=50, sd=2)))$coef[3,4])
mean(out < 0.05)

# estimated power assuming SD=2 and n=200
out <- replicate(1000,summary(lm(resp ~ age*treat, data=genData(n=200, sd=2)))$coef[3,4])
mean(out < 0.05)

# we can also loop through various settings and graph the results. I change 1000
# to 200 below in the interest of time:
power <- numeric(length(seq(50,300,by=25)))
j <- 1
# loop through sample sizes of 50 to 300 in increments of 25
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

# In statistics we usually can't measure every member of a population. The best
# we can do is take a sample and then use that sample as a surrogate for the 
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

# With replicate(), we can simulate the roll of a die 100 times. Here we
# replicate 100 rolls of a fair die.
replicate(n=100, sample(x=1:6, size=1))

# Using table, we can simulate a 1000 die rolls and tally up the totals
table(replicate(n=100,sample(1:6,1)))

# To simulate rolling two dice, or rolling a die two times, we need to set size
# = 2 and replace = TRUE. If we don't set replace = TRUE, then we would never
# roll a pair.
sample(x=1:6, size=2, replace=T)

# again we can use replicate to simulate rolling two dice a 100 times. The
# result is a matrix.
diceRolls <- replicate(n=100, sample(x=1:6, size=2, replace = TRUE))
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

# The sample() function is useful for creating test and training sets when it
# comes to modeling. 

# Recall our tree data:
trees <- read.csv("../data/139_treecores_rings.txt")
plot(DCH.cm. ~ DBH.cm., data=trees)

# It might be nice to only have to measure DBH since it's so highly correlated 
# with DCH. Let's say we're interested in developing a model for predicing DCH
# given DBH.

mod1 <- lm(DCH.cm. ~ DBH.cm., data=trees)
summary(mod1)
# residual standard error (RSE) gives you an idea of how well the model
# predicts.
summary(mod1)$sigma

# What if we remove our outlier?
mod2 <- lm(DCH.cm. ~ DBH.cm., data=trees, subset= DBH.cm.<100)
summary(mod2)$sigma

# That's one extremely influential observation! But our RSE is still overly 
# optimistic since it's tested with the same data used to build the model. So we
# can split the data into test and training sets. Build the model with the 
# training data, evaluate performance with the test data.

# we can use the sample function to randomly sample row numbers from the data
# frame to create training and test sets. First we remove the outlier.
trees2 <- subset(trees, DBH.cm.<100)

# Now we generate training data by simply generating random row numbers in the
# range of 1 to nrow(trees2)
train <- sample(nrow(trees2), size = nrow(trees2)%/%2)

# fit model with training data
modTrain <- lm(DCH.cm. ~ DBH.cm., data=trees2, subset=train)
# predict model with test data
p <- predict(modTrain, newdata = trees2[-train,])
# calculate RSE
sqrt(sum((trees2$DCH.cm.[-train] - p)^2)/summary(modTrain)$df[2])

# It's a little higher and perhaps a better estimate of true prediction error.

# We can also do something called cross validation that breaks data into, say, 
# 10 sets, fits the model with 9 and then tests with the one held out. And then 
# does this for each set and then averages the error. The cv.glm() function in 
# the boot package can do this for us. It just requires to fit a model with
# glm() instead of lm().

library(boot)
glm1 <- glm(DCH.cm. ~ DBH.cm., data=trees2)
sqrt(cv.glm(trees2, glm1, K=10)$delta[1])


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

# Let's again look at our tree data. The ratio of the mean diameter at breast
# height to the mean diameter at core height is easily calculated:
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




