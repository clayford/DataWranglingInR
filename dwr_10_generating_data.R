#' ---
#' title: "Data Wrangling in R: Generating/Simulating data"
#' author: "Clay Ford"
#' date: "Spring 2016"
#' output: html_document
#' ---


load("../data/datasets_L08.Rda")

# It is often desirable to generate fake data. Sometimes we just want data to 
# play around with. Other times we want to generate data similar to what we 
# expect to collect to see if our proposed analysis works as expected. Or we 
# want to simulate data collection many times over in order to estimate a 
# statistical measure such as standard error. 


# sampling data -----------------------------------------------------------

# An easy way to generate data is to sample from existing data. The sample 
# function makes this possible. The syntax is sample(x, size, replace) where x 
# is either a vector of one or more elements from which to choose, or a positive
# integer, size is a non-negative integer giving the number of items to choose, 
# and replace is a logical setting about whether sampling should be with
# replacement (The default is FALSE).

# The most basic use is to generate a random permutation of the numbers 1:n:
sample(5) # sample without replacement

# or generate a random permutation of a vector:
dat <- c(10,12,18,16,18,9)
sample(dat)

# bootstrap resampling: sampling the same number of items WITH replacement
sample(dat, replace = TRUE) 


# Using set.seed() allows us to reproduce the same random sample. Just give it a
# whole number, any number.
set.seed(2)
sample(10)
set.seed(2)
sample(10)

# The size argument allows to select a certain number of elements from a vector.
# For example, sample 10 states:
sample(state.abb, size = 10)

# Using 1:6 and size=1, we can simulate the roll of a die:
sample(1:6, size=1)

# We can simulate the roll of a die 100 times by setting size=100 and
# replace=TRUE
sample(1:6, size=100, replace=TRUE)

# sample produces a vector, so we can manipulate it as we would any other 
# vector. For example, simulate a 100 die rolls and tally up the totals using
# table() and prop.table():
prop.table(table(sample(1:6, size=100, replace=TRUE)))

# using the forward-pipe operator: %>% 
library(magrittr)
sample(1:6, size=100, replace=TRUE) %>% table() %>% prop.table()

# Or simulate rolling two dice and summing the total:
sum(sample(1:6, size=2, replace=TRUE))
# same thing with %>% 
sample(6, size=2, replace=TRUE) %>% sum()

# We can use the replicate() function to replicate samples. The replicate() 
# function allows you to replicate an expression as many times as you specify. 
# The basix syntax is replicate(n, expr) where n is the number of replications 
# and expr is the expression you want to replicate.

# Roll 2 dice and keep the largest number, 10,000 times:
rolls <- replicate(n=1e5, expr = max(sample(1:6, size=2, replace=TRUE)))
# calculate proportions:
prop.table(table(rolls))
barplot(table(rolls))
rm(rolls)


# The sample function also has a prob argument that allows you to assign 
# probabilities to your items. For example to simulate the flip of a loaded
# coin, with Tails having probability 0.65:
flips <- sample(c("H","T"), 1000, replace=TRUE, prob = c(0.35,0.65))
prop.table(table(flips))
rm(flips)

# Coins are nice, but we can also use sample to generate practical data, for 
# example males and females. The following web site says UVa has 11,632 female
# students and 10,353 male students, as of Fall 2015:

# https://avillage.web.virginia.edu/iaas/instreports/studat/hist/enroll/school_by_gender.shtm
uva <- c(11632, 10353) # female, male
round(uva/sum(uva),2)

# We can generate a fake random sample of 500 UVa students with a weighted sampling
# scheme like so:
students <- sample(c("female","male"), 500, replace=TRUE, prob = c(0.53, 0.47))
prop.table(table(students))
rm(students, uva)

# When used with subsetting brackets, sample() can be used to create training 
# and test sets. For example, say we want to build some sort of predictive model
# using our training data. We may want to use half our data to build the model 
# and then use the other half to evaluate its performance.
train <- sample(nrow(weather), size= nrow(weather)/2)

# train is a random sample of numbers from 1 - 365. We can treat these like row 
# numbers.

weatherTrain <- weather[train,]
weatherTest <- weather[-train,]
# confirm no intersection
dplyr::intersect(weatherTrain, weatherTest) 


# generating fixed levels -------------------------------------------------

# Often generating data means creating a series of fixed levels, such as 10 
# males and 10 females. The rep() function can be useful for this. Below we
# replicate 10 each of "M" and "F":
rep(c("M","F"), each=10)

# we can also specify number of times the vector is replicated:
rep(c("M","F"), times=10)

# Finally we can replicate until a certain length is achieved
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

# A more common occurence is combinations of fixed levels, say gender, 
# education, and status. A function that helps create every combination of 
# levels is expand.grid(). Below we generate every combination of the levels 
# provided for gender, education, and status. Notice the first factors vary
# fastest.
expand.grid(gender=c("M","F"), 
            education=c("HS","College","Advanced"), 
            status=c("Single","Married","Divorced","Widowed"))

# Notice that creates a data frame that we can save:
DF <- expand.grid(gender=c("M","F"), 
            education=c("HS","College","Advanced"), 
            status=c("Single","Married","Divorced","Widowed"))
class(DF)
rm(DF)

# Extended example --------------------------------------------------------

# Create a experimental design plan and write out to a csv file.

# In this experiment, 3 people throw 3 different kinds of paper airplanes, made of 3
# paper types (3x3 = 9 planes), throwing each plane 8 times.

# > 3*3*3*8 
# [1] 216

schedule <- expand.grid(thrower=c("Clay","Rod","Kevin"),
            paper=c("18", "20", "24"),
            design=c("a","b","c"),
            rep=1:8)

# Randomize and drop the rep column. The sample(nrow(schedule)) code scrambles 
# the numbers 1 through 216, which I then use to randomly shuffle the schedule
# of throws.
k <- sample(nrow(schedule))
schedule <- schedule[k,1:3]
head(schedule, n = 10)

# output to csv file for logging "distance flown" data
write.csv(schedule, file="throwLog.csv", row.names=FALSE)


# generating numerical sequences ------------------------------------------

# The seq() function allows you to generate sequences of numbers:
seq(from = 0, to = 10, by = 2)
seq(0, 10, 0.2)
# go backwards:
seq(1000, 0, -100)

# The seq() function has a length.out argument that allows you to specify the 
# size of the vector you want to create. It automatically calculates the
# increment. We usually just abbreviate to length
seq(1, 10, length = 30)

# The colon operator(:) also allows you to generate regular sequences in steps
# of 1.
1:10
10:-10 # reverse direction

# When used with factors, the colon operator generates an interaction factor:
f1 <- gl(n = 2, k = 3); f1
f2 <- gl(n = 3, k = 2, labels = c("a","b","c")); f2
f1:f2 # a factor, the "cross"  f1 x f2
rm(f1, f2)

# Two related functions are seq_along() and seq_len(). seq_along() returns the
# indices of a vector while seq_len(n) returns an integer vector of 1:n.
seq_along(100:120)
seq_along(state.abb) # state.abb = built-in vector of state abbreviations

seq_len(10)


# generating random data from a probability distribution ------------------

# A central idea in inferential statistics is that the distribution of data can 
# often be approximated by a theoretical distribution. R provides functions for 
# working with several well-known theoretical distributions, including the 
# ability to generate data from those distributions. One we've used several 
# times in the lectures is the rnorm() function which generates data from a
# Normal distribution.

# In R, the functions for theoretical distributions take the form of dxxx, pxxx,
# qxxx and rxxx. 

# - dxxx is for the probability density/mass function (dnorm)
# - pxxx is for the cumulative distribution function (pnorm)
# - qxxx is for the quantile function (qnorm)
# - rxxx is for random variate generation (rnorm)

# For this lecture we're interested in the rxxx variety. See the lecture
# appendix for a review of the others. See help(Distributions) for all
# distributions available with base R.


# Draw random values from a theoretical distribution.
# 10 random draws from N(100,5)
rnorm(n = 10, mean = 100, sd = 5)

# 10 random draws from b(1,0.5)
# AKA, 10 coin flips
rbinom(n = 10, size = 1, prob = 0.5)

# 10 random draws from b(1,0.8)
# AKA, 10 coin flips with a coin loaded Heads (or Tails) 80% of time
rbinom(n = 10, size = 1, prob = 0.8)

# 10 random draws from b(10,0.5)
# AKA, 10 results of 10 coin flips
rbinom(n = 10, size = 10, prob = 0.5)

# We can use a binomial distribution to simulate dichotmous answers such as 
# Yes/No or success/fail. Simulate a vector of responses where respondents are
# 65% likely to say Yes (1) versus No (0)
rbinom(n = 10, size = 1, prob = 0.65)
# could also just use sample
sample(c("Y","N"), size = 10, replace = TRUE, prob = c(.65, .35))

# 10 random draws from a uniform distribution u(0,100)
runif(10,0,100)

# A uniform distribution can be good for random sampling. Let's say we want to
# sample about 10% of our SenateBills data:
k <- runif(nrow(SenateBills),0,1) # [0,1] interval is default
sbSamp <- SenateBills[k < 0.1, ] # sample about 10% of rows
dim(sbSamp)

# dplyr does this as well without the need for runif; and it's precise in its
# sampling fraction.
sbSamp <- dplyr::sample_frac(SenateBills, 0.1) # sample exactly 10% of rows
dim(sbSamp)
rm(sbSamp, k)

# The arguments to rxxx functions can take vectors! This means we can use one
# function call to generate draws from multiple distributions.

# alternating random values from N(10,4) and N(100,40)
rnorm(10, mean = c(10,100),sd = c(4,40))

# 30 random draws, 10 each from N(10,4), N(90,4) and N(400,4)
rnorm(30, mean = rep(c(10,90,400),each=10), sd = 4)

# 100 random draws, 50 each from b(5,0.5) and b(50,0.5)
rbinom(n = 100, size = rep(c(5,50),each=50), prob = 0.5)

# Combined with matrix(), one can generate "multiple" random samples from a 
# distribution. For example, draw 5 random samples of size 10 from a N(10,1):
matrix(rnorm(10*5,10,1),ncol=5)
# Technically we drew one sample of size 50 and then laid it out in a 10x5
# matrix.

# Using ifelse() we can generate different data based on a TRUE/FALSE condition.
# Let's say we have treated and untreated subjects. I'd like to generate Normal
# data that differs based on the treatment.
trtmt <- sample(c("Treated","Untreated"), size = 20, replace = TRUE)
ifelse(trtmt=="Treated", yes = rnorm(20, 10, 1), no = rnorm(20, 20, 1))

# Notice we have to make the length of the yes/no arguments the SAME LENGTH as
# the trtmt=="Treated" logical vector! What happens if we use rnorm(n=1,...)?

# What about more than two groups?
n <- 200
trtmt <- sample(LETTERS[1:6], size = n, replace = TRUE)

# Say we want to generate differnt Normal data for each group. One way is to do
# a for-loop with multiple if statements:

val <- numeric(n) # empty vector
for(i in seq_along(trtmt)){
  if(trtmt[i]=="A") val[i] <- rnorm(1, 10, 2)
  else if(trtmt[i]=="B") val[i] <- rnorm(1, 20, 4) 
  else if(trtmt[i]=="C") val[i] <- rnorm(1, 30, 6) 
  else if(trtmt[i]=="D") val[i] <- rnorm(1, 40, 8) 
  else if(trtmt[i]=="E") val[i] <- rnorm(1, 50, 10) 
  else val[i] <- rnorm(1, 60, 12) 
}
val

# A more R-like way would be to take advantage of vectorized functions. First 
# create a data frame with one row for each group and the mean and standard
# deviations we want to use to generate the data for that group.
dat <- data.frame(g=LETTERS[1:6],mean=seq(10,60,10),sd=seq(2,12,2))

# Now sample the row numbers (1 - 6) WITH replacement. We can use these to 
# randomly sample the data frame rows. Recall that we can repeatedly call a row
# or element using subsetting brackets. For example, call the first row of 
# allStocks 10 times:
allStocks[c(1,1,1,1,1),]

# Let's exploit that to randomly sample with replacement our data frame of
# groups:
k <- sample(1:6, n, replace = TRUE)
dat <- dat[k,]

# Now generate our data for each group using ONE call to rnorm.
dat$vals <- rnorm(n, mean=dat$mean, sd=dat$sd)
head(dat)


# A demonstration of the Central Limit Theorem ----------------------------

# The Central Limit Theorem states that the sum of a large number of independent
# random variables will be approximately normally distributed almost regardless 
# of their individual distributions. We can demonstrate this using various rxxx
# functions.

# sum 6 values from 6 different distributions (sample size = 6)
n <- 1e4 # simulate 1000 times
clt  <- rexp(n, rate = 1) + rbinom(n,10,0.4) + rchisq(n,df = 6) + 
  rnorm(n, 12, 12) + rpois(n, lambda = 3) + rt(n, df = 7)
hist(clt, freq=FALSE)

# overlay a normal density curve
X <- seq(min(clt),max(clt),length = 500)       # x
Y <- dnorm(X, mean = mean(clt), sd = sd(clt))  # f(x) = dnorm
lines(X,Y,type = "l", col="blue") # plot (x,y) coordinates as a "blue" line ("l")

rm(X, Y, clt)


# Estimating Power and Sample Size ----------------------------------------


# A practical reason to generate data is to estimate statistical power or an 
# appropriate sample size for an experiment. Power is the probability of 
# correctly rejecting the null hypothesis when it is actually false. If our
# sample is too small, we may fail to reject the null even it truly is false. 

# EXAMPLE: The two-sample t-test is used to determine if two population means 
# are equal. The null is the means are the same. An appropriate sample size is 
# one that is no bigger than it needs to be. An appropriate sample size for such
# a test depends on...

# - the hypothesized difference between the means (effect size)
# - the standard deviation of the populations
# - the significance level of our test
# - our desired power (usually 0.8)

# There is a function in R that allows you to calculate power and sample size
# for a t-test:

# calculate power for n=20 in each group, SD=1, sig level=0.05 and difference of
# means assumed to be 1 (delta):
power.t.test(n = 20, delta = 1, sd = 1, sig.level = 0.05)

# calculate sample size for power=0.80, SD=1, sig level=0.05 and difference of 
# means assumed to be 1 (delta):
power.t.test(power = 0.80, delta = 1, sd = 1, sig.level = 0.05)
# Always round n to next largest integer


# Now let's do a t-test with some simulated data to estimate power via 
# simulation. Below we simulate 20 observations from two normal distributions, 
# one with mean 5, and the other with mean 6. We then run a t-test to test the 
# null hypothesis that both samples come from the same normal distribution
# against the alternative hypothesis that they do not.

tout <- t.test(rnorm(20,5,1), rnorm(20,6,1))
tout

# note the structure of tout; it's a list:
str(tout)
# pull out just the p-value
tout$p.value

# We can do all that in one shot:
t.test(rnorm(20,5,1), rnorm(20,6,1))$p.value

# Let's run 1000 such t-tests using the replicate function:
out <- replicate(1000, t.test(rnorm(20,5,1), rnorm(20,6,1))$p.value)
# Estimate "power": proportion of times we rejected null of equal means
mean(out < 0.05)

# This agrees with the results from power.t.test:
power.t.test(n = 20, delta = 1, sd = 1, sig.level = 0.05)

# We can also use simulation of two-sample t tests to evaluate various sample 
# sizes. Let's create a function that simulates data in two groups and outputs
# the p-value of a t-test.

genTTest <- function(size){
  g1 <- rnorm(size,5,1)
  g2 <- rnorm(size,6,1)
  t.test(g1,g2)$p.value
}

genTTest(size=10)

# Now let's replicate our function 500 times to see how often we get a
# p-value less than 0.05 when n = 10
r.out <- replicate(n = 500, genTTest(size=10))
mean(r.out < 0.05) # our estimate of Power

# We could actually incorporate the above steps into a single function to save
# steps.

# Define a function called tPower that runs N=1000 t-tests and outputs power 
# given "size" (sample size in each group), for the genTTest function:
tPower <- function(size){
  out <- replicate(1000, genTTest(size))
  mean(out < 0.05)
}

# Estimated power with n=10 (10 in each group) for N=1000 t-tests
tPower(size=10)

# Now run the tPower function for increasing levels of sample size (10 - 30)
n <- 10:30
p.est <- sapply(n,tPower) # this may take a moment
plot(n, p.est, type="b", ylab="Power")
abline(h=0.8) # add line for 80% power

# The smallest value of n that saw over 80% of p-values below 0.05
min(n[p.est > 0.8])

# Again this should agree with what power.t.test() tells us:
power.t.test(delta = 1, sig.level = 0.05, power = 0.80)


# The nice thing about simulation and the R programming language is that we can 
# simulate data and results that are not covered by the many assumptions of the 
# usual power calculations. power.t.test() assumes equal group sizes and a 
# common standard deviation. Using simulation, we can approximate power and 
# sample size for a t.test between groups with unequal sample size and different
# standard deviations.

# Let's say we'll sample two groups with a 2:1 ratio and we suspect one group is
# more variable. We'd like to be able to detect a difference as small as 2
# between the two groups.
out <- replicate(1000, t.test(rnorm(20,5,5), rnorm(40,3,2))$p.value)

# Estimate "power": proportion of times we rejected null of equal means
mean(out < 0.05)

# Not great. What if we set n1=35 and n2=70?
mean(replicate(1000, t.test(rnorm(35,5,5), rnorm(70,3,2))$p.value) < 0.05)

# What if we're willing to assume the first group has SD of 3 instead of 5?
mean(replicate(1000, t.test(rnorm(35,5,3), rnorm(70,3,2))$p.value) < 0.05)


# We can pretty much do this for any statistical test or model, it just gets a
# little more complicated. 

# Let's say we intend to collect data on mothers and their babies and we hope to
# show that gestation time, age of the mother, and weight of the mother are all 
# significant contributors to a baby's birth weight (in ounces). Once this data
# is collected we might wish to perform a multiple regression where we model a
# baby's birth weight as a linear combination (or weighted sum) of gestation
# time, age of the mother, and weight of the mother. How many mothers and their
# babies do we need to observe?

# Note: the idea for this example comes from the "babies" data set included with
# the UsingR package.

# Let's first simulate some data:
n <- 50
gestation <- round(runif(n,240,300)) # gestation time in days
age <- round(rnorm(n,26,5)) # age of mother in years
mwt <- round(rnorm(n,138,20)) # mother's weight in lbs

# These are imperfect simulations. It could result in simulated mother who's 14,
# weighs 100, with a gestation time of 300 days. (Unlikely) With more effort and
# subject expertise we could probably come up with a more realistic way to
# simulate data, but for now this will do.

# Now using our mothers, let's simulate birth weights in ounces. These will be
# based on a weighted sum of our mothers' data and random noise (ie, factors
# that we have not accounted for that contribute to a baby's birth weight.)
# That's one of the assumptions of a linear model. 

# Let's say we want to detect the following if it's true:
# - every day of gestation leads to an additional 0.5 ounces of birth weight. 
# - each additional year of mother's age adds 0.1 ounces to birth weight.
# - each additional pound of the mother's weight adds 0.1 ounces to birth weight.

# These are the weights in our linear combination, or the coefficients in our 
# model. We add random noise from a normal distribution with mean 0 and a 
# standard deviation of 15. This is another assumption of a classic linear
# model: the errors are normally distributed with mean 0 and a constant
# variance.

bwt <- 0.5*gestation + 0.1*age + 0.1*mwt + rnorm(n,0,15)

# Now let's regress bwt on gestation, age and mwt. This basically attempts to
# recover the parameters we used to simulate the data.
summary(lm(bwt ~ gestation + age + mwt))

# The coefficient estimates in the output are the estimated weights in our 
# linear model. Likewise the residual standard error is the estimated standard
# deviation of our error distribution.

# Of interest is whether or not the coefficients are deemed significant. They 
# are definitely part of the data generation process, but how do they stand out 
# against the noise given various sample sizes?

# We can save the summary and extract the coefficient as matrix
sout <- summary(lm(bwt ~ gestation + age + mwt))
str(sout) # a list
sout$coefficients
# Extract the p-values
sout$coefficients[-1,4] # p-values
# What is less than 0.05?
sout$coefficients[-1,4] < 0.05

# We can combine everything into a function and replicate it to see how various 
# values of n affect the proportion of times we deem a coefficient significant
# (ie, statistically different from 0)

sim_lm <- function(n){
  gestation <- round(runif(n,240,300))
  age <- round(rnorm(n,26,5))
  mwt <- round(rnorm(n,138,20))
  bwt <- 0.5*gestation + 0.1*age + 0.1*mwt + rnorm(n,0,15)
  summary(lm(bwt ~ gestation + age + mwt))$coefficients[-1,4]
}

# a single simulation
sim_lm(n=30) < 0.05

# Replicate 1000 times
rout <- replicate(n = 1000, sim_lm(n=30))
# rout is a matrix with 3 rows and 1000 columns that stores the results of our
# 1000 simulations.
dim(rout)
rout[,1:5]
# proportion of times predictor declared significant:
apply(rout,1,function(x)mean(x < 0.05))

# With n = 30, we're definitely detecting the gestation effect, but not so much
# the age and weight.

# What about n = 100?
rout <- replicate(n = 1000, sim_lm(n=100))
apply(rout,1,function(x)mean(x < 0.05))

# What about n = 1000?
rout <- replicate(n = 1000, sim_lm(n=1000))
apply(rout,1,function(x)mean(x < 0.05))

# Beware: everything becomes significant if your sample size is made large 
# enough since standard error and hence p-values are functions of sample size.

# What we might want to do is standardize the predictor variables so they're all
# on the same scale:
sim_lm <- function(n){
  gestation <- round(runif(n,240,300))
  age <- round(rnorm(n,26,5))
  mwt <- round(rnorm(n,138,20))
  bwt <- 0.5*gestation + 0.1*age + 0.1*mwt + rnorm(n,0,5)
  summary(lm(bwt ~ scale(gestation) + scale(age) + scale(mwt)))$coefficients[-1,4]
}

rout <- replicate(n = 1000, sim_lm(n=100))
apply(rout,1,function(x)mean(x < 0.05))

# Generating multi-level data ---------------------------------------------

# Let's say we're interested in the effect of a new curriculum on the 
# improvement of a particular standardized test. We would probably select some 
# schools, then select teachers in the school, and then randomly assign to each 
# teacher a new curriculum to use (versus the standard approach). We would 
# pre-test the students, apply the two curriculums, and then test again to 
# measure their improvement. In this design, we have teachers nested in schools,
# and students nested in teachers. Hence, the name multi-level. It's probable 
# some schools and teachers are better than others, and hence may contribute to 
# improvement in test scores. We'd like to control for that variability as well 
# as measure the effect of curriculum. A multi-level model (or linear mixed 
# effect model) allows you to do this.

# It may be of interest to generate some data to better understand how we might 
# want to analyze it. This means we'll want a data set with one record per
# student, per teacher, per school.

# First let's generate school, teacher and student ids

# 10 schools
school <- 1:10

# generate number of teachers at each school
set.seed(1)
teacher <- rbinom(n = 10, size = 6, prob = 0.8)
teacher # 5 teachers at school 1, 5 teachers at school 2...
sum(teacher) # 46 teachers

# generate number of students in each class
set.seed(1)
student <- rbinom(n = sum(teacher), size = 25, prob = 0.8)
length(student) # 46; matches number of teachers
sum(student) # 911 students

school[1]; teacher[1]; student[1]
# school 1 has 5 teachers; teacher 1 at school 1 has 21 students.

# generate school ids; one value for each student; a little tricky! First
# generate sid for each teacher, then generate sid for each student.
sid <- rep(school, times = teacher) # length = 46
sid <- rep(sid, times = student) # length = 911
length(sid) # 911


# generate teacher ids; teachers nested in schools. 
school[1]; teacher[1]

# For example generate teacher IDs at school 1:
seq(teacher[1])

# apply seq function to each value in the teacher vector:
tid <- unlist(sapply(teacher, seq))
length(tid) # 46
tid

# now repeat these ids as many times as each teacher has students
tid <- rep(tid, student)
length(tid) # 911

# now generate stundent ids, students nested in teachers. Don't really need
# these, but we'll generate these anyway.
student[1] # teacher 1 at school 1 has 21 students
student[2] # teacher 2 at school 1 has 21 students
stid <- unlist(sapply(student, seq))
length(stid)

# let's put in a data frame and see what we got:
sDat <- data.frame(sid = factor(sid), tid = factor(tid), stid = factor(stid))
head(sDat, n=25)


# assign treatments; randomly select teachers
set.seed(1)
trt <- sample(0:1, size = sum(teacher), replace = TRUE)

# need to repeat treatment for each teacher at student length
sDat$trt <- rep(trt, student)
length(sDat$trt) # 911
table(sDat$trt)

# Now let's generate a change in score that is better for students with 
# treatment 1. Let's say the new - old score is about 15 points better for
# trt==1, while the new - old score is about 10 points better for trt==0.

# Tempting to use ifelse like this, but that doesn't do what you
# want!
# ifelse(sDat$trt==1, rnorm(1, 15, 5), rnorm(1, 10, 5))

# To use ifelse() you have to define the yes and no arguments to be the same 
# length as the condition.
length(sDat$trt==1) == 911

# So our yes/no arguments need to be length 911 as well.
set.seed(1)
sDat$diff <- ifelse(sDat$trt==1, 
                    rnorm(nrow(sDat), 15, 5), 
                    rnorm(nrow(sDat), 10, 5))

head(sDat, n = 20)
aggregate(diff ~ trt, data=sDat, mean)
aggregate(diff ~ sid + trt, data=sDat, mean)

# Right now the only source of variation is due to treatment. What if we want to
# incorporate variation due to teacher or school? Let's think of deriving diff
# as a formula:

# diff = 10 + trt*5 + error

# The error can be due to student, teacher and/or school.

# school error (10 schools)
set.seed(1)
s_err <- rnorm(10, 0, 2)
s_err <- rep(s_err, times = teacher) # length = 46
s_err <- rep(s_err, times = student) # length = 911

# teacher error (46 teachers)
set.seed(1)
t_err <- rnorm(46, 0, 3)
t_err <- rep(t_err, times = student)

# student error (911 students)
set.seed(1)
st_err <- rnorm(911, 0, 2)

# derive diff (save as diff2 so we keep our original diff)
sDat$diff2 <- 10 + 5*sDat$trt + s_err + t_err + st_err

head(sDat, n = 20)
aggregate(diff2 ~ trt, data=sDat, mean)
aggregate(diff2 ~ sid + tid + trt, data=sDat, mean)

# Fit a mixed-effect model with trt as the fixed effect and three sources of
# variation: school, teacher within school, and student
library(lme4)
lme1 <- lmer(diff2 ~ trt + (1 | sid/tid), data=sDat)
summary(lme1, corr = FALSE)

# Again, this basically attempts to recover the parameters we used to simulate
# the data.

# Bootstrapping/Resampling ------------------------------------------------

# Bootstrapping means treating our sample like a surrogate population, 
# resampling from it many times over, and calculating a statistic of interest 
# each time. We can then use these simulated statistics to make some inferences
# about the uncertainty of our original estimate.

# EXAMPLE 1: estimate the median Temperation in Charlottesville in February.
hist(weather$Mean.TemperatureF[weather$Month=="Feb"])
temps <- weather$Mean.TemperatureF[weather$Month=="Feb"]
median(temps)

# The standard error gives some indication of how uncertain my estimate is. 
# There is a formula for the standard error of the median, but it can be wrong
# for extremely non-normal distributions. Let's use the bootstrap instead.

# The basic idea is to resample my data with replacement, calculate the median,
# store it, and repeat many times, usually about 1000.

# Doing it once
median(sample(temps, replace = TRUE))

# Doing it 999 times and storing
bmed <- replicate(n = 999, median(sample(temps, replace = TRUE)))

# Calculating the standard deviation of our bootstrapped medians provides an
# estimate of the standard error.
sd(bmed)
hist(bmed)

# The histogram of my bootstrapped medians is not symmetric, so I may prefer a 
# percentile confidence interval instead of the usual "add/substract 2 SEs to my
# estimate".
quantile(bmed, probs = c(0.025, 0.975))

# EXAMPLE 2: correlation of Temperature and Dew Point
plot(MeanDew.PointF ~ Mean.TemperatureF, data=weather)
with(weather, cor(Mean.TemperatureF, MeanDew.PointF, use = "complete.obs"))

# Let's use the bootstrap to estimate the standard error of the correlation:

# First we subset our data:
corrData <- weather[,c("Mean.TemperatureF", "MeanDew.PointF")]

# write function to resample data and calculate correlation. Notice you can
# write a function without arguments.
cor.fun <- function(){
  k <- sample(nrow(corrData), replace = TRUE)
  cor(corrData[k,1], corrData[k,2], use = "complete.obs")
}

# Try it out one time
cor.fun()

# Replicate the function 999 times and save
bcorr <- replicate(n = 999, cor.fun())
sd(bcorr) # estimate of standard error
hist(bcorr)

# The distribution is pretty symmetric so we could add/substract the SE to form a confidence interval:
cor.est <- with(weather, cor(Mean.TemperatureF, MeanDew.PointF, use = "complete.obs"))
cor.est + c(-2*sd(bcorr), 2*sd(bcorr))

# Almost identical to the percentile confidence interval
quantile(bcorr, probs = c(0.025, 0.975))

# For more information on Resampling Methods, see a previous workshop of mine:
# http://static.lib.virginia.edu/statlab/materials/workshops/ResamplingMethods.zip


# Time-permitting bonus material ------------------------------------------


# The wakefield package ---------------------------------------------------

# The wakefield package provides functions for generating random data sets.

# install.packages("wakefield")
library(wakefield)

# Let's look at a few of the functions.

# Generate Random Vector of animals
animal(n = 20) # samples 20 of 10 different animals from wakefield's "animal_list"
animal(n = 20, k=20) # 20 levels

# pets
# pet(n, x = c("Dog", "Cat", "None", "Bird", "Horse"), 
#     prob = c(0.365, 0.304, 0.258, 0.031, 0.015), name = "Pet")
pet(n = 15)
pet(n = 15, x = c("Dog", "Cat", "Bird", "Lizard"),
    prob = c(0.4,0.3,0.2,0.1))

# Generate Random Vector of Educational Attainment Level
education(n = 15)
# The educational attainments and probabilities used match approximate U.S.
# educational attainment make-up

# Generate Random Vector of Control/Treatment Groups
group(10)

# Using the r_data_frame() function allows you to create a data frame of random
# values. From the r_data_frame examples:

r_data_frame(n = 30,
             id,             # these are all functions
             race,
             age,
             sex,
             hour,
             iq,
             height,
             died,
             Scoring = rnorm, # random normal data N(0,1)
             Smoker = valid   # Random Logical Vector using valid function
)

# Notice it returs a dplyr tbl_df.

rData <- r_data_frame(n = 30,
             id,
             race,
             age(x = 8:14),
             Gender = sex,
             Time = hour,
             iq,
             grade, grade, grade,  #repeated measures
             height(mean=50, sd = 10),
             died,
             Scoring = rnorm,
             Smoker = valid)

aggregate(Grade_1 ~ Gender, data=rData, mean)

# generate dummy coded variables
r_data_frame(n=100,
             id,
             age,
             r_dummy(sex, prefix=TRUE),
             r_dummy(political)
)

# Friendly vignette is available here:
# https://github.com/trinker/wakefield/blob/master/README.md


# A demonstration of familywise error rate --------------------------------

# In statistics, familywise error rate (FWER) is the probability of making one
# or more false discoveries, or type I errors, among all the hypotheses when
# performing multiple hypotheses tests.

n <- 1000; p <- 20 # 1000 observations, 20 variables

# generate all data from N(0,1)
set.seed(5)
dat <- as.data.frame(matrix(rnorm(n*p),ncol = p))
head(dat)

# Let's say V1 is our response (or dependent variable) and V2 - V20 are 
# predictors (or independent variables). Let's do multiple regression and
# regress V1 on V2 - V20. None of these predictors "explain" the variability in
# V1. All data is random normal N(0,1). Yet we will likely see some predictors
# declared as significant.

# lm(V1 ~ ., data=dat) regresses V1 on all other columns in dat. summary()
# returns the coefficients and associated t-tests for significance.

m1 <- lm(V1 ~ ., data=dat)
summary(m1)

# V7 appears to be significant, but in fact is not.

# we can the summary results
sout <- summary(m1)
typeof(sout)
# str(sout)
sout$coefficients
sout$coefficients[,4] # p-values

# any coefficients less than 0.05?
any(sout$coefficients[,4] < 0.05)

# how many coefficient less than 0.05?
sum(sout$coefficients[,4] < 0.05)

# We can write a function to generate data in this fashion and look for false 
# discoveries. We can then replicate it 500 times to get a feel for often it
# happens.
sdata <- function(n=1000, p=20){
  dat <- as.data.frame(matrix(rnorm(n*p),ncol = p))
  sout <- summary(lm(V1 ~ ., data=dat))
  c(FP=any(sout$coefficients[-1,4] < 0.05),
    HowMany=sum(sout$coefficients[-1,4] < 0.05))
}
sdata()

# Use replication to do it 500 times; it returns a matrix with 2 rows and 500
# columns, therefore we wrap it in t() to transpose it.
results <- t(replicate(n = 500, sdata()))
head(results)

# proportion of false positives
mean(results[,1])

# summary of false positive
summary(results[,2])

# Your results will differ from mine, but probably not by very much.



# Appendix ----------------------------------------------------------------


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
# instead of curves since we're dealing with discrete values. Here we graph the
# probability mass function for a binomial dist'n with n=10 and p=0.35.
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
# more than 4 "successes"
pbinom(q = 4, size = 10, prob = 0.35, lower.tail = F)


# qxxx - the quantile function
# This is basically the opposite of pxxx.
# This returns the point (or the quantile) for a given probability.

# Say we have a N(100,5); 
# In what "lower" quantile can we expect to see values 15% of the time:
qnorm(p = 0.15, mean = 100, sd = 5)
# In what "upper" quantile can we expect to see values 15% of the time:
qnorm(p = 0.15, mean = 100, sd = 5, lower.tail = FALSE)


# Say we have a b(10,0.35);
# How many successes can we expect to see 70% of the time:
qbinom(p = 0.7, size = 10, prob = 0.35)
# 4 or fewer
qbinom(p = 0.7, size = 10, prob = 0.35, lower.tail = FALSE)
# more than 3 

# qnorm can be helpful when shading in areas under curves
# Normal curve for N(100,5)
X <- seq(85,115,0.01)
Y <- dnorm(X, mean = 100, sd = 5)
plot(X,Y,type="l")
# quantile for p=0.15
q <- qnorm(p = 0.15, mean = 100, sd = 5)
# create vectors of x,y coordinates for polygon function;
# rev() reverses vectors: rev(1:3) = 3 2 1 
xx <- c(seq(85,q,length.out = 100),rev(seq(85,q,length = 100)))
yy <- c(rep(0,100),dnorm(rev(seq(85,q,length = 100)), mean = 100, sd = 5))
# use polygon to fill area under curve
polygon(x=xx,y=yy,col="grey")
# annotate graph
text(x = 93, y = 0.005,labels = pnorm(q,mean = 100,sd = 5))
text(x = q, y = 0.06, labels = round(q,2))

