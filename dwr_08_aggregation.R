#' ---
#' title: "Data Wrangling in R: Aggregation"
#' author: "Clay Ford"
#' date: "Spring 2015"
#' output: pdf_document
#' ---

load("../data/datasets_L07.Rda")

# Aggregation means collecting units in a group and performing an operation on 
# them such as taking the sum or mean. In many cases, THAT is the data analysis.
# That's what is referred to as descritive statistics. Data aggregation is also 
# used to create a data frame for inferential analysis or for preparing data for
# graphing.

# categorical data --------------------------------------------------------

# Aggregating categorical data usually means creating cross-tabulations, or 
# cross-tabs for short. R has a number of functions to help tabulate membership
# in categories.

# The basic function is table(). Simply give table() objects that can be 
# intertrepted as factors and it will construct a table. Below we generate two 
# vectors of factors and ask table to cross-classify them. Imagine the two 
# vectors stacked on top of one another and R counting up the number of
# combinations.

X <- sample(1:2,size = 500,replace=TRUE,prob = c(0.8,0.2))
Y <- sample(c("A","B"),size = 500,replace=TRUE, prob = c(0.4,0.6))
table(X,Y)

# tables can be saved:
tab <- table(X,Y)
tab
class(tab)

# calling summary on a table object produces the following:
summary(tab)

# The table() documentation in R has pretty good examples. Let's look at some of
# them.

## Simple frequency distribution 

# rpois(100, 5) generates 100 random values from a Poisson distribution with
# lambda=5. Values from a Poisson distribution are integers. We often use a
# Poisson distribition to model counts, such as number of 911 calls per day.
table(rpois(100, 5))

## Check the design:

# warpbreaks is data set that comes with R. It gives The Number of Breaks in
# Yarn during Weaving. There are two factors: wool and tension. Calling table()
# on wool and tension tells how many observations we have for each combination
# of the two factors. Hence the section header, ""Check the Design"
with(warpbreaks, table(wool, tension))
# It's balanced; equal number of observations at each combination of factor
# levels.

# state.division and state.region are two vectors of class "factor" that come 
# with R and part of the US State Facts and Figures dataset. State divisions are
# subsets of state regions. We can check that with table()
table(state.division, state.region)
# Checks out: No division appears in more than one region.

## simple two-way contingency table

# To create the following two-way contingency table we first "cut" a continuous 
# measure (Temperature) into categories. Each measure of Temperature is
# classified according to our definition of cut. Below we create 4 categories
# based on quantiles (0%, 25%, 50%, 75%, 100%)
with(airquality, table(cut(Temp, quantile(Temp)), Month))

## xtabs() <-> as.data.frame.table():

# This section header may look a little mysterious. It states that the xtabs() 
# function can calculate a contingency table based on output from the 
# as.data.frame.table() function, and vice versa. The example is a little terse
# so we'll expand on it.

# The data used is called UCBAdmissions (Student Admissions at UC Berkeley). It 
# contains aggregate data on applicants to graduate school at Berkeley for the 
# six largest departments in 1973 classified by admission and sex. It's already
# a contingency 3-way table:
UCBAdmissions 
class(UCBAdmissions)
# Feeding this table to as.data.frame() returns a data frame with a column for
# Frequencies that displays the number of each table cell.
as.data.frame(UCBAdmissions)
# Now let's save the data frame and give it to xtabs()
DF <- as.data.frame(UCBAdmissions)
xtabs(Freq ~ ., DF)

# The xtabs() function allows a formula interface where counts are provided on 
# the left of the "~" and factors on the right. The . means use all factors in
# the data frame, but we can speciy specific factors:
xtabs(Freq ~ Admit + Gender, DF)

# We'll return to xtabs() in a moment.

# This example shows you how table() can handle NA and Inf values. (Note: 1/0 =
# Inf). First we take the sequence NA, Inf, 1, 1/2, 1/3 and repeat it 10 times. 
a <- rep(c(NA, 1/0:3), 10) 
a
# Now tally the number of each value, which should be 10
table(a)

# Notice NA was not included. To include it, set exclude = NULL.
table(a, exclude = NULL)

# we can also use exclude to exclude certain factor levels. For example:
b <- factor(rep(c("A","B","C"), 10))
table(b)
table(b, exclude = "B")

# Here we generate a factor with 5 levels but with only 3 levels taking values:
d <- factor(rep(c("A","B","C"), 10), levels = c("A","B","C","D","E"))
d # no values of D or E

# we now build a table to tally values of d and exclude the level "B". 
table(d, exclude = "B")
# Notice levels D and E are in the table even though there are no occurrences of
# either.

# In this example we cross-classify b and d and use the zero.print argument to
# print "." instead of zeros
print(table(b, d), zero.print = ".")

## NA counting:

# The last two sections deal with counting NAs. First we take our vector d and
# add two NA values in the 3rd and 4th positions
is.na(d) <- 3:4
d
# Next we create a new vector called "d." The addNA() function modifies a factor
# by turning NA into an extra level
d. <- addNA(d)
d.[1:7]
table(d.) 
# Notice exclude = NULL is not needed since NA is a factor level.

# If we want to count the NA's of 'd', use exclude = NULL
table(d)
table(d, exclude = NULL)

# There is also a useNA argument that can take three values: "no", "ifany", or
# "always". The default is "no"
table(d)
table(d, useNA = "no")
table(d, useNA = "ifany")
table(b, useNA = "always") # show tally of NA even if there is none

## Two-way tables with NA counts. The 3rd variant is absurd, but shows
## something that cannot be done using exclude or useNA.

# The final section demonstrate two-way counts with NA values. First it creates 
# a logical vector of whether or not airquality$Ozone > 80. We can first tally
# the number of TRUE and FALSE.
table(airquality$Ozone > 80)
# any NAs?
table(airquality$Ozone > 80, useNA = "ifany") # yep, 37

# Let's create a two-way contingency table of this logical vector with Month:
with(airquality,
     table(OzHi = Ozone > 80, Month, useNA = "ifany"))
# No NA for Month, but NA for logical vector. 

# if we set useNA = "always", we'll get NA for both factors:
with(airquality,
     table(OzHi = Ozone > 80, Month, useNA = "always"))

# The documentation says this example is absurd. How so? Well, they leave useNA 
# set to its default "no", which means the NAs in the logical will not be 
# tallied. But then they use the addNA() function to add NA as a level to Month.
# That means NA becomes a factor level of month whether or not NAs are present, 
# and hence it gets included in the Month dimension of the table. I suppose this
# is "absurd" since Month is never missing.
with(airquality,
     table(OzHi = Ozone > 80, addNA(Month)))

# I encourage you to occassionally work through the examples provided with R 
# documentation. They will often expose you to functions or methods of data 
# manipulation that you were not aware of.


# We mentioned xtabs() as another way to create contigency tables. I find 
# table() a little more intuitive to use, but xtabs() is nice because it has the
# formula interface. For example, here's how we can use it to create a two-way
# table of Events and Cloud.Cover.Index from the weather data.
xtabs( ~ Events + Cloud.Cover.Index, data=weather)

# Notice there is nothing on the left of the "~". We do that when creating a 
# table of factors in a data frame for which there is no column of counts. That 
# tends to be how most data is formatted, and thus this seems to be the way
# xtabs() is most often used.

# A nice feature of the formula interface is the presence of the subset
# argument, which allows to specify subsetting conditions in xtabs().
xtabs( ~ Events + Cloud.Cover.Index, data=weather, 
       subset= Max.TemperatureF > 50,
       exclude=c("None","missing"))

# The prop.table() function creates tables with proportions instead of counts. 
# The basic syntax is prop.table(x,margin) where x is a table and margin is the 
# index (or indices) to generate marginal proportions. Let's go back to the
# UCBAdmissions data. Recall it was a 3-way table. We can access parts of the
# table using subsetting brackets. Notice it's a 2 x 2 x 6 table.
dim(UCBAdmissions)

# To access the first table for department A:
(deptA <- UCBAdmissions[,,1])

# Let's see how prop.table works on deptA (two-way table):
prop.table(deptA) # all sum to 1
prop.table(deptA, margin = 1) # rows sum to 1
prop.table(deptA, margin = 2) # columns sum to 1

# Let's see how prop.table works on a three-way table
(deptAB <- UCBAdmissions[,,1:2])

# Below all 2x4=8 cells sum to 1
prop.table(deptAB) 

# Below all 4 row entries for each level of Admit sum to 1
prop.table(deptAB, margin = 1) 
# For Admit:
0.5272915 + 0.09165808 + 0.3635427 + 0.01750772

# Below all 4 column entries for each level of Gender sum to 1
prop.table(deptAB, margin = 2) 
# For Male
0.3696751 + 0.2259928 + 0.2548736 + 0.1494585

# Below all entries in each 2x2 table sum to 1
prop.table(deptAB, margin = 3) 

# Below rows within each 2x2 table sum to 1
prop.table(deptAB, margin = c(1,3)) 

# Below columns within each 2x2 table sum to 1
prop.table(deptAB, margin = c(2,3)) 

# The addmargins() function does what you probably guess it does: it adds table
# margins.
addmargins(deptA)
# watch what it does on a 3-way table
addmargins(deptAB)
# sums both tables into a 3rd table, whose rows and columns are again summed!

# Like prop.table, addmargins() has a margin argument that you can use to
# specify which dimension is summed.
addmargins(deptA, margin = 1)

# Finally the ftable() function flattens tables to make them a little easier to read: 
ftable(UCBAdmissions)
# You can use the row.vars and col.vars arguments to change what is displayed in
# the rows and columns:
ftable(UCBAdmissions, row.vars=3)
ftable(UCBAdmissions, col.vars=c("Admit","Dept"))

# Continuous data ---------------------------------------------------------

# Aggregating continuous data means doing things like finding the mean score per
# group, the median income per state, and so on. We split data into groups, 
# apply a statistical calculation to each group, and combine back together in
# data frame or vector.

# The bread and butter aggregation function in base R is aggregate(). It works 
# with a formula interface. It can work with a formular interface and will
# return a data frame. Let's see some examples using our weather data:

# Find the mean minimum temperature per Event
aggregate(Min.TemperatureF ~ Events, data=weather, mean)
# Find the median maximum temperature per Event
aggregate(Max.TemperatureF ~ Events, data=weather, median)

# We can specify two responses using the cbind() function. Below we request both
# means for Min and Max temperature per Event.
aggregate(cbind(Min.TemperatureF,Max.TemperatureF) ~ Events, data=weather, mean)

# With the formula interface we can specify multiple factors. Let's use the 
# mtcars data to demonstrate. Here we calculate mean mpg for each combination of
# number of gears and transmission  (0 = automatic, 1 = manual)
aggregate(mpg ~ gear + am, data=mtcars, mean)

# We can also specifiy multiple responses:
aggregate(cbind(mpg, drat) ~ gear + am, data=mtcars, mean)

# We can define our own functions with aggregate. Here we examine the chickwts
# data set. It has two columns: weight and feed (6-level factor)
str(chickwts)

# Say we want to calculate the mean and standard error:
aggregate(weight ~ feed, data=chickwts, mean)
aggregate(weight ~ feed, data=chickwts, function(x)sd(x)/sqrt(length(x)))

# we can save the results as a data frame and manipulate accordingly:
chickM <- aggregate(weight ~ feed, data=chickwts, mean)
chickSE <- aggregate(weight ~ feed, data=chickwts, function(x)sd(x)/sqrt(length(x)))
# create a new data frame, plot means and standard error bars:
chicks <- data.frame(feed=chickM$feed, mean=chickM$weight, SE=chickSE$weight)
chicks
# plot means and SE bars
stripchart(mean ~ feed, data=chicks, pch=19, vertical = T, ylim=c(100,400), las=1)
segments(x0 = 1:6, y0 = chicks$mean+chicks$SE,x1 = 1:6, y1 = chicks$mean-chicks$SE)

# Another function useful for aggregating continuous data is tapply(). The R 
# documentation describes tapply as a function that allows you to "Apply a 
# function to each cell of a ragged array, that is to each (non-empty) group of 
# values given by a unique combination of the levels of certain factors." What 
# does that mean?! Basically it means you can aggregate groups of different
# sizes, just as you can with aggregate. The main differences are there is no
# formula interface and it does not return a data frame.

# The basic syntax is tapply(X, INDEX, FUN), where X is a vector of values, 
# INDEX is a vector(s) of group labels for X, and FUN is a function. Going to
# back to our weather data again:

tapply(weather$Min.TemperatureF, weather$Events, mean)
# or little easier using with():
with(weather, tapply(Min.TemperatureF, Events, mean))

# The output is technically an "array", not a data frame.

# tapply is not limited to returning scalars (ie, single values). For example,
# see how tapply and aggregate differ in the way they return a range:
aggregate(Min.TemperatureF ~ Events, data=weather, range)
tapply(weather$Min.TemperatureF, weather$Events, range)

# tapply can take more than one value for the INDEX argument. They just need to
# be formatted as a list.
with(mtcars, tapply(mpg, list(gear, am), mean))

# Notice tapply returned an arrary (two-way table) with every possible 
# combination, even those for which no values of mpg exist. This differs from 
# aggregate() which returned a data frame only containing rows for those
# combinations with values of mpg.


