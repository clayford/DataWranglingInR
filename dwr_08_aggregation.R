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
# used to create a data frame for inferential analysis, to prepare data for 
# graphing, or as an intermediate step for additional data wrangling.

# categorical data --------------------------------------------------------

# Aggregating categorical data usually means creating cross-tabulations, or 
# cross-tabs for short. R has a number of functions to help tabulate membership
# in categories.

# The basic function is table(). Simply give table() objects that can be 
# intertrepted as factors and it will construct a table. Below we generate two 
# vectors and ask table to cross-classify them. Imagine the two vectors stacked
# on top of one another and R counting up the number of combinations.

X <- sample(1:2,size = 500,replace=TRUE,prob = c(0.8,0.2))
Y <- sample(c("A","B"),size = 500,replace=TRUE, prob = c(0.4,0.6))
X[1:4]
Y[1:4]
table(X,Y)

# Notice X and Y are not factors:
class(X);class(Y)
# ...yet table coerced them to factors before tabulation. 

# tables can be saved:
tab <- table(X,Y)
tab
class(tab)

# calling summary on a table object produces the following:
summary(tab)

# more than meets the eye in summary(tab)
str(summary(tab))
# we see it's a list object. This means we can extract information from it, such
# as the p-value for the test of independence:
summary(tab)$p.value

# Tables can go beyond 2 dimensions. Let's add a third dimension:
Z <- sample(c("I","II"), size=500, replace=T)
table(X,Y,Z)
# The tables in the 3rd dimension are sort of like subsetting a data frame by
# the 3rd dimentsion's value and then creating a 2-way table:
tdf <- data.frame(X,Y,Z)
head(tdf)
with(tdf[tdf$Z=="II",], table(X,Y))

# The table() documentation in R has pretty good examples. Let's look at some of
# them. I encourage you to occasionally work through the examples provided with
# R documentation. They will often expose you to functions or methods of data 
# manipulation that you were not aware of.

## Simple frequency distribution 

# rpois(100, 5) generates 100 random values from a Poisson distribution with 
# lambda=5. Values from a Poisson distribution are integers. We often use a 
# Poisson distribition to model counts, such as number of 911 calls per hour. 
# The takeaway from this example is that table will coerce vectors of numbers
# into factors and calculate a frequency distribution.
pdata <- rpois(100, 5)
table(pdata)
# And this is handy for plotting
plot(table(pdata), type="h")
barplot(table(pdata))

## simple two-way contingency table

# To create the following two-way contingency table we first "cut" a continuous 
# measure (Temperature) into categories. Each measure of Temperature is 
# classified according to our definition of cut. Below we create 4 categories 
# based on quantiles (0%, 25%, 50%, 75%, 100%) and then create a contingency
# table with Month.
with(airquality, table(cut(Temp, quantile(Temp)), Month))

## xtabs() <-> as.data.frame.table():

# This section header may look a little mysterious. It states that the xtabs() 
# function can calculate a contingency table based on output from the 
# as.data.frame.table() function, and vice versa. The example is a little terse
# so we'll expand on it.

# The data used is called UCBAdmissions (Student Admissions at UC Berkeley). It 
# contains aggregate data on applicants to graduate school at Berkeley for the 
# six largest departments in 1973 classified by admission and sex. It's already
# a 3-way contingency table:
UCBAdmissions 
class(UCBAdmissions)
# Feeding this table to as.data.frame() returns a data frame with a column for 
# Frequencies that displays the number of each table cell. In other words, it 
# converts a table to a data.frame. It has the same information as the table,
# but stored as a data frame with the counts in the Freq column.
as.data.frame(UCBAdmissions)

# Now let's save the data frame and give it to xtabs()
DF <- as.data.frame(UCBAdmissions)
xtabs(Freq ~ ., DF) 
# We see that xtabs() reverses the effect of as.data.frame(). In other words,
# xtabs() allows you to take a data frame with counts and produce a table.

# The xtabs() function allows a formula interface where counts are provided on 
# the left of the "~" and factors on the right. The . means use all factors in 
# the data frame, but we can speciy specific factors. For example for a two-way
# table of admissions amd and gender over all departments:
xtabs(Freq ~ Admit + Gender, DF)

# We'll return to xtabs() in a moment.

## Including/Excluding levels

# The table() documentation has examples for Including/Excluding levels, but
# let's use "our" data to illustrate.

# table() does not count NA by default:
table(arrests$Children)

# To include NA, you can set the exclude argument to NULL:
table(arrests$Children, exclude = NULL)

# You can also use the useNA argument, which takes three values: "no", "ifany",
# or "always". The default is "no":
table(arrests$Children, useNA = "no")
# show NA if any exist
table(arrests$Children, useNA = "ifany")

# useNA = "always" means show tally of NA even if there is none:
with(arrests, table(Children[!is.na(Children)], useNA = "always"))

# We can also exclude certain factor levels. For example, let's exclude anyone
# who had more than 7 children and NA:
table(arrests$Children, exclude = c(7:23,NA))

# We mentioned xtabs() as another way to create contigency tables. I find 
# table() a little more intuitive to use, but xtabs() is nice because it has the
# formula interface. For example, here's how we can use it to create a two-way
# table of Events and Cloud.Cover.Index from the weather data.
xtabs( ~ Events + Cloud.Cover.Index, data=weather)

# Notice there is nothing on the left of the "~". We do that when creating a 
# table of factors in a data frame for which there is no column of counts. That 
# tends to be how most data are formatted, and thus this seems to be the way
# xtabs() is most often used. Note this is identical to:
with(weather, table(Events, Cloud.Cover.Index))

# A nice feature of the formula interface is the presence of the subset 
# argument, which allows to specify subsetting conditions in xtabs(). Below we
# creat the same table but only for Max.TemperatureF > 50:
xtabs( ~ Events + Cloud.Cover.Index, data=weather, 
       subset= Max.TemperatureF > 50,
       exclude=c("None","missing"))

# The prop.table() function creates tables with proportions instead of counts. 
# The basic syntax is prop.table(x, margin) where x is a table and margin is the 
# index (or indices) to generate marginal proportions. Let's go back to the
# UCBAdmissions data. Recall it was a 3-way table. We can access parts of the
# table using subsetting brackets. Notice it's a 2 x 2 x 6 table.
dim(UCBAdmissions) # rows x columns x levels

# To access the first table for department A:
(deptA <- UCBAdmissions[,,1]) # UCBAdmissions[,,"A"] works as well

# Let's see how prop.table works on deptA (two-way table):
prop.table(deptA) # all sum to 1
prop.table(deptA, margin = 1) # rows sum to 1
prop.table(deptA, margin = 2) # columns sum to 1

# Let's see how prop.table works on a three-way table
(deptAB <- UCBAdmissions[,,1:2]) # UCBAdmissions[,,c("A","B")] works too

# all 2x4=8 cells sum to 1
prop.table(deptAB) 

# all 4 row entries for each level of Admit sum to 1
prop.table(deptAB, margin = 1) 
# For Admitted:
0.5272915 + 0.09165808 + 0.3635427 + 0.01750772

# all 4 column entries for each level of Gender sum to 1
prop.table(deptAB, margin = 2) 
# For Male
0.3696751 + 0.2259928 + 0.2548736 + 0.1494585

# all entries in each 2x2 table sum to 1
prop.table(deptAB, margin = 3) 

# rows within each 2x2 table sum to 1
prop.table(deptAB, margin = c(1,3)) 

# columns within each 2x2 table sum to 1
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

# Finally the ftable() function flattens tables to make them a little easier to
# read:
ftable(UCBAdmissions)
# You can use the row.vars and col.vars arguments to change what is displayed in
# the rows and columns:
ftable(UCBAdmissions, row.vars=3) # 3rd dimension as row variable
ftable(UCBAdmissions, col.vars=c("Admit","Dept"))

# The gmodels package has a nice function for creating tables called CrossTable
# that outputs something similar to what you might see in SAS or SPSS.

# install.packages("gmodels")
library(gmodels)
CrossTable(UCBAdmissions[,,1])
CrossTable(UCBAdmissions[,,1], format="SPSS")

CrossTable(arrests$Sex)


# Continuous data ---------------------------------------------------------

# Aggregating continuous data means doing things like finding the mean score per
# group, the median income per state, and so on. We split data into groups, 
# apply a statistical calculation to each group, and combine back together in
# data frame or vector.

# The bread and butter aggregation function in base R is aggregate(). It works 
# with a formula interface and will return a data frame, but can only use one
# function at a time. Let's see some examples using our weather data:

# Find the mean minimum temperature per Event
aggregate(Min.TemperatureF ~ Events, data=weather, mean)
# Find the median maximum temperature per Event
aggregate(Max.TemperatureF ~ Events, data=weather, median)

# We can specify two responses using the cbind() function. Below we request both
# means for Min and Max temperature per Event.
aggregate(cbind(Min.TemperatureF,Max.TemperatureF) ~ Events, data=weather, mean)

# With the formula interface we can specify multiple factors. Let's use the 
# mtcars data to demonstrate. (1974 Motor Trend US magazine data)
head(mtcars)

# Here we calculate mean mpg for each combination of number of gears and
# transmission  (0 = automatic, 1 = manual)
aggregate(mpg ~ gear + am, data=mtcars, mean)

# We can also specifiy multiple responses:
aggregate(cbind(mpg, drat) ~ gear + am, data=mtcars, mean)

# We can define our own functions with aggregate. Here we examine the chickwts
# data set. It has two columns: weight and feed (6-level factor)
str(chickwts)

# Say we want to calculate the mean and standard error:
aggregate(weight ~ feed, data=chickwts, mean)
aggregate(weight ~ feed, data=chickwts, function(x)round(sd(x)/sqrt(length(x)),2))
# NOTE: round(x,n) rounds x to n decimal places

# we can save the results as a data frame and manipulate accordingly:
chickM <- aggregate(weight ~ feed, data=chickwts, mean)
chickSE <- aggregate(weight ~ feed, data=chickwts, function(x)sd(x)/sqrt(length(x)))
# create a new data frame, plot means and standard error bars:
chicks <- data.frame(feed=chickM$feed, mean=chickM$weight, SE=chickSE$weight)
chicks
# plot means and SE bars
stripchart(weight ~ feed, data=chickwts, pch=1, vertical = T, ylim=c(100,400), las=1,
           col="grey60")
points(1:6,chicks$mean, pch=19, col="red")
# add 2x standard error bars:
segments(x0 = 1:6, y0 = chicks$mean+(2*chicks$SE),
         x1 = 1:6, y1 = chicks$mean-(2*chicks$SE), 
         col="red")

# Another function useful for aggregating continuous data is tapply(), which we 
# discussed in a previous lecture. The main differences between it and
# aggregate() are there is no formula interface and it does not return a data
# frame.

# The basic syntax is tapply(X, INDEX, FUN), where X is a vector of values, 
# INDEX is a vector(s) of group labels for X, and FUN is a function. Going to
# back to our weather data again:

tapply(weather$Min.TemperatureF, weather$Events, mean)
# or little easier using with():
with(weather, tapply(Min.TemperatureF, Events, mean))

# The output is technically an "array", not a data frame.

# tapply is not limited to returning scalars (ie, single values). For example,
# see how tapply and aggregate differ in the way they return a range:
aggregate(Min.TemperatureF ~ Events, data=weather, range) # data frame
tapply(weather$Min.TemperatureF, weather$Events, range) # list

# tapply can take more than one value for the INDEX argument. They just need to
# be formatted as a list.
with(mtcars, tapply(mpg, list(gear, am), mean))

# Notice tapply returned an two-way table with every possible combination, even
# those for which no values of mpg exist. This differs from aggregate() which
# returned a data frame only containing rows for those combinations with values
# of mpg.
aggregate(mpg ~ gear + am, data=mtcars, mean)

# The doBy package has a nice function for group summaries called summaryBy. It
# works like aggregate() except you can specify more than one function. 

# install.packages("doBy")
library(doBy)
# using 3 functions: mean,var,length
summaryBy(Min.TemperatureF ~ Events, data=weather, FUN=c(mean,var,length))
# using 3 functions (mean,var,length) and multiple responses
summaryBy(Min.TemperatureF + Max.TemperatureF ~ Events, 
          data=weather, FUN=c(mean,var,length))

# The summaryBy() function, and indeed the entire doBy package, does much more.
# Read the vignette.

# Using TRUE/FALSE --------------------------------------------------------

# TRUE/FALSE conditional vectors can be quite powerful for aggregation. Since 
# they equal 1/0, we can sum them up for counts and take their mean to find
# proportions.

# number of people arrested in Parisian insurrection under the age of 25:
sum(arrests$Age < 25, na.rm = T)

# proportion of people arrested in Parisian insurrection under the age of 25:
mean(arrests$Age < 25, na.rm = T)

# number of days hotter than 90 degress in 2013
sum(weather$Max.TemperatureF > 90)

# two-way table of Temp > 90 and Humidity > 90
table(weather$Max.TemperatureF > 90, weather$Max.Humidity > 90)


# sweep() -----------------------------------------------------------------

# The sweep() function allows you to process matrix rows or columns differently 
# based on values in an auxiliary vector. One example is calculating proportions
# in a table, say by column. You have to first sum the columns, and then divide 
# each value in a column by its column sum. The sweep() function generalizes
# this operationn.

# sweep() takes 4 arguments: a matrix, the matrix margin, an auxiliary matrix
# and a function.

# calculating column proportions in a table:
# first create the "matrix":
(tab <- with(arrests, table(MaritalStatus, Sex)))
# Note: For Sex, 1 = Male, 2 = Female, 9 = No info; For MaritalStatus, 1 = 
# Married, 2 = Single, 3 = Widowed, 9 = No info

# create an auxiliary vector of column totals:
cs <- colSums(tab)
cs
# Then sweep. In other words, divide each value in each column of the tab matrix
# by the value in the cs vector:
sweep(tab, 2, cs, "/") 

# Of course, prop.table() does the same things:
prop.table(tab,margin = 2)

# In fact, the help page for prop.table says "This is really sweep(x, margin,
# margin.table(x, margin), "/") for newbies"
rm(tab,cs)

# centering variables: first select only those columns from weather that are of
# class integer and not just 0,1 (or greater than two values):
temp <- weather[,sapply(weather,class)=="integer" & 
                  sapply(lapply(weather, unique), length)!= 2]
# calculate means of each column
cm <- colMeans(temp, na.rm = TRUE)
# Then sweep. In other words, substract from each value in each column of the
# temp matrix the corresponding value in the cm vector.
centered <- sweep(temp, 2, cm, "-")
head(centered[,1:3])

# Of course this does the same thing:
centered2 <- scale(temp, scale = FALSE, center = TRUE)
head(centered2[,1:3])


# mapply() ----------------------------------------------------------------

# mapply is a multivariate version of sapply. The basic syntax is mapply(FUN, 
# ...) where FUN is a function and ... are arguments. With sapply(), you apply a
# function to a single data structure. With mapply, you can apply a function to
# multiple data structures. This is best explained with a demonstration.

# Let's say we want to calculate the mean value for each variable in our temp 
# matrix using only those values that are greater than the median in each
# column. First we write a function where v=vector and m=mean:
meanmed <- function(v,m) mean(v[v >= m], na.rm=TRUE)
# next we calculate the medians
meds <- apply(temp,2,median, na.rm=TRUE)
# Then we use mapply to apply the meanmed function to the two data structures.
mapply(meanmed, v=temp, m=meds)

# How would we do that without mapply? 
sapply(sapply(temp, function(x)x[x >= median(x, na.rm=TRUE)]), mean, na.rm=TRUE)

# That's why we say "mapply is a multivariate version of sapply."

# Of course you can also do something like this!
meanMed <- function(x){
  tmp <- x[x >= median(x, na.rm=TRUE)]
  mean(tmp, na.rm=TRUE)
}
apply(temp,2,meanMed)

# mapply can pretty much do anything that sapply can do, you just have to
# reverse the order of the arguments.
sapply(weather, class)
mapply(class, weather)

