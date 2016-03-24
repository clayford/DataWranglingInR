#' ---
#' title: "Data Wrangling in R: Aggregation"
#' author: "Clay Ford"
#' date: "Spring 2016"
#' output: pdf_document
#' ---

load("../data/datasets_L07.Rda")

# Aggregation means collecting units in a group and performing an operation on 
# them such as taking the sum or mean. In many cases, THAT is the data analysis.
# That's what is referred to as descritive or summary statistics. Data
# aggregation is also used to create a data frame for inferential analysis, to
# prepare data for graphing, or as an intermediate step for additional data
# wrangling.



# categorical data --------------------------------------------------------

# Aggregating categorical data usually means creating cross-tabulations, or 
# cross-tabs for short. R has a number of functions to help tabulate membership
# in categories.


# table -------------------------------------------------------------------

# The basic function is table(). Simply give table() objects that can be 
# intertrepted as factors and it will construct a table. Below we generate two 
# vectors and ask table to cross-classify them. Imagine the two vectors stacked
# on top of one another and R counting up the number of combinations.

X <- sample(1:2,size = 500,replace=TRUE,prob = c(0.8,0.2))
Y <- sample(c("A","B"),size = 500,replace=TRUE, prob = c(0.2,0.8))
X[1:4]
Y[1:4]
table(X,Y)
table(X)
# tables can be saved:
tab <- table(X,Y)
tab
class(tab)
str(tab) # a matrix of class "table"

# see the row and column names using dimnames(); it's a list
dimnames(tab)
dimnames(tab)$X
dimnames(tab)$Y

# change the row and column values
dimnames(tab)$X <- c("M","F")
dimnames(tab)$Y <- c("Yes","No")
tab

# change the row and column name and values; need to use a list
# first remake the table
tab <- table(X,Y)
tab
dimnames(tab) <- list(gender=c("M","F"), preference=c("Y","N"))
tab

# calling summary on a table object produces the following:
summary(tab)

# look at the structure
str(summary(tab))
# we see it's a list object. This means we can extract information from it, such
# as the p-value for the test of independence:
summary(tab)$p.value

# we can extract table values into a vector using c():
c(tab)

# we can sum all the cells using sum():
sum(tab)

# we can do calculations, like find cell frequencies:
tab/sum(tab)
# Below we'll look at prop.table() for calculating cell proportions.

# Tables can go beyond 2 dimensions. Let's add a third dimension:
Z <- sample(c("I","II"), size=500, replace=T)
table(X,Y,Z)
# The tables in the 3rd dimension are sort of like subsetting a data frame by
# the 3rd dimentsion's value and then creating a 2-way table:
tdf <- data.frame(X,Y,Z)
head(tdf)
with(tdf[tdf$Z=="II",], table(X,Y))


# Example with arrests data -----------------------------------------------

# Lets update the codes for Sex and MaritalStatus and then do some
# crosstabulations.

# Note: For Sex, 1 = Male, 2 = Female, 9 = No info; 
# For MaritalStatus, 1 = Married, 2 = Single, 3 = Widowed, 9 = No info
arrests$Sex <- factor(arrests$Sex, labels = c("Male","Female","No Info"))
arrests$MaritalStatus <- factor(arrests$MaritalStatus, 
                                labels = c("Married","Single","Widowed", "No Info"))

with(arrests, table(Sex, MaritalStatus))

# can use the exclude argument to exclude levels
with(arrests, table(Sex, MaritalStatus, exclude = "No Info"))
with(arrests, table(Sex, MaritalStatus, 
                    exclude = c("No Info","Widowed")))


# FinalOutcome is the code reflecting the final outcome of proceedings against
# the individual. 1 = Freed, 2 = Pardoned
table(arrests$FinalOutcome)

# Look at cross tabulation for Sex and Marital Status by Freed and Pardoned. We
# could do this using subset():
with(subset(arrests, FinalOutcome %in% c(1,2)), 
     table(Sex, MaritalStatus, FinalOutcome))

# or use exclude:
with(arrests, table(Sex, MaritalStatus, FinalOutcome, 
                    exclude = c("No Info", 0, 3:10)))

# table() data works with the barplot() and plot() functions for quick visuals:
barplot(table(arrests$FinalOutcome), ylab = "Final Outcome")
plot(table(arrests$FinalOutcome), type="h", ylab = "Final Outcome")

barplot(with(arrests, table(Sex, MaritalStatus, exclude = "No Info")), 
        beside = TRUE, legend.text = TRUE, main = "Arrests 1848")



# xtabs -------------------------------------------------------------------

# The xtabs function works like table except it can produce tables from 
# frequencies using the formula interface. Let's say we have the following data:

dat <- data.frame(gender = c("M","M","F","F"),
                  pref = c("Y","N","Y","N"),
                  freq = c(12,19,23,4))
dat

# This is basically a cross-tabulation expressed as a data frame. xtabs can take
# a data frame with a frequency column and return an actual cross-tabulated
# table. The syntax is (frequency ~ row + column)
xtabs(freq ~ gender + pref, data=dat)

# Notice the data argument that allows us to reference the data frame.

# However, like table, xtabs can produce cross-tabulations for data sets with
# one row per count. The trick is to not put anything before the "~":
xtabs( ~ FinalOutcome, data=arrests)
xtabs( ~ Sex + MaritalStatus, data=arrests)
xtabs( ~ Events + Cloud.Cover.Index, data=weather)

# The xtabs also has a built-in subset argument so we can subset data on the fly:
xtabs( ~ Sex + MaritalStatus, data=arrests, subset = Age < 40)

# it also has the exclude argument
xtabs( ~ Sex + MaritalStatus, data=arrests, subset = Age < 40, exclude = "No Info")

xtabs( ~ Events + Cloud.Cover.Index, data=weather, 
       subset= Max.TemperatureF < 72,
       exclude="None")


# as.data.frame -----------------------------------------------------------

# We can create a data frame with tabulations using the as.data.frame() function
# with a table. If we look at methods(as.data.frame) we see many methods for the
# as.data.frame() function. This means it behaves differently depending on what 
# you give it. We're interested in what it does with tables, that is 
# as.data.frame.table()

# Let's save the arrests table of Sex and MaritalStatus
with(arrests, table(Sex, MaritalStatus, exclude = "No Info"))
t1 <- with(arrests, table(Sex, MaritalStatus, exclude = "No Info"))
as.data.frame(t1)
df1 <- as.data.frame(t1)

# Notice the cell counts from the table now have their own column: Freq. We can
# change that using the responseName argument.
df1 <- as.data.frame(t1, responseName = "Count")
df1

# And of course, as mentioned above, xtabs() can reverse the operation and
# re-create the cross-tabulation.
xtabs(Count ~ Sex + MaritalStatus, data=df1)

# We can also use a period on the right hand side of the "~" to indicate we want
# to include all other variables:
xtabs(Count ~ . , data=df1)



# missing data in tables --------------------------------------------------

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
table(arrests$Sex, useNA = "always")


# proportions and margins -------------------------------------------------

# The prop.table() function creates tables with proportions. The basic syntax is
# prop.table(x, margin) where x is a table and margin is the index (or indices)
# to generate marginal proportions. Let's demonstrate:

tab1 <- xtabs( ~ Sex + MaritalStatus, data=arrests, exclude = "No Info")
tab1

# all cells sum to 1
prop.table(tab1)
# rows sum to 1
prop.table(tab1, margin = 1)
# columns sum to 1
prop.table(tab1, margin = 2)


# Let's demonstrate with a 3-way table. R includes a data set called 
# UCBAdmissions. It contains aggregate data on applicants to graduate school at 
# UC Berkeley for the six largest departments in 1973 classified by admission
# and sex. It's already a 3-way contingency table:

UCBAdmissions 
dim(UCBAdmissions) # rows x columns x levels

# Let's subset it to for demo purposes; just get data for depts A and B:
(deptAB <- UCBAdmissions[,,c("A","B")]) 

# all cells sum to 1
prop.table(deptAB) 

# all 4 row entries for each level of Admit sum to 1
prop.table(deptAB, margin = 1) 
# For Admitted:
0.5272915 + 0.09165808 + 0.3635427 + 0.01750772

# all 4 column entries for each level of Gender sum to 1
prop.table(deptAB, margin = 2) 
# For Male
0.3696751 + 0.2259928 + 0.2548736 + 0.1494585

# all cells in each 2x2 table sum to 1
prop.table(deptAB, margin = 3) 

# rows within each 2x2 table sum to 1
prop.table(deptAB, margin = c(1,3)) 

# columns within each 2x2 table sum to 1
prop.table(deptAB, margin = c(2,3)) 

# The addmargins() function does what you probably guess it does: it adds table 
# margins.
addmargins(tab1)

# Like prop.table, addmargins() has a margin argument that you can use to
# specify which dimension is summed.
addmargins(tab1, margin = 1) # sum down the rows
addmargins(tab1, margin = 2) # sum across the columns
addmargins(tab1, margin = c(1,2)) # sum across both dimensions (default)

# on a 3-way table
addmargins(deptAB)
# sums both tables into a 3rd table, whose rows and columns are again summed!

addmargins(deptAB, margin = 1) # sum rows for both tables
addmargins(deptAB, margin = 2) # sum columns for both tables
addmargins(deptAB, margin = c(1,2)) # sum rows and columns for both tables
addmargins(deptAB, margin = 3) # sum both tables

# The margin.table() function will extract just the margin totals. Say the
# documentation: "This is really just apply(x, margin, sum) packaged up for
# newbies, except that if margin has length zero you get sum(x)."

margin.table(tab1) # same as sum(tab1)

margin.table(tab1, margin = 1) # row margins
apply(tab1, 1, sum)

margin.table(tab1, margin = 2) # column margins
apply(tab1, 2, sum)


# Extended example with UBCAdmissions -------------------------------------

# This data set is frequently used for illustrating Simpson's paradox. This is 
# where a trend that appears in different groups of data disappears or reverses 
# when the groups are combined. Or put another way, a trend that appears between
# two variables disappears or reverses when a third variable is incorporated
# into the analysis. At issue in the UBCAdmissions data is whether the data show
# evidence of sex bias in admission practices.

# Let's make a data frame with frequencies:
DF <- as.data.frame(UCBAdmissions)
names(DF)

# cross tabulation of Gender and Admit, ignoring Depts:
xtabs(Freq ~ Gender + Admit, data=DF)
prop.table(xtabs(Freq ~ Gender + Admit, data=DF), margin = 1)

# Males seem more likely to be admitted. 

# Now incorporate Depts:
xtabs(Freq ~ Gender + Admit + Dept, data=DF)
prop.table(xtabs(Freq ~ Gender + Admit + Dept, data=DF), margin = c(1,3))

# The bias seems to disappear, or even reverse in one case.


# ftable ------------------------------------------------------------------

# The ftable() function flattens tables to make them a little easier to read:
ftable(UCBAdmissions)
# You can use the row.vars and col.vars arguments to change what is displayed in
# the rows and columns:
ftable(UCBAdmissions, row.vars=3) # 3rd dimension as row variable
ftable(UCBAdmissions, col.vars=c("Admit","Dept"))


# CrossTable --------------------------------------------------------------


# The gmodels package has a nice function for creating tables called CrossTable
# that outputs something similar to what you might see in SAS or SPSS.

# install.packages("gmodels")
library(gmodels)
with(arrests, CrossTable(Sex, MaritalStatus))
with(arrests, CrossTable(Sex, MaritalStatus, format = "SPSS"))


# Continuous data ---------------------------------------------------------

# Aggregating continuous data means doing things like finding the mean score per
# group, the median income per state, and so on. We split data into groups, 
# apply a statistical calculation to each group, and combine back together in
# data frame or vector.


# aggregate ---------------------------------------------------------------

# The bread and butter aggregation function in base R is aggregate(). It works 
# with a formula interface and will return a data frame, but can only use one
# function at a time. Let's see some examples using our weather data:

# Find the mean minimum temperature per Event
aggregate(Min.TemperatureF ~ Events, data=weather, mean)

# Find the maximum temperature per Event
aggregate(Max.TemperatureF ~ Events, data=weather, max)

# total precipitation by month; note the month() function
library(lubridate)
aggregate(PrecipitationIn ~ month(Date), data=weather, sum)

# maximum max temperture by month
aggregate(Max.TemperatureF ~ month(Date, label = TRUE), data=weather, max)

# probably better to just update the data frame with month
weather$Month <- month(weather$Date, label = TRUE)
aggregate(Max.TemperatureF ~ Month, data=weather, max)

# subsetting just for days with Event = "Rain"
aggregate(Max.TemperatureF ~ Month, data=weather, max,
          subset = grepl("^Rain$", Events))

# We can specify two responses using the cbind() function. Below we request both
# means for Min and Max temperature per Event.
aggregate(cbind(Min.TemperatureF,Max.TemperatureF) ~ Month, data=weather, mean)


# With the formula interface we can specify multiple factors. 
aggregate(Mean.TemperatureF ~ Month + Events, data=weather, mean)

aggregate(Mean.TemperatureF ~ Month + Events, data=weather, mean, 
          subset = Month=="Jan")

aggregate(PrecipitationIn ~ Month + Events, data=weather, sum, 
          subset = grepl("Rain", Events) & Month %in% c("Jun","Jul","Aug"))


# again with multiple responses:
aggregate(cbind(Min.DewpointF, Min.Humidity) ~ Month + Events, 
          data=weather, min, subset = grepl("Thunder", Events))


# Extended example --------------------------------------------------------


# We can define our own functions with aggregate. Here we examine the chickwts 
# data set. This data was for an experiment to measure and compare the 
# effectiveness of various feed supplements on the growth rate of chickens. It
# has two columns: weight and feed (6-level factor)
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
stripchart(weight ~ feed, data=chickwts, 
           pch=1, # type of plotting character (pch) 
           vertical = T, # groups on x-axis
           ylim=c(0,450), # range of y-axis 
           las=1, # turn numbers on y-axis upright
           col="grey60") # color of dots

# add means to graph as red solid points;
# points(x,y) - add points at (x,y) coordinates
points(1:6,chicks$mean, pch=19, col="red")

# add red 2x standard error bars;
# segments extending from (x0,y0) to (x1,y1)
segments(x0 = 1:6, y0 = chicks$mean+(2*chicks$SE),
         x1 = 1:6, y1 = chicks$mean-(2*chicks$SE), 
         col="red")

# tidy up
rm(list = ls(pattern = "^chick"))

# tapply ------------------------------------------------------------------

# Another function useful for aggregating continuous data is tapply(), which we 
# discussed in a previous lecture. The main differences between it and
# aggregate() are there is no formula interface and it does not return a data
# frame. 


# ave ---------------------------------------------------------------------


# Another base R function useful for aggregation is ave(). It provides a short 
# cut for aggregating data and then merging back into the original data set. The
# ave() function does the same thing as aggregate or tapply, but returns a 
# result the same length as the source data. An example will help explain.

# Let's say I want to store total precipitation per month in my weather data 
# frame. That means all records for, say, January will have the same total
# precipitation value (5.054).
aggregate(PrecipitationIn ~ Month, weather, sum)

# I could do something like this.
TP <- aggregate(PrecipitationIn ~ Month, weather, sum)
names(TP)[2] <- "Total.Precip.Month"
weather <- merge(TP, weather, by="Month")
weather[1:6,c("Month","PrecipitationIn", "Total.Precip.Month")]

# Works, but required a merge, which sorted our data frame alphabetically!

# The ave() function takes care of this for us. Let's reset the data frame.
weather$Total.Precip.Month <- NULL
weather <- weather[order(weather$Date),]
rownames(weather) <- NULL
weather[1:6,c("Date","PrecipitationIn")]

# Now let's use ave(). The basic syntax is ave(x, ..., FUN), where x is the 
# vector of numbers to aggregate, ... are the grouping variables, and FUN is the
# function to apply to each group, defaulting to mean.

# find total precip per month
ave.out <- ave(x = weather$PrecipitationIn, weather$Month, FUN = sum)
length(ave.out) # same length (number of rows) as weather

# Notice that the sum is repeated to match each month. 
head(weather$Month)
head(ave.out)

# We can easily add the Total Precip per month to the weather data frame as 
# follows:
weather$Total.Precip.Month <- ave(weather$PrecipitationIn, weather$Month, FUN = sum)
weather[1:6,c("Month","PrecipitationIn", "Total.Precip.Month")]

# Now that's better. Did not have to do merge and data frame still sorted from
# Jan - Dec.
rm(ave.out)

# summaryBy ---------------------------------------------------------------


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


# More aggregation --------------------------------------------------------

# Some more examples of aggregating using the functions we've discussed so far.

head(allStocks)

# Let's add a column to allStocks for day of the week. Base R has a built-in 
# function for this called weekdays(). I prefer the wday() function in
# lubridate because it automatically turns into an ordered factor.
library(lubridate)
allStocks$Day <- wday(allStocks$Date, label = TRUE)
head(allStocks)
class(allStocks$Day) # ensures days are presented in proper order in plots

# Let's add month as well. Base R has a months function for this but I prefer
# lubridate's month() function for the same reasons I prefered wday().
allStocks$Month <- month(allStocks$Date, label=TRUE)
head(allStocks)

# Say I want find mean change in opening and closing price for all stocks by day
# of the week. Probably easiest to first add a column to the data frame for 
# difference in opening and closing
allStocks$Change <- allStocks$Close - allStocks$Open

# Now find mean change in opening and closing price for all stocks by day
# of the week.
aggregate(Change ~ Day + Stock, data=allStocks, mean)

# I could save this data frame and do some visualization with it.
mout <- aggregate(Change ~ Day + Stock, data=allStocks, mean)
library(ggplot2)
ggplot(mout, aes(x=Day, y=Change, group=Stock, color=Stock)) + 
  geom_point() + geom_line() + geom_hline(yintercept=0, linetype = 2)

# Buy Monday, sell Tuesday?

# We could refine this graph and look at all changes by day, by month.
mout2 <- aggregate(Change ~ Day + Stock + Month, data=allStocks, mean)
head(mout2)
ggplot(mout2, aes(x=Day, y=Change, group=Stock, color=Stock)) + 
  geom_point() + geom_line() + geom_hline(yintercept=0, linetype = 2) +
  facet_wrap(~ Month)


# The whole "Buy Monday, sell Tuesday" thing doesn't seem to hold throughout the
# year.

# save data for next set of lecture notes
save(list=c("electionData", "weather", "arrests", "allStocks", "popVa", "airplane",
            "SenateBills"), file="../data/datasets_L08.Rda")


# time-permitting bonus material ------------------------------------------

# mapply() ----------------------------------------------------------------

# mapply is a multivariate version of sapply. The basic syntax is mapply(FUN, 
# ...) where FUN is a function and ... are arguments to the function. With
# sapply(), you apply a function to a single data structure. With mapply, you
# can apply a function to multiple data structures. This is best explained with
# a demonstration.

# function that calculates BMI
bmi <- function(weight, height) (weight/(height^2))*703

# a data frame with weights and heights of 100 males
dat <- data.frame(weight = round(runif(100,140,220)),
                  height = round(runif(100,60,75))) 
head(dat)

# How to "apply" bmi function to the weight and height columns? mapply
mapply(bmi, weight = dat$weight, height = dat$height)

# or add to data frame
dat$bmi <- mapply(bmi, weight = dat$weight, height = dat$height)
head(dat)

# can also do with a for loop but requires more setup
bmiVals <- numeric(nrow(dat)) # container vector for bmi values
for(i in 1:nrow(dat)){
  bmiVals[i] <- bmi(weight = dat$weight[i], height = dat$height[i])
}
bmiVals

bmi(dat$weight, height = dat$height)

# rle ---------------------------------------------------------------------

# Sometimes categorical/discrete data is recorded in order (or over time) and we
# want to summarize "runs", such as how often something occured in a row or how 
# many times something continued to increase. As a quick example, let's simulate
# flipping a coin 1000 times. 
set.seed(1)
flips <- sample(c("H","T"), size = 1000, replace = TRUE)

# What's the longest streak of Heads? We could answer this with a for loop and 
# conditional statements and some book-keeping. But it turns out base R has a
# built-in function for this: rle (rle stands for "Run Length Encoding")

rle(flips)
flips[1:10]

# The output of rle reports two heads, then two tails, then one head, then four
# tails...

# we usually want to save the output to an object and then manipulate it.
rout <- rle(flips)

# this has all the streaks:
rout$lengths[1:10]

# this locates the largest streak
max(rout$lengths)

# Is there more than one such streak?
sum(rout$lengths == max(rout$lengths))
# or..
table(rout$lengths)

# when does the streak start? 
# First get the index of the max streak from rout$lengths
k <- which(rout$lengths == max(rout$lengths))
k

# Now use rep to recreate the sequence of flips leading up to the beginning of
# the streak, and then find the length of *that* sequence.
st <- length(rep(rout$values[1:(k-1)], rout$lengths[1:(k-1)]))
st # starts at flip 270

# we can also visualize distribution of streaks:
barplot(table(rout$lengths))

# Using a more realistic example, what was the longest streak in 2013 of
# consecutive increases in the maximum temperature? And when did it start?

rout <- rle(diff(weather$Max.TemperatureF) > 0)
table(rout$lengths) 
k <- which(rout$lengths == max(rout$lengths))
st <- length(rep(rout$values[1:(k-1)], rout$lengths[1:(k-1)]))
weather[st+1,"EST"]
weather[(st+1):(st+8),c("EST","Max.TemperatureF")]



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
# class integer, not just 0,1 (or greater than two values) and not named
# Cold.Rank:
temp <- weather[,sapply(weather,class)=="integer" & 
                  sapply(lapply(weather, unique), length)!= 2 & 
                  names(weather) != "Cold.Rank"]
# calculate means of each column
cm <- colMeans(temp, na.rm = TRUE)
# Then sweep. In other words, substract from each value in each column of the
# temp matrix the corresponding value in the cm vector.
centered <- sweep(temp, 2, cm, "-")
head(centered[,1:3])

# It turns out the function scale() does the same thing:
centered2 <- scale(temp, scale = FALSE, center = TRUE)
head(centered2[,1:3])

