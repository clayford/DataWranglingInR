#' ---
#' title: "Data Wrangling in R: Combining, Merging and Reshaping Data"
#' author: "Clay Ford"
#' date: "Spring 2016"
#' output: pdf_document
#' ---


# load data from last lecture
load("../data/datasets_L04.Rda")


# Sometimes we have multiple data frames we want to combine. There are typically
# three ways to do this: (1) stack on top of each other, (2) place side-by-side,
# or (3) merge together based on common variables.


# Stacking ----------------------------------------------------------------

# Let's generate some fake data to illustrate combining data frames by stacking.

first <- data.frame(x0=1:5,
                    x1=rnorm(5),
                    x2=c("M","F","M","F","F"))
first
second <- data.frame(x0=10:14,
                     x1=rnorm(5),
                     x2=c("M","F","M","F","F"))
second
third <- data.frame(x4=c(3,3,1,3,2),
                    x5=c("e","g","v","b","z"))
third

# We can use the rbind() function to stack data frames. Make sure the number of 
# columns match. Also, the names and classes of values being joined must match.
# Here we stack the first, the second and then the first again:
rbind(first, second, first)
class(rbind(first, second, first)) # still a data frame

# works with vectors too:
rbind(1:3,4:6)
class(rbind(1:3,4:6)) # matrix

# Remember the allStocks data? This is a list containing 7 data frames of stock
# data for 7 different companies.
names(allStocks)

# We can use rbind to combine these into one data frame. The thing is we have to
# call rbind repeatedly since there are so many data frames. 
# rbind(allStocks$bbby.csv, allStocks$flws.csv, allStocks$foxa.csv,...)

# A useful function for this type of task in R is the do.call() function. This 
# function allows you to call any R function, but instead of writing out the 
# arguments one by one, you can use a list to hold the arguments of the 
# function. The basic syntax is do.call(what, args), where "what" is a function 
# and "args" are the arguments to pass to the function IN A LIST.

# Since allStocks is a LIST of data frames, and rbind can take data frames as 
# arguments, we can simply pass allStocks to rbind via the do.call function.
allStocks <- do.call(rbind, allStocks)

str(allStocks)
# Let's go ahead and fix the Date column name:
names(allStocks)[1] <- "Date"

# Let's look at the first few:
head(allStocks)

# We see that R very kindly created row names for us that identifies the source 
# data frame for each row! That's because rbind() has a logical argument called 
# "make.row.names" that defaults to TRUE. We can exploit that information to
# create a variable that indicates which record belongs to which company. In
# other words, we can add a column called "company" that will list "BBBY",
# "FLWS", etc. for their respective rows. We will do this in a later lecture.

# But what if you didn't want that behavior? We could set make.row.names = 
# FALSE. The trick to doing that with do.call is that you need c() to add
# further arguments. So you have to do something like this:
# 
# allStocks <- do.call(rbind, c(allStocks, make.row.names = FALSE))

# Recall that c() combines values into vectors OR lists.

is.vector(
  c(1,2,3)
  )

is.list(
  c(1, 2, list(g=c("m","f"), x=2:4))
  )

apply(iris[,-5], 2, function(x) c(mean(x), sd(x), length(x)))


# Side-by-side ------------------------------------------------------------

# Use the cbind function to combine data frames side-by-side:
cbind(first,third)
class(cbind(first,third))

# WARNING: cbind does not require matching heights; if one data frame is shorter
# it will recycle it. Notice below the third data frame is recycled.
cbind(rbind(first,second),third)

# However, if the number of rows of the shorter data frame does not evenly 
# divide into the number of rows of the taller data frame, then R throws an 
# error.

# cbind(rbind(first,second),third[-1,])
# Error in data.frame(..., check.names = FALSE) : 
#   arguments imply differing number of rows: 10, 4


# A note about cbind and rbind --------------------------------------------

# Using cbind and rbind on vectors does NOT produce data frames.
x <- 1:3; y <- letters[1:3]

class(cbind(x,y))
is.data.frame(cbind(x,y))
is.data.frame(rbind(x,y))

dat <- data.frame(x,y)

# However if one of the objects you're binding is a data frame, then you do end
# up with a data frame.
dat <- data.frame(x,y)
z <- c("a","a","c")
is.data.frame(cbind(dat,z))


# Merging -----------------------------------------------------------------

# When we wish to join two data sets together based on common variables, we use 
# the merge() function. For example, let's say we have a data set of crime 
# statistics for all 50 US states, and another data set of demographic 
# statistics for all 50 US states. We may want to merge them together so we have
# one row per state that contains crime and demographic statistics. The common
# variable between the two data sets would be "state".

# The basic syntax for merge() is merge(x, y, by, by.x, by.y), where "x" and "y"
# are the respective data sets, "by" is the column(s) to merge by (assuming the 
# column names match between the two data sets), and "by.x" and "by.y" are also 
# columns to merge by in the event the column names do not match between the two
# data sets. You either use by or by.x and by.y, not all three. From the help 
# file: "By default the data frames are merged on the columns with names they 
# both have, but separate specifications of the columns can be given by by.x and
# by.y." In other words, you don't have to use the by argument if your data
# frames have matching column names that you want to merge on.


# Let's create some more fake data to illustrate:
left <- data.frame(id=c(2:7),
                     y2=rnorm(6,100,5))
left
right <- data.frame(id=rep(1:4,each=2),
                    z2=sample(letters,8, replace=TRUE))
right

# Data frames left and right have columns "id" in common. Let's merge them 
# together based on id:
merge(left, right)

# Notice y2 from the left data frame is recycled to match up with multiple id in
# the right data frame. Also notice only rows with matching ids in both data
# frames are retained. In database terminology this is known as an INNER JOIN.
# Only those records with matching "by" variables are joined.

# If we wanted to merge all rows regardless of match, we use the argument
# all=TRUE. It is FALSE by default. This creates an OUTER JOIN.
merge(left, right, all=TRUE)

# If we want to retain everything in the left data frame and merge only what 
# matches in the right data frame, we specify all.x=TRUE. This is known as a
# LEFT JOIN.
merge(left, right, all.x=TRUE)

# If we want to retain everything in the right data frame and merge only what 
# matches in the left data frame, we specify all.y=TRUE. This is known as a
# RIGHT JOIN.
merge(left, right, all.y=TRUE)

# When merging two data frames that do not have matching column names, we can
# use the by.x and by.y arguments to specify columns to merge on.

# Let's say we want to merge the first and left data frames by x0 and id. The
# by.x and by.y arguments specify which columns to use for merging.
first
left
merge(first, left, by.x="x0", by.y="id")

# Notice the merged data frame has an "x0" column, not an "id" column. And this
# of course is an inner join.

# Let's try to merge the second and left by x0 and id:
second
left
merge(second, left, by.x="x0", by.y="id") 

# There are no matches, so no merging happens. What if we don't specify columns
# to merge on?
merge(second, left)
# We get a "cartesian product"; every possible combination of rows.

# Dimension of result:
c(nrow(second)*nrow(left), ncol(second) + ncol(left))


# match(), %in%, intersect(), union(), setdiff(), setequal() --------------

# Sometimes we don't actually want to merge data but rather just find out which 
# records they have in common. We can use the match() and intersect() functions
# and the %in% operator. Let's once again create some data to demonstrate:

set.seed(111) # this ensures we get the same random numbers
alot <- round(runif(100,1,1000)) # 100 numbers from interval [1,1000]
alot
few <- round(runif(10,1,1000)) # 10 numbers from interval [1,1000]
few

# Are there any values in "few" also in "alot"?

# First let's use match(). The basic syntax of match() is match(x, table) where 
# x is the values to be matched and table is the values to be matched against. 
# This asks the question: "do any values in 'few' match values in 'alot', and if
# so, which indices do they match?"
match(few, alot)

# this says the 1st, 6th and 8th values of the "few" vector matches the 45th, 
# 69th and 77th values of the "alot" vector. 

# The %in% operator is perhaps more intutive. It returns a logical vector.
few %in% alot
# This says the 1st, 6th and 8th values of the "few" vector match values of the 
# "alot" vector. Notice it doesn't return the actual number. But we could do
# this:
few[few %in% alot]

# Using %in% means we can easily count the number of matches:
sum(few %in% alot)

# intersect() returns values in the first AND second vector:
intersect(few,alot)

# union() returns values in the first OR second vector:
union(few, alot)

# setdiff() returns the list of items in the first vector not in the 2nd vector:
setdiff(few, alot)

# setequal() asks if both vectors are equal and returns TRUE or FALSE:
setequal(few, alot)

# Note from documentation: "Each of union, intersect, setdiff and setequal will
# discard any duplicated values in the arguments"

# Reshaping Data ----------------------------------------------------------

# It's often helpful to think of data as "wide" or "long". When there are 
# multiple occurrences of values for a single observation in one row, the data 
# is said to be wide. When there are multiple occurrences of values for a single
# observation in multiple rows, the data is said to be long.

# Examples:
wide <- data.frame(name=c("Clay","Garrett","Addison"), 
                   test1=c(78, 93, 90), 
                   test2=c(87, 91, 97),
                   test3=c(88, 99, 91))
wide
long <- data.frame(name=rep(c("Clay","Garrett","Addison"),each=3),
                   test=rep(1:3, 3),
                   score=c(78, 87, 88, 93, 91, 99, 90, 97, 91))
long

##############
# wide to long
##############

# Many R functions require data in "long" format in order to perform 
# calculations on, or create graphs of, the data. Therefore it's important to
# know how to reshape data from wide to long. A very popular package for this
# task is the reshape2 package. If you don't already have it, please install it:
# install.packages("reshape2)
library(reshape2)

# The star function of the reshape2 package is melt(). It basically "melts" wide
# data into long format. The basic syntax is melt(data, id.vars, measure.vars), 
# where "data" is your data frame, "id.vars" are the ID variables (ie, variables
# that will still have their own column after reshaping) and "measure.vars" are 
# the variables that are getting "melted". Column headers of the "measure.vars" 
# become a single variable in the melted data frame as does the values under 
# those column headers. This is best explained with an example.

# To make our "wide" data frame long
wide
melt(wide, id.vars = "name", measure.vars = c("test1","test2","test3"))

# Notice the "test" column headers in wide are now in a column called 
# "variable", and the values under the "test" columns in wide are now in a 
# single column called "value". We can provide our own names for those columns 
# using the optional "variable.name" and "value.name" arguments, like so:

melt(wide, id.vars = "name", measure.vars = c("test1","test2","test3"),
     variable.name = "test", value.name="score")

# It should be noted that you can melt a data frame without explicitly using the
# measure.vars argument. If you leave it blank, melt will use all the variables
# not named in the id.vars argument:
melt(wide, "name")

# As another example, consider the airquality dataset that comes with R.
head(airquality)

# Notice there are multiple measurements on each day in a single row. Hence we 
# can think of this data as "wide". To make it "long" we require that each 
# measurement have its own row with a column to identify which measurement it
# is. We can do this with melt as follows:
aqLong <- melt(airquality, id.vars=c("Month", "Day"),
               variable.name = "Measurement", value.name="Reading")
head(aqLong)

# Data in long format makes summaries like this easy.
with(aqLong, tapply(Reading, list(Month, Measurement), mean,na.rm=TRUE))

# There is a reshape() function that comes with base R, but I find melt much 
# easier to use. Here is how to use reshape() to do what we just did with
# melt(). Notice we need to use three extra arguments.
aqLong2 <- reshape(airquality, idvar=c("Month","Day"), times=names(airquality)[1:4],
                   timevar = "Measurement", v.names="Reading",
                   varying = list(names(airquality)[1:4]), direction="long")
head(aqLong2)
rm(aqLong2)

# Let's subset and reshape the election data to long format to facilitate a bar 
# graph. 

names(electionData)[1] <- "State"  
edSub <- subset(electionData, select=c("State", "Obama Democratic", "Romney Republican", "Elec Vote D"))
edSub$Winner <- ifelse(is.na(edSub$"Elec Vote D"),"Romney","Obama")
edSub$"Elec Vote D" <- NULL
edSub <- melt(edSub, id.vars = c("State","Winner"), value.name="Votes",
              variable.name="Candidate")
head(edSub)

library(ggplot2)
library(scales) # for the comma function
ggplot(edSub, aes(x=State, y=Votes, group=Candidate, fill=Candidate)) + 
  geom_bar(position="dodge", stat="identity") + facet_wrap(~Winner) +
  scale_fill_manual(values=c("blue","red")) + scale_y_continuous(labels=comma) +
  coord_flip()

##############
# long to wide
##############


# The dcast() function can reshape a long data frame to wide. First we'll 
# demonstrate and then explain. 

# Let's reshape our aqLong data frame back to its original wide format.
aqOrig <- dcast(aqLong, Month + Day ~ Measurement, value.var = "Reading")
head(aqOrig)

# A good way to think of the dcast() function is to imagine it creating a matrix
# from the melted (long) data. The formula specifes the rows and columns where
# the LHS is the "rows" and the RHS is the "columns"; the value.var specifies
# what goes into the interior of the matrix.

# We can also use the dcast function to calculate summaries such as means by 
# supplying an aggregation function as the third argument. For example, we can
# find the mean measurement by month:
dcast(aqLong, Month ~ Measurement, mean, na.rm=TRUE, 
      value.var = "Reading")
# Notice the result is a data frame

# We can also make our own function to count the number of missing measurements
# by month:
dcast(aqLong, Month ~ Measurement, function(x)sum(is.na(x)), 
      value.var = "Reading")

# See the examples for help(cast) for more complex examples.


# tidyr -------------------------------------------------------------------

# Tidy data is a concept put forth in Hadley Wickham's 2014 paper, Tidy Data 
# (http://www.jstatsoft.org/v59/i10/). To quote the abstract: "Tidy datasets are
# easy to manipulate, model and visualize, and have a specific structure: each
# variable is a column, each observation is a row, and each type of
# observational unit is a table."

# Hadley created a package called tidyr to help tidy R data frames. Among other 
# things it can be used to reshape data. The two main functions are gather() and
# spread().

# install.packages("tidyr")
library(tidyr)

# gather ------------------------------------------------------------------

# gather is sort of like melt. It can make a wide data set long.

# Documentation description: Gather columns into key-value pairs.

# Syntax: gather(data, key, value, columns to gather) where data is your data 
# frame, key is the name of the new key column, value is the name of the new 
# value column, and the last part is names or numeric indices of columns to
# collapse (or to exclude from collapsing).

# Let's use gather() on the airquality data
aqLong2 <- gather(airquality, key = Measurement, value = Reading, -Month, -Day)

# Notice gather() handles character data differently than melt()!
str(aqLong$Measurement)
str(aqLong2$Measurement)

# Let's compare the syntax from melt and gather:

# melt(airquality, id.vars=c("Month", "Day"), 
#      variable.name = "Measurement", value.name="Reading")
# 
# gather(airquality, key = Measurement, value = Reading, -Month, -Day)

# The big difference is that in melt you identify the id.vars, the columns that 
# will remain in the long data set after conversion from wide. In the gather 
# function, you indicate which columns are being "gathered", either explicitly
# or by not excluding them.

# We see that the key and value arguments in gather correspond to the 
# variable.name and value.name arguments in melt. We also don't need to quote
# variable names in gather.


# spread ------------------------------------------------------------------

# spread is sort of like dcast. It can make a long data set wide.

# Documentation description: Spread a key-value pair across multiple columns.

# Basic syntax: spread(data, key, value) where data is a data frame, key is name
# of the column with the (unique) values you want turned into column headers,
# and value is the name of the column that has the values you want placed under
# your new column headers.

# Let's use spread() on the aqLong2 data
head(aqLong2)

# I want the unique values of Measurement to become column headers. I want the
# corresponding values in the Reading column to go under the new column headers.

aqOrig2 <- spread(aqLong2, key = Measurement, value = Reading)


# Does this return the same wide data frame as dcast()? Not quite.
names(aqOrig)
names(aqOrig2)

# The last two columns are swapped because dcast had to deal with Measure as a 
# factor (and it's associated ordering of levels) whereas spread had to deal 
# with Measure as a character vector and thus determined order of columsn
# alphabetically.

# Let's compare the syntax from dcast and spread:

# dcast(aqLong, Month + Day ~ Measurement, value.var = "Reading")
# 
# spread(aqLong2, key = Measurement, value = Reading)

# The big difference is that spread() doesn't require a formula. You just
# indicate the column(s) you want to "spread" out.

# We also see that the value argument in spread() corresponds to the value.var 
# argument in dcast(). And spread() doesn't require quoting variable names.

# Another major difference is that dcast() allows you to supply an aggregation 
# function for generating summary statistics. spread() is just spreading out
# key-value pairs across multiple columns.


# tidyr helper functions --------------------------------------------------

# tidyr includes a few helper functions. One that I really like is
# extract_numeric().

# This uses a regular expression to strip all non-numeric characters from a 
# string and then coerces the result to a number. This strips all non-numeric
# characters from a string and then coerces the result to a number.

extract_numeric("$1,200.34")
extract_numeric("-2%")

# Let's generate some dollar amounts
money <- dollar(round(runif(100,100,200),2)) # dollar() function from scales package
money
typeof(money)
extract_numeric(money)
typeof(extract_numeric(money))

# The heuristic is not perfect - it won't fail for things that clearly aren't
# numbers
extract_numeric("12abc34")

# Another helper function that may come in handy is separate().

# Given either regular expression or a vector of character positions, separate()
# turns a single character column into multiple columns. The default separation
# value is a regular expression that matches any sequence of non-alphanumeric
# values.

df <- data.frame(x = c("a.b", "a.d", "b.c"))
df
# split column x into two new columns called A and B
separate(df, x, c("A", "B"))

# Example: separate() can be useful for splitting times into components.

# Create a place holder data frame
dat <- data.frame(i=1:10, time=character(10), stringsAsFactors = F)

# loop through 10 iterations of logging the system time
for(i in 1:10){
  dat[i,2] <- format(Sys.time(), format = "%H:%M:%OS3") 
  # %OS3 = fractional seconds to 3 places; see ?strptime
  Sys.sleep(0.01) # delay 0.01 seconds
}
dat
separate(dat, time, c("H","M","S","FS"))



# save data for next set of lecture notes
save(list=c("electionData", "weather", "arrests", "allStocks"), file="../data/datasets_L05.Rda")
