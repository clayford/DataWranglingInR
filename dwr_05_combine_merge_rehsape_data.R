#' ---
#' title: "Data Wrangling in R: Combining, Merging and Reshaping Data"
#' author: "Clay Ford"
#' date: "Spring 2015"
#' output: pdf_document
#' ---


# load data from last lecture
load("../data/datasets_L04.Rda")



# Combining Data Frames ---------------------------------------------------

# Sometimes we have multiple data frames we want to combine. There are typically
# three ways to do this: (1) place side-by-side, (2) stack on top of each other,
# or (3) merge together based on a common variable.

# Let's generate some fake data to illustrate combining data frames by stacking.

first <- data.frame(x0=1:5,
                    x1=rnorm(5),
                    x2=sample(c("M","F"),5, replace=T))
first
second <- data.frame(x0=6:10,
                     x1=rnorm(5),
                     x2=sample(c("M","F"),5, replace=T))
second
third <- data.frame(x4=rbinom(5,4,0.5),
                    x5=sample(letters,5, replace=T))
third

# We can use the rbind() function to stack data frames. Make sure the number of 
# columns match. Also, the names and classes of values being joined must match.
# Here we stack the first, the second and then third:
rbind(first, second, first)

# Use the cbind function to combine data frames side-by-side:
cbind(second,third)

# WARNING: cbind does not require matching heights; if one data frame is shorter
# it will recycle it. Notice below the third data frame is recycled.
cbind(rbind(first,second),third)


# Remember the allStocks data? This is a list containing 7 data frames of stock
# data for 7 different companies.
names(allStocks)

# We can use rbind to combine these into one data frame. The thing is we have to
# call rbind repeatedly since there are so many data frames. 
# rbind(allStocks$bbby.csv, allStocks$flws.csv, allStocks$foxa.csv,...)

# A useful function for this type of task in R is the do.call() function. This
# function allows you to call any R function, but instead of writing out the
# arguments one by one, you can use a list to hold the arguments of the
# function. Since allStocks is a list of data frames, and rbind takes data
# frames as arguments, we can simply pass allStocks to rbind via the do.call
# function.
allStocks <- do.call("rbind", allStocks)
str(allStocks)
# Let's go ahead and fix the Date column name:
names(allStocks)[1] <- "Date"

# Let's look at the first few:
head(allStocks)

# We see that R very kindly created row names for us that identifies the source 
# data frame for each row! We can exploit that information to create a variable 
# that indicates which record belongs to which company. In other words, we can 
# add a column called "company" that will list "BBBY", "FLWS", etc. for their
# respective rows. We will do this in a later lecture.

# Merging Data ------------------------------------------------------------

# When we wish to join two data sets together based on common variables, we use
# the merge() function.

# Let's create some more fake data to illustrate:
fourth <- data.frame(y1=1:5,
                     y2=rnorm(5,100,5))
fourth
fifth <- data.frame(y1=rep(1:4,each=3),
                    z2=sample(c("Y","N"),12, replace=T))
fifth

# data sets fourth and fifth have columns y1 in common. Let's merge them
# together:
merge(fourth, fifth)

# Notice y2 from the fourth data frame is recycled to match up with multiple y1
# in the fifth data frame.

# Let's say we want to merge the first and fourth data frames by x0 and y1. The
# by.x and by.y arguments specify which columns to use for merging.
first
fourth
merge(first, fourth, by.x="x0", by.y="y1")

# Let's try to merge the second and fourth by x0 and y1:
second
fourth
merge(second, fourth, by.x="x0", by.y="y1") 

# No matches. What if we don't specify columns to merge on? 
merge(second, fourth)
# We get a cartesian product; every possible combination of rows.
# dimension of result:
c(nrow(second)*nrow(fourth), ncol(second) + ncol(fourth))

# Let's try merging the first and fifth data frames using the x0 and y1 columns.
first
fifth
merge(first, fifth, by.x="x0", by.y="y1")
# Notice the x0 = 5 record from first is dropped. To keep it specify all=TRUE:
merge(first, fifth, by.x="x0", by.y="y1", all=T)

# Sometimes we don't actually want to merge data but rather just find out which 
# records they have in common. We can use the match() and intersect() functions
# and the %in% operator. Let's once again create some data to demonstrate:

set.seed(111) # this ensures we get the same random numbers
alot <- round(runif(100,1,1000))
alot
set.seed(999)
few <- round(runif(10,1,1000))
few

# Are there any values in y also in x?

# First let's use match(). The basic syntax of match() is match(x, table) where
# x is the values to be matched and table is the values to be matched against.
match(few, alot)
# this says the 3rd and 4th values of the "few" vector matches the 79th and 92nd
# values of the "alot" vector.

# The %in% operator is perhaps more intutive. It returns a logical vector.
few %in% alot
# This says the 3rd and 4th values of the "few" vector matches values of the 
# "alot" vector. Notice it doesn't locate the match. How we can easily calculate
# the number of matches.
sum(few %in% alot)

# intersect returns the matching values between two vectors:
intersect(few,alot)



# Reshaping Data ----------------------------------------------------------

# It's often helpful to think of data as "wide" or "long". When there are 
# multiple occurrences of values for a single observation in one row, the data 
# is said to be wide. When there are multiple occurrences of values for a single
# observation in multiple rows, the data is said to be long.

# Examples:
wide <- data.frame(name=c("Clay","Garrett"),score1=c(78, 93), score2=c(87, 91))
wide
long <- data.frame(name=c("Clay","Clay","Garrett","Garrett"),
                   score=c(78, 87, 93, 91), test=c(1,1,2,2))
long

# Many R functions require data in "long" format in order to perform 
# calculations on the data. Therefore it's important to know how to reshape data
# from wide to long. A very popular package for this task is the reshape2
# package. If you don't already have it, please install it:
# install.packages("rehsape2)
library(reshape2)

# The star function of the reshape2 package is melt. It basically "melts" wide 
# data into long format. As an example, consider the airquality dataset that
# comes with R.
head(airquality)

# Notice there are multiple measurements on each day in a single row. Hence we 
# can think of this data as "wide". To make it "long" we require that each 
# measurement have its own row with a column to identify which measurement it
# is. We can do this with melt as follows:
aqLong <- melt(airquality, id.vars=c("Month", "Day"))
head(aqLong)

# The id.vars argument tells melt which columns are the ID variables. You can 
# think of the ID variables as those variables that will still have their own
# column after reshaping.

# Notice there are two new variables: variable and value. The variable column 
# contains the names of the column headers that were in the wide data. The value
# column contains the values for the respective variable. We can name these when
# using melt as follows:
aqLong <- melt(airquality, id.vars=c("Month", "Day"),
               variable.name = "Measurement", value.name="Reading")
head(aqLong)

# There is a reshape() function that comes with base R, but I find melt much 
# easier to use. Here is how to use reshape() to do what we just did with
# melt(). Notice we need to use three extra arguments.
aqLong2 <- reshape(airquality, idvar=c("Month","Day"), times=names(airquality)[1:4],
                   timevar = "Measurement", v.names="Reading",
                   varying = list(names(airquality)[1:4]), direction="long")
head(aqLong2)
rm(aqLong2)

# The dcast() function can reshape a long dataset to wide. Let's reshape the
# airquality data back to its original wide format:
aqOrig <- dcast(aqLong, Month + Day ~ Measurement, value.var = "Reading")
head(aqOrig)

# A good way to think of the dcast() function is to imagine it creating a matrix
# from the melted (long) data. The formula specifes the rows and columns where
# the LHS is the "rows" and the RHS is the "columns"; the value.var specifies
# what goes into the interior of the matrix.

# We can also use the dcast function to calculate summaries such as means. For example,
# we can find the mean measurement by month:
dcast(aqLong, Month ~ Measurement, mean, na.rm=TRUE)

# We can make our own function to count the number of missing measurements by
# month:
dcast(aqLong, Month ~ Measurement, function(x)sum(is.na(x)))

# See the examples for help(cast) for more examples.


# save data for next set of lecture notes
save(list=c("electionData", "weather", "arrests", "allStocks"), file="../data/datasets_L05.Rda")
