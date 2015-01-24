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
                    x2=c("M","F","M","F","F"))
first
second <- data.frame(x0=6:10,
                     x1=rnorm(5),
                     x2=c("M","F","M","F","F"))
second
third <- data.frame(x4=c(3,3,1,3,2),
                    x5=c("e","g","v","b","z"))
third

# rbind()

# We can use the rbind() function to stack data frames. Make sure the number of 
# columns match. Also, the names and classes of values being joined must match.
# Here we stack the first, the second and then the first again:
rbind(first, second, first)
class(rbind(first, second, first)) # still a data frame

# works with vectors too:
rbind(1:3,4:6)
class(rbind(1:3,4:6)) # matrix

# cbind()

# Use the cbind function to combine data frames side-by-side:
cbind(first,third)
class(cbind(first,third))

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
# data frame for each row! We can exploit that information to create a variable 
# that indicates which record belongs to which company. In other words, we can 
# add a column called "company" that will list "BBBY", "FLWS", etc. for their
# respective rows. We will do this in a later lecture.

# Merging Data ------------------------------------------------------------

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
fourth <- data.frame(id=1:5,
                     y2=rnorm(5,100,5))
fourth
fifth <- data.frame(id=rep(1:4,each=3),
                    z2=sample(c("Y","N"),12, replace=TRUE))
fifth

# Data frames fourth and fifth have columns "id" in common. Let's merge them 
# together based on id:
merge(fourth, fifth)

# Notice y2 from the fourth data frame is recycled to match up with multiple id
# in the fifth data frame.

# Let's say we want to merge the first and fourth data frames by x0 and y1. The
# by.x and by.y arguments specify which columns to use for merging.
first
fourth
merge(first, fourth, by.x="x0", by.y="id")

# Notice the merged data frame has an "x0" column, not an "id" column. 

# Let's try to merge the second and fourth by x0 and id:
second
fourth
merge(second, fourth, by.x="x0", by.y="id") 

# There are no matches, so no merging happens. What if we don't specify columns
# to merge on?
merge(second, fourth)
# We get a cartesian product; every possible combination of rows.

# Dimension of result:
c(nrow(second)*nrow(fourth), ncol(second) + ncol(fourth))

# Let's try merging the first and fifth data frames using the x0 and y1 columns.
first
fifth
merge(first, fifth, by.x="x0", by.y="id")

# Notice the x0 = 5 record from first is dropped. To keep it specify all=TRUE:
merge(first, fifth, by.x="x0", by.y="id", all=T)

# match(), intersect(), and %in% 

# Sometimes we don't actually want to merge data but rather just find out which 
# records they have in common. We can use the match() and intersect() functions
# and the %in% operator. Let's once again create some data to demonstrate:

set.seed(111) # this ensures we get the same random numbers
alot <- round(runif(100,1,1000)) # 100 numbers from interval [1,1000]
alot
few <- round(runif(10,1,1000)) # 10 numbers from interval [1,1000]
few

# Are there any values in y also in x?

# First let's use match(). The basic syntax of match() is match(x, table) where 
# x is the values to be matched and table is the values to be matched against. 
# So basically we're asking the question: "do any values in 'few' match values in
# 'alot', and if so, which indices do they match?"
match(few, alot)
# this says the 1st, 6th and 8th values of the "few" vector matches the 45th,
# 69th and 77th values of the "alot" vector.

# The %in% operator is perhaps more intutive. It returns a logical vector.
few %in% alot
# This says the 1st, 6th and 8th values of the "few" vector match values of the 
# "alot" vector. Notice it doesn't locate the match. 

# Using %in% means we can easily count the number of matches:
sum(few %in% alot)

# intersect actually returns the matching values between two vectors:
intersect(few,alot)


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
                   test=rep(1:3, each=3),
                   score=c(78, 87, 88, 93, 91, 99, 90, 97, 91))
long

# Many R functions require data in "long" format in order to perform 
# calculations on, or create graphs of, the data. Therefore it's important to
# know how to reshape data from wide to long. A very popular package for this
# task is the reshape2 package. If you don't already have it, please install it:
# install.packages("rehsape2)
library(reshape2)

# The star function of the reshape2 package is melt. It basically "melts" wide 
# data into long format. The basic syntax is melt(data, id.vars, measure.vars), 
# where "data" is your data frame, "id.vars" are the ID variables (ie, variables
# that will still have their own column after reshaping) and "measure.vars" are 
# the variables that are getting "melted". Column headers of the "measure.vars" 
# become a single variable in the melted data frame as does the values under
# those column headers. This is best explained with an example. 

# To make our "wide" data frame long
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

# Now that the data is in long form, it's very easy to do something like this:
op <- par(mfrow=c(2,3))
for(i in 5:9){
  plot(Reading ~ Day, data=aqLong, 
       subset= Month==i & Measurement=="Temp", 
       type="l", main=paste("Month =",i))
}
par(op)

# or like this:
library(ggplot2)
ggplot(aqLong, aes(x=Day, y=Reading, color=Measurement)) + geom_line() +
  facet_wrap(~Month)


# There is a reshape() function that comes with base R, but I find melt much 
# easier to use. Here is how to use reshape() to do what we just did with
# melt(). Notice we need to use three extra arguments.
aqLong2 <- reshape(airquality, idvar=c("Month","Day"), times=names(airquality)[1:4],
                   timevar = "Measurement", v.names="Reading",
                   varying = list(names(airquality)[1:4]), direction="long")
head(aqLong2)
rm(aqLong2)

# Let's subset and reshape the election data to long format to facilitate a bar
# graph:

names(electionData)[1] <- "State"
edSub <- subset(electionData, select=c("State", "Obama Democratic", "Romney Republican", "Elec.Vote D"))
edSub$Winner <- ifelse(is.na(edSub$"Elec.Vote D"),"Romney","Obama")
edSub$"Elec.Vote D" <- NULL
edSub <- melt(edSub, id.vars = c("State","Winner"), value.name="Votes", variable.name="Candidate")
head(edSub)

library(scales) # for the comma function
ggplot(edSub, aes(x=State, y=Votes, group=Candidate, fill=Candidate)) + 
  geom_bar(position="dodge", stat="identity") + facet_wrap(~Winner) +
  scale_fill_manual(values=c("blue","red")) + scale_y_continuous(labels=comma) +
  coord_flip()

# The dcast() function can reshape a long data frame to wide. First we'll 
# demonstrate and then explain. Let's reshape our aqLong data frame back to its 
# original wide format.
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

# See the examples for help(cast) for more examples.


# save data for next set of lecture notes
save(list=c("electionData", "weather", "arrests", "allStocks"), file="../data/datasets_L05.Rda")
