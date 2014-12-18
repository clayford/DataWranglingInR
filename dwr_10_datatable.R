#' ---
#' title: "Data Wrangling in R: data.table"
#' author: "Clay Ford"
#' date: "Spring 2015"
#' output: pdf_document
#' ---

load("../data/datasets_L07.Rda")


# The data.table package is very similar to dplyr in it's mission: provide fast 
# aggregation of data using short, flexible syntax. I suppose you could say 
# data.table and dplyr compete with one another, though I think any competition 
# between them is ultimately friendly. The developers of each always seem very
# complimentary of the other. Which is "better"? That's up to you!

# I will admit I'm partial to dplyr only because I started using it before 
# data.table. I've been teaching myself data.table and it's not coming to me as 
# easily as dplyr did. I believe that says less about data.table and more about 
# me! Perhaps data.table will come faster to you. I'll try my best to give it a
# fair and thorough treatment below.

# install.packages("data.table")
library(data.table)

# Like dplyr, data.table is intended to work on data frames. You may recall that
# dplyr gave us the option to wrap a data frame with a "data frame tbl" using 
# the tbl_df() function, but it wasn't required. You can still use dplyr 
# functions on a data frame without it being wrapped by tbl_df(). data.table on 
# the other hand requires you to convert the data frame to a "data table" in
# order to use data.table functions.

# To create a data table, use the data.table function:
class(allStocks)
allStocksDT <- data.table(allStocks)
class(allStocksDT)

# so we see that allStocks is now a data table and a data frame.
is.data.frame(allStocksDT)
is.data.table(allStocksDT)

# Like dplyr's tbl_df() function, data.table has the effect of supressing the
# printing of entire data frames to the console.
allStocksDT

# It prints the first 5 and last 5 records and places a colon after the row
# number.


# And since it's a data frame we can use the same base R functions. For example:
names(allStocksDT)
ncol(allStocksDT)
dim(allStocksDT)
str(allStocksDT)
levels(allStocksDT$Stock)

# How about indexing? Does that work the same? Not quite.
# We can still extract, say, row 2:
allStocksDT[2,]

# But trying to extract 3rd column of row 2 produces this:
allStocksDT[2,3]

# What's going here? It turns out that data table uses indexing brackets much 
# differently than data frames. The basic arguments within brackets are not row
# and column numbers but rather "i", "j" and "by".

# The general form is thus DT[i, j, by] where DT is a data table, like 
# allStocksDT. In words, this translates to "Take DT, subset rows using i, then 
# calculate j grouped by by". If you're familiar with SQL, i is WHERE, j is
# SELECT and by GROUP BY.

# Some examples:

# Select row 2 and column 3
allStocksDT[2, High]

# can we make allStocksDT[2, 3] work? Yes, setting the with argument to FALSE:
allStocksDT[2, 3, with=FALSE]

# Select column 3 and return as a vector
allStocksDT[, High]

# Select column 3 and return as a data.table. Wrap in .(). When you use .() in
# j, the result is always a data.table.
allStocksDT[, .(High)]

# where rows have a date in January, select Date and Volume 
allStocksDT[months(Date)=="January", .(Date, Volume)]

# where rows have a date in January, calculate mean Volume by Stock 
allStocksDT[months(Date)=="January", .(meanVolume = mean(Volume)), by = Stock]

# See what we just did? We defined a new variable in j called meanVolume that is
# the mean volume of each stock for January.

# This is a good time to demonstrate data.table's speed. Let's generate a data
# frame with 1,000,000 rows.
DF <- data.frame(x=factor(sample(x = c("A","B","C"),size = 1e6, replace = T)),
           y = rnorm(1e6,100,10))
dim(DF)
print(object.size(DF), units = "Mb")

# Now lets find the mean of y for each level of x:
system.time(
  ans1 <- aggregate(y ~ x, data=DF, mean)
  )
ans1

# Now do the same with data.table:
DT <- data.table(DF)
system.time(
  ans2 <- DT[,.(y = mean(y)), by = x]
  )
ans2

# Considerably faster!

# maybe compare to dplyr with baseball example?

# example of chaining:
DT[,.(mt10 = mean(head(y,n=10))), by = x][order(-mt10)]

# topics to cover: setting key, chaining and joins