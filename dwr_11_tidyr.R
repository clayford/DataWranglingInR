#' ---
#' title: "Data Wrangling in R: tidyr"
#' author: "Clay Ford"
#' date: "Spring 2015"
#' output: pdf_document
#' ---

load("../data/datasets_L07.Rda")

# install.packages("tidyr")
library(tidyr)


# tidy data ---------------------------------------------------------------

# Tidy data is a concept put forth in Hadley Wickham's 2014 paper, Tidy Data 
# (http://www.jstatsoft.org/v59/i10/). To quote the abstract: "Tidy datasets are
# easy to manipulate, model and visualize, and have a specific structure: each
# variable is a column, each observation is a row, and each type of
# observational unit is a table."

# Hadley created a package called tidyr to help tidy R data frames. In this 
# lecture we will explore the tidyr package and see how we can use it to "tidy"
# data.

# As R packages go, tidyr is quite small, currently consisting of just 11
# functions. The two main functions are gather() and spread().


# gather ------------------------------------------------------------------
# Gather columns into key-value pairs. (aka, convert wide to long)
# key = former column names
# value = former cells

# Syntax: gather(data, key, value, columns to gather) where data is your data 
# frame, key is the name of the new key column, value is the name of the new 
# value column, and the last part is names or numeric indeces of columns to
# collapse.

# Let's make some fake data on three stocks: X, Y and Z.
stocks <- data.frame(
  time = as.Date('2015-01-01') + 0:9, # 10 dates: 2015-01-01 - 2015-01-10
  X = round(rnorm(10, 15, 1),2),
  Y = round(rnorm(10, 20, 2),2),
  Z = round(rnorm(10, 30, 4),2)
)
stocks

# Take data frame stocks, make new variables called stock and price, gathering 
# all but the time column (ie, X,Y,Z). The former column names X, Y and Z become
# the values in the stock column and the former values of X, Y and Z become the
# values in the price column.
gather(stocks, stock, price, -time)

# This is basically reshaping data and can be done with the melt function in
# reshape2
library(reshape2)
melt(stocks, id.vars = "time", variable.name = "stock", value.name = "price")

# Use gather on the popVa data. Let's tidy the data such that there is a
# column indicating the census count and a column for population.
popVaT <- gather(popVa, census, pop, c(rescen42010:respop72012))
head(popVaT[order(popVaT$city),])
# dimensions before gathering
dim(popVa)
# dimensions after gathering
dim(popVaT)

# Use gather() on Anscombe's data that comes with R. (See 
# http://en.wikipedia.org/wiki/Anscombe%27s_quartet for more information.)

anscombe
# x1 goes with y1, x2 with y2, etc.

# I'd like to tidy up the data set such that there is one column for each 
# variable and each observation is a row. There are three variables: x, y, and 
# group. A single observation is an x,y pair with group indicator (1,2,3 or 4).

# like this:
#   group  x     y
#       1 10  8.04
#       1  8  6.95
#       1 13  7.58
# ....

# gather just the x columns
tmpx <- gather(anscombe[,1:4], group, x)
head(tmpx)
# gather just the y columns
tmpy <- gather(anscombe[,-c(1:4)], group, y)
head(tmpy)
# drop the group column in y since x already has it.
tmpy$group <- NULL
# now combine tmpx and tmpy into a single data frame
anscombeT <- cbind(tmpx, tmpy)
# finally remove the x from the group column so it's just numbers and convert to
# factor
anscombeT$group <- factor(sub("x","",anscombeT$group))
# the tidy data set:
anscombeT

# The tidy dataset is very easy to work with for aggregation and plotting purposes.

# Summary statistics by group:
library(dplyr)
anscombeT %>% group_by(group) %>% 
  summarise(meanx=mean(x), meany=round(mean(y),2),
            sdx=round(sd(x),2), sdy=round(sd(y),2),
            gCorr=round(cor(x,y),2))
# linear regression by group
for(i in 1:4){
  print(round(coef(lm(y ~ x, data=anscombeT, subset= group==i)),2))
}
# scatterplots by group
library(ggplot2)
ggplot(anscombeT, aes(x,y)) + geom_point() + 
  geom_smooth(method="lm", se=F) + facet_wrap(~ group) +
  ggtitle("Anscombe's Quartet")


# spread ------------------------------------------------------------------ 
# Spread a key-value pair across multiple columns. In other words, generate
# multiple columns from two columns. (aka, convert long to wide)

# Syntax: spread(data, key, value) where data is your data frame, key is the
# column to use to create keys, and value is the column to use for values.

# Let's "gather" the X, Y, Z columns in stocks and save:
stocksL <- gather(stocks, stock, price, -time)
head(stocksL) # notice the data is in "long" format

# Now use spread() to convert stocksL to wide; the values of stock become
# variables with values of price. This reverses the effect of gather().
spread(stocksL, stock, price)

# We could also set values of time as new variables that have values of price in
# their columns.
spread(stocksL, time, price)

# doing the same with reshape2 package requires the dcast function:
dcast(stocksL, time ~ stock, value.var = "price")
dcast(stocksL, stock ~ time, value.var = "price")


# We can reverse what we did on our popVa data frame using spread.
popVa <- spread(popVaT, census, pop)
head(popVa)


# tidyr helper functions --------------------------------------------------

# The tidyr package also includes a few handy helper functions. Let's take a
# look at each.

# expand ------------------------------------------------------------------

# Expand data frame to include all combinations of levels

# sort of like the expand.grid() function in base R.
head(mtcars)
# get all combinations of "vs" and "cyl"
expand(mtcars, vs, cyl)
# same as this call to expand.grid()
with(mtcars, expand.grid(vs=levels(factor(vs)),cyl=levels(factor(cyl))))

# another example:
df <- data.frame(a = c(1, 2, 5), b = c(3, 5, 3), c = c(1, 2, 3))
df

# works on the entire data frame
expand(df)
# doing the same with expand.grid
with(df, expand.grid(a=a, b=unique(b), c=c))


# seq_range ---------------------------------------------------------------

# Create an evenly spaced sequence of values from highest to lowest.
summary(mtcars$mpg)
seq_range(mtcars$mpg, 2)
seq_range(mtcars$mpg, 3)
seq_range(mtcars$mpg, 4)
seq_range(mtcars$mpg, 5)

# seq_range is not quite the same as pretty(), a function in base R:
seq_range(mtcars$mpg, 5)
pretty(mtcars$mpg, 5) # makes nice "pretty" intervals

# We can use expand with seq_range to get combinations of a factor with a 
# continuous variable.
expand(mtcars, cyl, mpg = seq_range(mpg, 5))


# extract_numeric ---------------------------------------------------------

# This uses a regular expression to strip all non-numeric character from a
# string and then coerces the result to a number.

extract_numeric("$1,200.34")
extract_numeric("-2%")

money <- paste0("$",sprintf("%.2f", round(runif(100,100,200),2))) 
money
extract_numeric(money)
class(extract_numeric(money))


# The heuristic is not perfect - it won't fail for things that clearly aren't
# numbers
extract_numeric("12abc34")


# separate ----------------------------------------------------------------
# Separate one column into multiple columns.

# Given either regular expression or a vector of character positions, separate()
# turns a single character column into multiple columns. The default separation
# value is a regular expression that matches any sequence of non-alphanumeric
# values.

df <- data.frame(x = c("a.b", "a.d", "b.c"))
df
# split column x into two new columns called A and B
separate(df, x, c("A", "B"))


# If every row doesn't split into the same number of pieces, use
# the extra argument to control what happens
df <- data.frame(x = c("a", "a b", "a b c", NA))
df
# merge "b" and "c" into a single element in column b
separate(df, x, c("y", "z"), extra = "merge")
# drop c
separate(df, x, c("y", "z"), extra = "drop")

# If only want to split specified number of times use extra = "merge". For
# example in the next data frame I only want to split on the first colon:
df <- data.frame(x = c("x: 123", "y: error: 7"))
df
separate(df, x, c("key", "value"), ": ", extra = "merge")


# unite -------------------------------------------------------------------
# Convenience function to paste together multiple columns into one.

# make a new variable called "vs_am" that unites the vs and am variables
head(mtcars)
unite(head(mtcars), vs_am, vs, am)

# Separate is the complement of unite
unite_cars <- unite(head(mtcars), vs_am, vs, am) 
str(unite_cars)
separate(unite_cars, vs_am, c("vs", "am"))


# unnest ------------------------------------------------------------------
# Unnest a list column.
# 
# If you have a list-column, this makes each element of the list its own row.
df <- data.frame(
  x = 1:3,
  y = c("a", "d,e,f", "g,h"),
  stringsAsFactors = FALSE
)
df
str(df)

# create a new column called y that's a list
df2 <- transform(df,y = strsplit(y, ","))
df2
str(df2)
# Notice y is a list of 3

# Now unnest y so that it's one column in the data frame
unnest(df2, y)
str(unnest(df2, y))

# unnest also works on lists alone
y  <- strsplit(df$y, ",")
y
unnest(y) # turns into a data frame

# same as:
data.frame(x=unlist(y))

# a little more elaborate
# select the first two rows of each list element
my_list <- lapply(split(subset(iris, select = -Species), iris$Species), "[", 1:2, )
my_list

# "[", 1:2,
`[`(iris,1:2,)

unnest(my_list)
# add column to indicate species
unnest(my_list, Species)


# extract -----------------------------------------------------------------

# Extract one column into one or more columns.

# It helps to know a little about regular expressions to get the most out of
# this function. This is the example in the help pages:

(df <- data.frame(x = c("a.b", "a.d", "b.c")))
# pull out the stuff before the period and make a new column called "A"
extract(df, x, "A")
extract(df, x, c("A", "B"), "([[:alnum:]]+)\\.([[:alnum:]]+)")
extract(df, x, c("A", "B"), "([[:alnum:]]+)\\.([[:alnum:]]+)")

# doing same thing using strsplit and sapply
tmp <- strsplit(as.character(df$x), ".", fixed = TRUE)
tmp
data.frame(A=sapply(tmp, function(x)x[1]))

data.frame(A=sapply(tmp, function(x)x[1]),
           B=sapply(tmp, function(x)x[2]))


# Extended example --------------------------------------------------------

# data from ProQuest
# Table 1253: Arts, Entertainment, And Recreation Services--Estimated Revenue:
# 2005 To 2012 [By Industry, Selected Years] Source: Bureau of Census. Last
# Updated: Feb. 2014 Edition: 2014

tab1253 <- read.csv("../data/table1253.csv", stringsAsFactors=FALSE)
# get indices for rows where Industry begins with capital letter
ind <- grep("^[A-Z]", tab1253$Industry) 
dat <- tab1253[ind,]
dat <- subset(dat, select=-2)
dat
# This dataset has three variables: industry, year, revenue. We need to gather
# the non-variable columns into a two-column key-value pair. In this case the
# non-variable columns are the columns with a year header.

# create two new columns called year and revenue comprised of all the columns 
# except Industry. The column headers of the gathered columns become the values
# under year, the values of the gathered columns become the values under revenue.

datTidy <- gather(dat, year, revenue, -Industry)
head(datTidy)
# clean up year and make revenue numeric
datTidy$year <- factor(extract_numeric(datTidy$year))
datTidy$revenue <- extract_numeric(datTidy$revenue)
head(datTidy)

library(ggplot2)
library(scales)
ggplot(datTidy, aes(x=year,y=revenue, group=Industry, color=Industry)) +
  geom_line() + scale_y_continuous(labels=dollar) +
  ggtitle("Estimated Revenue over time (millions of dollars)")






