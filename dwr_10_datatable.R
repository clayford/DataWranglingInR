#' ---
#' title: "Data Wrangling in R: data.table"
#' author: "Clay Ford"
#' date: "Spring 2015"
#' output: pdf_document
#' ---

load("../data/datasets_L07.Rda")


# The data.table package is very similar to dplyr in its mission: provide fast 
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

# Since it's also a data frame, it works with packages and functions that work
# with data frames. For example, aggregate() is a function for data frames:
aggregate(Volume ~ Stock, data=allStocksDT, mean)

# And since it's a data frame we can use the same base R functions. For example:
names(allStocksDT)
ncol(allStocksDT)
dim(allStocksDT)
str(allStocksDT)
levels(allStocksDT$Stock)

# Like dplyr's tbl_df() function, data.table has the effect of supressing the
# printing of entire data frames to the console.
allStocksDT

# It prints the first 5 and last 5 records and places a colon after the row
# number.

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
# calculate j grouped by by". If you're familiar with SQL it may be useful to
# think of i as WHERE, j as SELECT and by as GROUP BY.

# How to select rows:
allStocksDT[1:5,]
# Actually don't need the comma to just select rows (unlike data frames)
allStocksDT[1:5]
# Can use conditional selection:
allStocksDT[Close < 50]
allStocksDT[Close < 50 & Stock=="tfm"]

# Notice we didn't have to preface Close or Stock with allStocksDT$. Nice
# benefit of data.table.

# How to select columns:
allStocksDT[1:5,.(Open, High) ]
# Here we would still need the comma if we wanted to just select columns. (I
# included 1:5 in the i argument to limit console output.)

# What is the .() that wraps the column names? It's an alias to list().
allStocksDT[1:5,list(Open, High)] # same as previous

# What if we don't use .() or list()? You get a vector:
allStocksDT[1:5,c(Open, High)]

# When you use .() in j, the result is always a data.table.

# Going back to this: allStocksDT[2,3]. Can we make that work the way it works
# for a data frame? Yes, by setting the with argument to FALSE:
allStocksDT[2, 3, with=FALSE]

# But data.table brackets don't stop with selection. You can also compute on
# columns. For example, find the mean and std deviation of the Open price:
allStocksDT[,.(meanOpen = mean(Open), sdOpen = sd(Open))]

# You can also combine column selection with computation:
allStocksDT[,.(Open, meanOpen = mean(Open))]
# Notice the mean was "recycled" to fill the data table

# You can pretty much throw anything into j. The following graphs bbby volume
# over time:
allStocksDT[Stock == "bbby",plot(Date, Volume, type="l", main="bbby Volume")]

# Finally we can use the by argument to do calculations by group. Here we
# calculate mean and SD of Open by levels of Stock:
allStocksDT[,.(meanOpen = mean(Open), sdOpen = sd(Open)), by = .(Stock)]

# Notice the .() notation in the by argument. If you have one item in by, you
# can drop the .(). Probably not a bad idea to just keep it.

# We can also define groups in the by argument. For example, calculate the mean
# volume per month per stock:
allStocksDT[, .(meanVolume = mean(Volume)), by = .(Month = months(Date), Stock)]

# We defined a new grouping variable called Month and then used it as one of the
# by variable for which to calculate the means.

# We can use i to limit the calculation to a subset. Here we calculate the mean
# Volume for per month for bbby:
allStocksDT[Stock == "bbby", .(meanVolume = mean(Volume)), 
            by = .(Month = months(Date))]

# Chaining ----------------------------------------------------------------

# Recall how we chained operations together in dplyr using %>%. We can also
# chain operations in data.table.

# Calculate mean open price per stock then sort by mean in ascending order:
allStocksDT[,.(meanOpen = mean(Open)), by = .(Stock)][order(meanOpen)]

# Notice the "][". They need to be next to one another for chaining to work.

# Find the minimum and maximum stock price for each stock. 

# Recall the chaining we used in dplyr:
# allStocks %>% 
#   group_by(Stock) %>%
#   summarise(Min=min(Low), Max=max(High))

# We actually don't need to chain anything to do that in data.table:
allStocksDT[, .(Min=min(Low), Max=max(High)), .(Stock)]


# Find the largest change in Open and Close price for each stock. 

# Recall the chaining we used in dplyr:
# allStocks %>%
#   group_by(Stock) %>%
#   mutate(Change = Close - Open) %>%
#   summarise(LargestGain = max(Change), LargestLoss = min(Change))

allStocksDT[,.(Change = Close - Open), 
            .(Stock)][,.(LargestGain = max(Change),
                         LargestLoss = min(Change)),
                      .(Stock)]

# It works, but it's a little too concise for my taste.

# Back to computations on columns. What if you have a lot of columns? This can
# get tedious:
allStocksDT[,.(mean(Open), mean(High), mean(Low), mean(Close), mean(Volume)), by = .(Stock)]

# data.table provides the .SD symbol to help with this. SD = Subset Data. Of
# course it's only helpful if you're comfortable using the lapply function.
allStocksDT[,lapply(.SD, mean), by = .(Stock)]

# That calculated the mean for all columns (including the date!) except what was
# in the by argument. We can use the .SDcols argument to specify all columns
# except the Date and Volume columns:
allStocksDT[,lapply(.SD, mean), by = .(Stock), .SDcols = -c("Date","Volume")]


# := ----------------------------------------------------------------------

# So far everything we've done has been output to the console and not saved. We 
# could have saved our work the usual way with an assigment operator "<-". 
# However, data.table provides a convenient way to modify a data table without 
# using an assignment operator. The function is ":=" (read "colon equals"). It 
# updates or adds column(s) by reference. That is, it makes no copies of any
# part of memory at all. This can be very efficient for large data sets.

# Let's do some examples. 

# Create a column for day of trading:
names(allStocksDT)
allStocksDT[, Day := weekdays(Date)]
names(allStocksDT)

# Remove the column we created:
allStocksDT[, Day := NULL]
names(allStocksDT)

# We can also add/update multiple columns. Here we create a new column for Day
# and format the Volume column to have commas:
# install.packages("scales")
library(scales) # for comma function
allStocksDT[, c("Day", "Volume") := list(weekdays(Date), comma(Volume))]
allStocksDT

# Let's change back to the way it was by removing the Day column and converting
# Volume to integer:
# install.packages("tidyr")
library(tidyr) # for extract_numeric function
allStocksDT[, c("Day", "Volume") := list(NULL, extract_numeric(Volume))]
allStocksDT

# And now let's do what we did before another way!
allStocksDT[, `:=`(Day = weekdays(Date), Volume = comma(Volume))]
allStocksDT

# Here we're using := like a function, because it is a function! On a side note,
# just about everything is accomplished by functions in R. Even the "+" sign is
# a function:
2+4
`+`(2,4)

# Those brackets in data frames? Yep, functions:
`[`(allStocks,1:4,1:4)

# Again let's tidy up:
allStocksDT[, `:=`(Day = NULL, Volume = extract_numeric(Volume))]
allStocksDT

# we can combine := with i and j. Here we subset where month equals January,
# then calculate the total Volume per Stock.
allStocksDT[months(Date)=="January", Total := sum(Volume), by = .(Stock)]

# If we print the data table, we'll see NA for Total. That's because the head
# and tail of the data table do not display data from January.
allStocksDT

# To see the result, we need to show some data from January
allStocksDT[months(Date)=="January"]


# data.table Speed --------------------------------------------------------

# This is a good time to demonstrate data.table's speed. Let's generate a data
# frame with 1,000,000 rows.
DF <- data.frame(x=factor(sample(x = c("A","B","C"),size = 1e6, replace = T)),
           y = rnorm(1e6,100,10))
dim(DF)
print(object.size(DF), units = "Mb")

# Now lets find the mean of y for each level of x using aggregate():
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

# Remember the baseball example from the dplyr lecture? Here it is again with dplyr:
library(Lahman)

# Batting %>%
#   group_by(playerID) %>%
#   summarize(total = sum(G_batting)) %>%
#   arrange(desc(total)) %>%
#   head(5)

# Here's how we can do it with data.table and some chaining:
BattingDT <- data.table(Batting)
BattingDT[,.(total = sum(G_batting)), by = .(playerID)][head(order(total,decreasing = T),n=5)]
rm(BattingDT)


# Keys --------------------------------------------------------------------


# data.table allows us to create a "key" on a data table. The data.table 
# documentation refers to keys as "super-charged row names". It may help to also
# think of them as a factor. Let's see how to set a key and what we can do with
# it.

tables()
# Notice the key column is empty

setkey(allStocksDT, Stock)
tables()

# Now Stock is the key
allStocksDT

# The data table is now sorted automatically by Stock. Also notice we didn't
# have to use an assignment operator "<-" above.

# see just the bbby stocks
allStocksDT["bbby"]

# A key can consist of multiple columns.

head(airquality)
airqualityDT <- data.table(airquality)

# Make Month and Day the keys:
setkey(airqualityDT, Month, Day)
tables()
# see record for May 5
airqualityDT[.(5,2)]
# see record for June 21
airqualityDT[.(6,21)]

