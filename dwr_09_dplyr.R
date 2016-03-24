#' ---
#' title: "Data Wrangling in R: dplyr"
#' author: "Clay Ford"
#' date: "Spring 2016"
#' output: pdf_document
#' ---

load("../data/datasets_L08.Rda")

# This lecture introduces two packages: plyr and dplyr.

# plyr is a package that supplies functions for splitting data into groups, 
# applying function to each group, and combining the results back together; 
# known as the split-apply-combine strategy.

# original journal article: http://www.jstatsoft.org/v40/i01
# plyr tutorial: http://plyr.had.co.nz/09-user/

# We'll only talk briefly about plyr and move on to dplyr, a sort of evolution 
# of plyr. dplyr is specifically for data frames and has a very powerful but 
# easy to use syntax for manipulating data. It's also very fast when working
# with "large" data frames.

# plyr --------------------------------------------------------------------

# install.packages("plyr")
library(plyr)

# Example of split-apply-combine
# split
temps <- split(weather$Max.TemperatureF, weather$Events) 
temps
# apply a function to each group
maxmeans <- sapply(temps, mean) 
maxmeans
# combine results
data.frame(event=names(maxmeans), meanMaxTemp=maxmeans, row.names = NULL) 

# Here's how you do the same with plyr
ddply(weather, "Events", summarize, meanMaxTemp=mean(Max.TemperatureF))

# Let's break that down:
# - dd in ddply means data frame in, data frame out
# - first argument: input data frame
# - second argument: grouping variable to split data frame by
# - third argument: function to apply to each group; summarize is a plyr function
# - fourth argument: argument passed to third function

# Note we can do (mostly) the same with aggregate()
aggregate(Max.TemperatureF ~ Events, data=weather, mean)

# So why use plyr?
# ddply can also do things like this:
ddply(weather, "Events", summarize, 
      meanMaxTemp=mean(Max.TemperatureF),
      medianMaxTemp=median(Max.TemperatureF),
      sdMaxTemp=sd(Max.TemperatureF),
      n=sum(!is.na(Max.TemperatureF)),
      seMaxTemp=sdMaxTemp/sqrt(n))

# This is the same as we did before, but notice we calculated more than one 
# summary. Also notice we used calculated summaries in the subsequent 
# calculation of seMaxTemp. We cannot do that with aggregate().


# dplyr -------------------------------------------------------------------

# dplyr focuses only on data frames. It is faster than plyr and easier to use
# (in my opinion). In addition it comes with a fantastic Introductory vignette
# in the documentation.

# Let's detach plyr to prevent conflicts with dplyr. (Actually I think they may
# play well together now, but historically they haven't.) The unload=TRUE
# argument unloads the package from memory; Otherwise, R removes the package
# from the search path but doesn't unload it.
detach("package:plyr", unload=TRUE) 

# now load dplyr
library(dplyr)

# We see that dplyr has functions with the same names of functions in the stats 
# and base packages. The message "The following objects are masked..." means we 
# have packages loaded with functions sharing the same name, and that when we
# use, say, the setdiff function, we'll be using the setdiff function in dplyr,
# not the setdiff function in the base package. To access the base setdiff
# function you need to specify base::setdiff().

# dplyr provides data manipulation verbs that work on a single data frame, a 
# sort of grammar of data wrangling. The dplyr philosophy is to have small 
# functions that each do one thing well. Some of the more commonly used verbs
# include:

# filter() - select a subset of the rows of a data frame

# slice() - select rows by position

# select() - select columns

# arrange() - reorder (sort) rows by columns

# rename() - rename variables (column headers)

# distinct()- return the unique values in a data frame

# mutate() - add new columns that are functions of existing columns

# transmute() - like mutate, but keeps only the new columns

# summarise() - summarize values

# top_n() - Select and order top n entries

# sample_n() - randomly sample fixed number of rows of a data frame

# sample_frac() - randomly sample fixed fraction of rows of a data frame

# group_by() - how to break a dataset down into groups of rows

# ungroup() - removing the grouping created in the previous function

# For all these functions, the first argument is a data frame. The subsequent
# arguments describe what to do with it, and you can refer to columns in the
# data frame directly without using $. And they all return a new data frame.

# dplyr also provides the ability to chain functions using the magrittr
# forward-pipe operator: %>%. Use Ctrl + Shift + M to quickly enter.

# These functions and many others are documented in RStudio's data wrangling
# cheat sheet: 
# http://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

# Let's give all these functions a spin! 

# Going back to our weather data.

# mean of Max.TemperatureF by Event
weather %>%  
  group_by(Events) %>% 
  summarise(meanTemp = mean(Max.TemperatureF))

# mean of Max.TemperatureF by Event, arranged in ascending order
weather %>% 
  group_by(Events) %>% 
  summarise(meanTemp = mean(Mean.TemperatureF)) %>% 
  arrange(meanTemp)

# mean of Max.TemperatureF by Event, arranged in ascending order for dates after
# May 31
weather %>% 
  filter(Date > "2013-05-31") %>%
  group_by(Events) %>% 
  summarise(meanTemp = mean(Mean.TemperatureF)) %>% 
  arrange(meanTemp)

# Mean number of cosponsors per sponsor:
SenateBills %>% 
  group_by(sponsor) %>% 
  summarise(meanSponsors = mean(cosponsors))

# Notice output is truncated; whereas base R defaults to outputting everything,
# dplyr defaults to just a few rows.

# One way to see all rows: use as.data.frame()
SenateBills %>% 
  group_by(sponsor) %>% 
  summarise(meanSponsors = mean(cosponsors)) %>% 
  as.data.frame()

# Or use print with n argument to specify number of rows
SenateBills %>% 
  group_by(sponsor) %>% 
  summarise(meanSponsors = mean(cosponsors)) %>% 
  print(n=20)

# bill submitted by VA senators, just show bill number
SenateBills %>% 
  filter(grepl("\\[VA\\]", sponsor)) %>% 
  select(bill, sponsor)

# Now why did that return all rows? No group_by() function! The group_py 
# function converts a data frame into a "tbl_df" class, which only prints a few
# rows when thrown to the console. More on this in a bit.

# Top 10 bills by number of cosponsors
SenateBills %>% 
  select(bill, sponsor, cosponsors) %>% 
  top_n(10, cosponsors)


# sample 20 rows from arrests data frame and just show ID, Sex, and Age
arrests %>% 
  sample_n(20) %>% 
  select(ID, Sex, Age)

# Everything we did above was just output to the console. To save our results,
# we need to use the assignment operator.

# add indicator to SenateBills that takes the value 1 if bill has any
# cosponsors, 0 otherwise, and update SenateBills.
SenateBills <- SenateBills %>% 
  mutate(cosponsorsI = as.numeric(cosponsors > 0)) 

table(SenateBills$cosponsorsI)

# We could have done this as well!

SenateBills %>% 
  mutate(cosponsorsI = as.numeric(cosponsors > 0)) -> SenateBills


# the %>% operator --------------------------------------------------------

# The %>% operator is the magrittr forward-pipe operator.
# help(`%>%`)

# We can use the %>% operator with base R functions

# http://blog.revolutionanalytics.com/2014/07/magrittr-simplifying-r-code-with-pipes.html

# The object on the left hand side is passed as the first argument to the
# function on the right hand side. For example, a common structure:
# 
# my.data %>% my.function = my.function(my.data) 

# my.data %>% my.function(arg=value) = my.function(my.data, arg=value)

# Let's compare the base R nested method with the magrittr method:

# nested functions
head(sort(allStocks$Volume, decreasing = TRUE))
# chained functions
allStocks$Volume %>% sort(decreasing=TRUE) %>% head()

# From assignment 3
trees <- read.csv("../data/139_treecores_rings.txt", na.strings = "-0.999")
# nested
head(tolower(trimws(as.character(trees$Condition.of.inner.core))))
# chained
trees$Condition.of.inner.core %>% 
  as.character() %>% 
  trimws() %>% 
  tolower() %>% 
  head()
rm(trees)

# nested functions
mean(is.na(arrests$Children[arrests$Sex=="Female"]))
# chained functions
arrests$Children %>% 
  `[`(arrests$Sex=="Female") %>% 
  is.na() %>% 
  mean()

# nested
paste0(round(prop.table(table(SenateBills$cosponsorsI)),2)*100,"%")
# chained
SenateBills$cosponsorsI %>% 
  table() %>% 
  prop.table() %>% 
  round(2) %>% 
  `*`(100) %>% 
  paste0("%")


# dplyr speed -------------------------------------------------------------

# This is a good time to demonstrate dplyr's speed. Let's generate a data
# frame with 30,000,000 rows.

DF <- data.frame(x = rep(c("A","B","C"), each = 1e7),
                 y = c(rnorm(1e7,100,4), rnorm(1e7,90,4), rnorm(1e7,80,4)))
dim(DF)
print(object.size(DF), units = "Mb")

# Now lets find the mean of y for each level of x using aggregate():
system.time(
  ans1 <- aggregate(y ~ x, data=DF, mean)
)
ans1


# How about dplyr?
system.time(
  ans2 <- DF %>%
    group_by(x) %>%
    summarise(y = mean(y))
)
ans2

# What about tapply()?
system.time(
  ans3 <- with(DF, tapply(y, x, mean))
)
ans3

rm(ans1, ans2, ans3, DF)

# More on the dplyr verbs -------------------------------------------------

# tbl_df() - wraps a local data frame. The main advantage to using a tbl_df over
# a regular data frame is the printing: tbl objects only print a few rows and 
# all the columns that fit on one screen, describing the rest of it as text. 
# This is not technically one of the dplyr "verbs", nor is it even required, but
# it can help you from blowing away your console by accidentally printing your
# entire data frame.

class(weather)
# Create a data frame tbl.
weather <- tbl_df(weather)
class(weather)
weather
# dplyr has something similar to str() called glimpse(), though str() still
# works on data frame tbl.
glimpse(weather)

# if you want to print the entire data frame to the console, you can use
# as.data.frame(weather)

# filter() - select a subset of the rows of a data frame; works much like
# subset()

# days it snowed
filter(weather, snow==1) 

# compare to base R brackets (notice tbl_df printing still in effect)
weather[weather$snow==1,]

# Of course base R brackets allow this:
weather[weather$snow==1,1:3]

# with dplyr...
weather %>% 
  filter(snow==1) %>% 
  select(1:3)

# another example...
# Max temp > 90 and max humidity > 90
filter(weather, Max.TemperatureF > 90 & Max.Humidity > 90) 

# compare to
weather[weather$Max.TemperatureF > 90 & weather$Max.Humidity > 90, ]

# slice() - select rows by position
slice(weather,1:10)

# compare to:
weather[1:10,]

# arrange() - reorder (sort) rows by columns; much easier, in my opinion, than
# using order() with subsetting brackets

# sort popVa data frame by rescen42010; notice we can use tbl_df() on the fly
arrange(tbl_df(popVa), rescen42010)

# versus base R
popVa[order(popVa$rescen42010),] %>% tbl_df()

# with the desc() helper function
arrange(tbl_df(popVa), desc(rescen42010)) # uses desc() helper function

# versus base R
popVa[order(popVa$rescen42010, decreasing = TRUE),] %>% tbl_df()

# sort data frame by more than one variable
arrange(weather, Max.TemperatureF, Max.Dew.PointF)

# versus base R
weather[order(weather$Max.TemperatureF, weather$Max.Dew.PointF),]

# It's important to note that the weather data frame itself has not changed. The
# sort order does not change unless we assign the result!

# select() - select columns
select(weather, Max.TemperatureF, Min.TemperatureF, Temp.Range)
select(tbl_df(electionData), 7:10)

# compare to base R
weather[,c("Max.TemperatureF", "Min.TemperatureF", "Temp.Range")]
electionData[,7:10] %>% tbl_df()

# That's nice, but we can use ":" with the actual variable names.
select(tbl_df(allStocks), Open:Close)
select(tbl_df(allStocks), Open:Close, -Low)
# If variable has spaces, surround it with back ticks: `
select(tbl_df(electionData), `Obama Democratic`:`Stein Green`)
# Can also use - to drop variables
select(tbl_df(electionData), `Obama Democratic`:`Stein Green`, -`0 Independent`)

# in base R there really is no comparison; you have to type
# tbl_df(allStocks[,c("Open", "High", "Low", "Close")])
# tbl_df(allStocks[,c("Open", "High", "Close")])

# dplyr has a number of helper functions to use with select:

# - starts_with(x, ignore.case = TRUE): names starts with x

# - ends_with(x, ignore.case = TRUE): names ends in x

# - contains(x, ignore.case = TRUE): selects all variables whose name contains x

# - matches(x, ignore.case = TRUE): selects all variables whose name matches the
# regular expression x

# - num_range("x", 1:5, width = 2): selects all variables (numerically) from x01
# to x05.

# - one_of("x", "y", "z"): selects variables provided in a character vector.

# - everything(): selects all variables.

# Examples
select(weather, starts_with("Max")) 
select(weather, ends_with("F")) 
select(weather, contains("Dew")) 
select(weather, matches("^[^.]+$")) 
select(weather, -matches("^[^.]+$")) 
select(popVa, num_range("respop", 72010:72012, width=5)) 


# rename() - rename variables (column headers); new name = old name

weather <- rename(weather, Snowed = snow)
# using rename() when variable name has spaces (use backticks)
electionData <- rename(electionData, MOV = `Margin of Victory Votes`)

# compare to base R
# names(weather)[30] <- "Snowed"

# or
# weather$Snowed <- weather$snow
# weather$snow <- NULL


# distinct()- return the unique values in a data frame; often used with select()
distinct(select(arrests, Children))

# or with %>% 
arrests %>% select(Children) %>% distinct()

# in base R
unique(arrests$Children) # vector
data.frame(Children=unique(arrests$Children))


# mutate() - add new columns that are functions of existing columns; 
# new columns can refer to other columns that you just created.
weather <- mutate(weather, 
                  Dew.Point.Range = Max.Dew.PointF - Min.DewpointF,
                  Humidity.Range = Max.Humidity - Min.Humidity,
                  DH.Range.Ratio = Dew.Point.Range/Humidity.Range)

# base R
weather$Dew.Point.Range <- weather$Max.Dew.PointF - weather$Min.DewpointF
weather$Humidity.Range <- weather$Max.Humidity - weather$Min.Humidity
weather$DH.Range.Ratio <- weather$Dew.Point.Range/weather$Humidity.Range


# or using within()
weather <- within(weather, {
  Dew.Point.Range <- Max.Dew.PointF - Min.DewpointF
  Humidity.Range <- Max.Humidity - Min.Humidity
  DH.Range.Ratio <- Dew.Point.Range/Humidity.Range
  })

# or using transform();
# Heads up! transform() resets the class to "data.frame"
class(weather)
weather <- transform(weather, Dew.Point.Range = Max.Dew.PointF - Min.DewpointF,
                     Humidity.Range = Max.Humidity - Min.Humidity)
weather$DH.Range.Ratio <- weather$Dew.Point.Range - weather$Humidity.Range
class(weather) # no longer tbl_df
# reset
weather <- tbl_df(weather)

# another mutate example
weather <- mutate(weather, Temp.Centered = Max.TemperatureF - mean(Max.TemperatureF))
weather$Temp.Centered[1:5]
sum(weather$Temp.Centered) # should sum to 0, or thereabouts

# transmute() - like mutate, but keeps only the newly created variables
changes <- transmute(allStocks, HighLowDiff = High - Low,
                     OpenCloseDiff = Open - Close)
head(changes)

# in base R
changes <- data.frame(HighLowDiff = allStocks$High - allStocks$Low,
                      OpenCloseDiff = allStocks$Open - allStocks$Close)
head(changes)


# summarise() - summarize values and collapse a data frame to a single row
summarise(weather, meanMaxTemp = mean(Max.TemperatureF), 
          medianMaxTemp = median(Max.TemperatureF))
# in base R
data.frame(meanMaxTemp = mean(weather$Max.TemperatureF), 
           medianMaxTemp = median(weather$Max.TemperatureF))

# top_n() - Select top n rows (by value)
top_n(weather, 5, Max.TemperatureF) # notice order by date is preserved

# in base R, more complicated (to preserve order):
weather[rank(weather$Max.TemperatureF, ties.method = "min") > (nrow(weather)-10),]


# sample_n() - randomly sample fixed number of rows of a data frame
set.seed(1)
sample_n(weather, 5)

# in base R
set.seed(1)
weather[sample(x = nrow(weather), size = 5),]


# sample_frac() - randomly sample fixed fraction of rows of a data frame
set.seed(2)
sample_frac(weather, 0.10)

# in base R
set.seed(2)
weather[sample(x = nrow(weather), size = 0.10*nrow(weather)),]


# dplyr provides numerous helper functions: 

# n(): number of observations in the current group; This function can only be
# used from within summarise, mutate and filter. For example:
summarise(group_by(weather, Events),n=n())
# or chained together
weather %>% group_by(Events) %>% summarise(n=n())

# base R:
xtabs(~ Events, data=weather) %>% as.data.frame()

# n_distinct(x): count the number of unique values in x. This is a faster and
# more concise equivalent of length(unique(x))
n_distinct(arrests$Children)

# first(x), last(x) and nth(x, n): similar to x[1], x[length(x)], x[n] 
first(popVa$city) 
last(popVa$city) 
nth(popVa$city, 10)


# Combining and Comparing Data Sets ---------------------------------------

# dplyr has a number of functions for combining and comparing data sets. Let's
# walk through the examples presented in RStudio's dplyr cheat sheet.
a <- data.frame(x1=c("A","B","C"), x2=1:3, 
                stringsAsFactors = FALSE)
b <- data.frame(x1=c("A","B","D"), x2=c(TRUE,FALSE,TRUE),
                stringsAsFactors = FALSE)
a;b

# mutating joins - create new data frames

# join matching rows from b to a (ie, keep all records in a)
left_join(a, b, by="x1")
merge(a, b, by="x1", all.x = TRUE) # base R equivalent

# join matching rows from a to b (ie, keep all records in b)
right_join(a, b, by="x1")
merge(a, b, by="x1", all.y = TRUE) # base R equivalent

# join data, retain only rows in both sets
inner_join(a, b, by="x1")
merge(a, b, by="x1") # base R equivalent

# join data, retain all values all rows (aka, outer join)
full_join(a, b, by="x1")
merge(a, b, by="x1", all=TRUE) # base R equivalent

# filtering joins - returns a filtered data frame

# all rows in a that have a match in b
semi_join(a, b, by="x1")

# all rows in a that do not have a match in b
anti_join(a, b, by="x1")

# set operations - comparing two data frames (notice these data frames have
# matching column names)

y <- data.frame(x1=c("A","B","C"), x2=1:3, 
                stringsAsFactors = FALSE)
z <- data.frame(x1=c("B","C","D"), x2=2:4, 
                stringsAsFactors = FALSE)
y;z

# rows that appear in both y and z
intersect(y, z)

# rows that appear in either or both y and z
union(y, z)

# rows that appear in y but not z
setdiff(y, z)

# Recall these are dplyr functions that are masking base R functions.
conflicts(where = search(), detail = TRUE)

# In dplyr documentation: "These functions override the set functions provided
# in base to make them generic so that efficient versions for data frames and
# other tables can be provided."
methods(intersect)

# binding - appending rows or columns

# append z to y as new rows
bind_rows(y, z) # returns a tbl_df
rbind(y, z)

# When you supply a column name with the `.id` argument, a new column is created
# to link each row to its original data frame
bind_rows(y, z, .id = "source") 

# Also, columns don't need to match when row-binding
bind_rows(data.frame(x = 1:3), data.frame(y = 1:4))
# rbind gives an error:
# rbind(data.frame(x = 1:3), data.frame(y = 1:4))

# append z to y as new columns
bind_cols(y, z) # returns a tbl_df
cbind(y, z)




# More examples -----------------------------------------------------------

# Let's work through some more examples

# Find the minimum and maximum stock price for each stock
allStocks %>% 
  group_by(Stock) %>%
  summarise(Min=min(Low), Max=max(High))

# How could we do that without dplyr?
tmp <- split(allStocks, allStocks$Stock)
cbind(
  Min = sapply(tmp, function(x)min(x[,"Low"])),
  Max = sapply(tmp, function(x)max(x[,"High"]))
  )

# I like the first method better myself.
rm(tmp) # tidy up
    
# Find the largest change in Open and Close price for each stock
allStocks %>%
  group_by(Stock) %>%
  mutate(Change = Close - Open) %>%
  summarise(LargestGain = max(Change), LargestLoss = min(Change))

# We can save the new data frame
lgl <- allStocks %>%
  group_by(Stock) %>%
  mutate(Change = Close - Open) %>%
  summarise(LargestGain = max(Change), LargestLoss = min(Change))
lgl
class(lgl) # notice it has class "tbl_df"

# Again we can do assignment at the end of the chain as well
rm(lgl)
allStocks %>%
  group_by(Stock) %>%
  mutate(Change = Close - Open) %>%
  summarise(LargestGain = max(Change), LargestLoss = min(Change)) -> lgl

# More examples...

# get mean pop'n in cities vs towns in popVa
popVa %>%
  group_by(city.ind) %>%
  summarize(meanPop=mean(respop72012))

# get % change in population from April 2010 to July 2012,
# rounded to one place, sort descending, show top 5
popVa %>%
  select(city, rescen42010, respop72012) %>%
  mutate(percentChange=round((respop72012-rescen42010)/rescen42010*100,1),
         absoluteChange=respop72012-rescen42010) %>%
  arrange(desc(percentChange)) %>%
  head(5)

# same as before, but add an indicator for growing (or not) and save
popVaGRate <- popVa %>%
  select(city, rescen42010, respop72012, city.ind) %>%
  mutate(percentChange=round((respop72012-rescen42010)/rescen42010*100,1),
         growing=ifelse(percentChange > 0, 1, 0)) %>%
  arrange(desc(percentChange))


# top 10 fastest growing cities and towns
popVaGRate %>%
  filter(growing == 1) %>%
  select(city,percentChange, rescen42010, respop72012, city.ind) %>%
  arrange(desc(percentChange)) %>%
  head(n=10)

# Instead of head, use top_n()
popVaGRate %>%
  filter(growing == 1) %>%
  select(city,percentChange, rescen42010, respop72012, city.ind) %>%
  top_n(10, percentChange)

# cosponsors of senate bills: the top 10 higgest cosponsored bills
SenateBills %>%
  filter(cosponsors > 0) %>%
  arrange(desc(cosponsors)) %>%
  select(bill, sponsor, cosponsors) %>%
  head(n=10)

# number of bills per sponsor (senator);
# for those with at least two bills;
# sorted descending
SenateBills %>%
  group_by(sponsor) %>%
  summarize(total=n()) %>%
  arrange(desc(total)) %>%
  filter(total>1)

# total arrested by occupation and sex
arrests %>%
  group_by(Occup2, Sex) %>%
  filter(Sex != 9) %>%
  summarize(total = n()) %>% 
  arrange(desc(total))

# This isn't arranged in the order I requested. What's going on? We have to 
# ungroup the data before we can arrange the data. 

arrests %>%
  group_by(Occup2, Sex) %>%
  filter(Sex != 9) %>%
  summarize(total = n()) %>% 
  ungroup() %>%     # ungroup the data
  arrange(desc(total))

# Moving on...

# Add a variable to weather for cumulative precipitation using cumsum(), a base 
# R function. cumsum() belongs to a class of functions called window functions.
# These functions take a vector of values and return another vector of values.

cumsum(c(1,2,5,3))

weather <- weather %>%
  mutate(cumPrecip = cumsum(PrecipitationIn))
weather$cumPrecip[1:10]

# quick plot of cumulative precipitation over 2013
plot(cumPrecip ~ Date, data=weather, type="l")
abline(h = seq(10,40,10), lty=3, col="grey")

# calculate mean max temperature per month
weather %>%
  group_by(Month) %>%
  summarize(meanMaxTemp=round(mean(Max.TemperatureF)))

# save the previous summary and graph a dot chart
meanMax <- weather %>%
  group_by(Month) %>%
  summarize(meanMaxTemp=round(mean(Max.TemperatureF)))

dotchart(x = meanMax$meanMaxTemp, 
         labels = meanMax$Month, lcolor="black", pch=19,
         main="Mean Max C'ville Temp by Month, 2013")

# Let's shorten the names in electionData and derive some new variables.

electionData <- electionData %>% 
  rename(TEV = `Total Elec Vote`, TPR = `Total.Popular.Vote`, 
         EVD = `Elec Vote D`, EVR = `Elec Vote R`) %>%
  mutate(MOV2 = ifelse(is.na(EVD),MOV*-1,MOV),  # pos/neg Margin of victory
         State = tolower(State),           # make state lower case for mapping purposes
         Blue = ifelse(!is.na(EVD),1,0))   # Blue State/Red State indicator

# With our data cleaned up we can create some graphs:
# install.packages("ggplot2)
library(ggplot2)

# a sideways bar plot showing Margin of Votes, coded by Blue indicator:
library(scales) # for comma() function
ggplot(electionData, aes(y=MOV2, x=State, fill=factor(Blue))) + 
  geom_bar(stat="identity", position="identity") +
  scale_fill_manual(values=c("red","blue"), labels=c("Romney","Obama"), 
                    guide=guide_legend(title=NULL)) +
  scale_y_continuous(breaks=pretty(range(electionData$MOV2)), 
                     labels=comma(pretty(range(electionData$MOV2)))) +
  ylab("Margin of Votes") +
  coord_flip() 

# US map with color mapped to MOV2 to see how closely contested a state was. A
# less statistical way to display the same information as the previous graph.

# install.packages("maps)
library(maps)
# Use the map_data() function from the maps package to create a data frame of US
# map data. states contains lat/long data for states.
states <- map_data("state")

# Now merge the map data with the election data by state name using dplyr's
# inner_join()
choro <- inner_join(states, electionData, by = c("region" = "State"))

# Notice how we merge when we different variable names:
# by = c("region" = "State")

# Base R equivalent
# choro <- merge(states, electionData, by.x = "region", by.y = "State")

# now plot the map using ggplot
ggplot(choro, aes(x=long, y=lat, group=group, fill=MOV2)) +
  geom_polygon(color="black") +
  scale_fill_gradient2("Margin of Vote", low="red", high="blue", 
                       space = "Lab",
                       labels=comma, limits=c(-4e6,4e6)) +
  coord_quickmap()

# end