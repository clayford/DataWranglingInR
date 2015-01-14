#' ---
#' title: "Data Wrangling in R: Formatting Strings and Dates"
#' author: "Clay Ford"
#' date: "Spring 2015"
#' output: pdf_document
#' ---


# load data from last lecture
load("../data/datasets_L05.Rda")


# Formatting Character Strings --------------------------------------------

# Let's read in some census data. The following data are 2012 population 
# estimates of cities/towns in Virginia. It comes from the URL:
# http://quickfacts.census.gov/qfd/states/51000lk.html

# I set stringsAsFactors to FALSE because I don't want the character strings 
# treated as factors. The character string associated with each row is unique so
# there's no advantage or reason to store as a factor. 
popVa <- read.csv("PEP_2012_PEPANNRES_with_ann.csv",
                  stringsAsFactors=FALSE)
head(popVa)
str(popVa)

# I loaded this data set to show some examples of how R can manipulate character
# strings. Specifically I want to work with the GEO.display.label and GEO.id 
# columns. Some things we might want to do include removing the comma, the word 
# "Virginia" and the words "town" or "city. We might also like to create an 
# indicator for whether a row refers to a city or town. Finally I may want to 
# extract the last 7 digits from Geo.id since those are the unique ID values. 
# (Of course the GEO.id2 already has the last 7 digits from Geo.id, so we don't
# need to do it, but I'm going to show you how anyway.)

# Let's introduce some basic functions for investigating and manipulating
# character strings.

# nchar() - calculate the number of characters in a string. 

nchar("data")
popVa$GEO.id[1]
nchar(popVa$GEO.id[1])
popVa$GEO.display.label[1]
# Notice that spaces are counted
nchar(popVa$GEO.display.label[1])

# nchar is vectorized so it will work on entire vectors:
nchar(popVa$GEO.display.label)

# NOTE: nchar() does not work on factors.

# How does nchar() handle NAs? It returns 2:
(x <- c("UVa","UVa",NA, "GT", "GT"))
is.na(x)
nchar(x)
# Here's one way to skip NAs and only report the number of characters of
# non-missing strings:
nchar(x[!is.na(x)])
rm(x)

# paste() - Concatenate vectors after converting to character

# paste converts its arguments to character strings, and concatenates them
# (separating them by the string given by the sep argument)

x <- "Clay"
y <- "Ford"
paste(x,y)
paste(y,x,sep=", ")

# What about a vector of character strings? Below is a vector of three strings:
(z <- c("Joe","Clayton","Ford"))
# How can I paste the elements together to form one string? Use the collapse
# argument (the sep argument has no effect when applied to a single vector):
paste(z, collapse = " ")

# paste() is also vectorized and will work on vectors. Recall the airquality
# dataset that comes with R:
head(airquality)

# Let's say we want to paste the Month and Day together to form a new variable
# called Date. Here's how we do it:
airquality$Date <- paste(airquality$Month, airquality$Day, sep="/")
head(airquality)


# substr() - Extract or replace substrings in a character vector. 

# basic syntax: substr(x, start, stop) where x is a character vector and
# start/stop are integers representing the fist and last elements.
substr("virginia", 4, 6)
substr(c("214-555-1234","434-888-7777"), 5, 12)


# Notice substr() works on vectors of integers
class(popVa$GEO.id2)
# extract the last five digits:
substr(popVa$GEO.id2, 3, 7)
# notice the digits are returned as character strings. We can easily convert
# back to integer:
as.integer(substr(popVa$GEO.id2, 3, 7))
# But notice leading 0s are dropped. Probably better to keep extracted data as 
# character vector in this case since we wouldn't use these numbers for
# calculation.

# We can also use substr() to replace substrings:
x <- "Megatron"
substr(x,5,8) <- "zord"
x
# This works but probably easier to use sub() and gsub() which we'll get to
# shortly.


# strsplit() - Split the Elements of a Character Vector

# strsplit() splits the elements of a character vector x into substrings 
# according to a specified split. The basic syntax is strsplit(x, split) where x
# is a character vector and split is a character vector to use for splitting.

strsplit("212-555-1212", split="-")

# Notice strsplit returns a list object.

# Let's split the contents of the GEO.display.label column by the comma and
# store in temp.
temp <- strsplit(popVa$GEO.display.label,",")
temp[1:3]
# Notice we split each string by comma into two strings and the splitting
# character, the comma, is discarded.

# Now we can use this object to extract just the city/town names. Again this is
# a list. Let's investigate the structure of the object:
temp[[1]] # the first list element
temp[[1]][1] # the first element of the vector in the first list element

# we can use sapply and an anonymous function to go through the list and pull
# out the first vector element from each list element, like so.
popVa$city <- sapply(temp, function(x)x[1]) # apply to each list element
head(popVa)

# sub() - find and replaces first instance
# gsub() - find and replaces all instances

text <- "I said no no no"
sub("no","yes", text) # first instance
gsub("no","yes", text) # all instances

# Let's go back to the popVa data and remove "city" and "town" from city column:
popVa$city[1:5] # look at first 5

# first remove " city"; find " city" and replace with nothing. gsub() or sub() 
# will work here. I tend to use gsub() by default unless I know I only want to
# replace the first instance of something.
popVa$city <- gsub(" city","",popVa$city)
# then remove " town":
popVa$city <- gsub(" town","",popVa$city)
# And have a look:
popVa$city[1:5]

# I should point out the previous two lines of code could have been carried with
# one line using a "regular expression". Regular expressions are a method of 
# expressing patterns in character values. For example I could have submitted
# the following single line of code to extract the city/town names:

temp <- gsub(" city, Virginia$| town, Virginia$", "", popVa$GEO.display.label)
temp[1:5]

# The dollar sign means "find at the end of the string". The pipe means "or". So
# the expression is find " city, Virginia" or " town, Virginia" at the end of 
# the string. We will delve into regular expressions in a future lecture. This 
# is a very simple example. Regular Expressions can get quite complicated and
# indeed there are entire books devoted to regular expressions.


# grep and grepl - Pattern Matching and Replacement

# grep and grepl search for text or strings. grep() returns indices of matches
# while grepl() returns a logical vector.
grep("Fog-Rain",weather$Events)
# Note: the argument invert=T will return the opposite: indices that do not
# match the string.

# The value=T will extract the vector elements containing the match
grep("Fog-Rain",weather$Events, value=T) 

# grepl() returns a logical vector
grepl("Fog-Rain",weather$Events)

# Let's create city/town indicator in popVa data frame:
popVa$city.ind <- ifelse(grepl("city,", popVa$GEO.display.label)==1,1,0)

# grep() and grepl() also have an ignore.case argument. When set to TRUE, case
# is ignored so searching for "city" finds "City", "CITY", and "city". 

# grep, grepl, sub and gsub become extremely powerful with regular expressions,
# which we'll explore in a later lecture.

# The stringr package

# From the package description: "stringr is a set of simple wrappers that make 
# R's string functions more consistent, simpler and easier to use." That about 
# says it all. This is an invaluable package for working with string data.

# install.packages("stringr")
library(stringr)

# We will look deeper into stringr when we cover regular expressions, for now
# here are two handy functions tide you over:

# str_trim() - Trim whitespace from start and end of string.
x <- c(" VA ", " MD ", " DE ")
x
str_trim(x)

# str_sub() - Extract substrings from a character vector.

# This differs from substr() by allowing you use negative numbers to count
# backward and to specify only the end position

loc <- "Charlottesville, VA, 22901"
str_sub(loc, end = 15)
str_sub(loc, start = -5) # extract the last 5 characters
str_sub(loc, end = -5) # extract the 5th from last character and everything before it



# Formatting Dates --------------------------------------------------------

# Recall the weather data has a column called EST, which contains the date of
# the record.
weather$EST[1:5]
class(weather$EST)

# It is currently stored as a factor because each date contains slashes, which 
# meant R interpreted the column as character and consequently converted to 
# factor upon import. It would be better to format this column as a date class
# so we could do things like calculate the elapsed number of days between "snow".

# When dealing with dates that contain only month, day and/or year, we can use 
# the as.Date() function. The basic syntax is as.Date(x, format) where x is the 
# object to be converted and format is the display format of the date stated in
# strptime (stir-pee-time) symbols.

# see help(strptime) for a list of symbols. It may seem daunting at first but 
# it's not too bad once you get the hang of it. A date such as April 5, 1982 has
# a strptime format of "%B %d, %Y". %B is full month name, %d is day of month as
# a decimal number, and %Y is a four digit year. Let's do a quick example:

# format April 5, 1982 as a date:
x <- "April 5, 1982 "
x
class(x)
# Now convert to Date class:
y <- as.Date(x, format="%B %d, %Y")
y
class(y)

# Typically we'll do this for an entire column in a data frame. Let's convert
# the EST column in weather:
weather$Date <- as.Date(weather$EST, format="%m/%d/%Y")
weather$Date[1:5]
class(weather$Date)

# That may seem like a minor conversion, but with a date column formatted as 
# Date we can now easily identify weekdays, months, and quarters. The
# weekdays(), months() and quarters() functions will create character vectors of
# weekdays, months and quarters respectively when applied to a date vector.
weekdays(weather$Date)[1:10]
months(weather$Date)[30:35]
quarters(weather$Date)[85:95]

# It also makes plotting values over time easy:
plot(Max.TemperatureF ~ Date, data=weather, type="l")

# Sometimes we desire to display dates in a certain format. We can do that with
# the format() function as follows:
today <- Sys.Date()
today
class(today)
format(today, "%B %d, %Y")
format(today, "%a %b %d")
format(today, "%Y-%b-%d")
format(today, "%m/%d/%y")
format(today, "%A, %B%e, %Y")

# In each case above the date is displayed in a new format and converted to
# character.

# What if we have day, month and year in separate fields? Let's generate some
# data:
set.seed(3)
Year <- sample(2010:2014, 20, replace=T)
Year
Month <- sample(1:12, 20, replace=T)
Month
Day <- sample(1:31, 20, replace=T)
Day
rdates <- data.frame(Year, Month, Day)
head(rdates)

# The ISOdate() function allows us to combine those fields into a single date.
# The basic syntax is ISOdate(year, month, day):
with(rdates, ISOdate(Year, Month, Day)) 

# Two things to notice here: (1) the dates are displayed in POSIXct format. It's
# basically a date format with hours, minutes and seconds displayed plus a time 
# zone. There's more to POSIX but that will do for now. (2) There's a NA. Why is
# that? Because June 31 is not an actual date. My randomly generated data 
# created a date that doesn't exist! But fortunately ISOdate() caught that.

# If we like we can convert to Date class:
as.Date(with(rdates, ISOdate(Year, Month, Day)))


# Dates are represented as the number of days since 1970-01-01, with negative 
# values for earlier dates. We can can get the number of days (aka, the Julian
# date) using the julian() function.
julian(Sys.Date())
julian(as.Date("1970-01-01"))
julian(as.Date("1776-07-04"))
julian(Sys.Date()) - julian(as.Date("1776-07-04")) 


# With dates formatted in Date class, we can calculate elapsed time:
as.Date("2014-03-01") - as.Date("2013-03-01")

# We can also use the difftime() function:
difftime(as.Date("2014-03-01"), as.Date("2013-03-01"))

# The difftime() function can display elapsed time in several different units, 
# including "days", "weeks", "hours", "mins" and "secs". Note that "hours",
# "mins" and "secs" only works for POSIX classes.
difftime(as.Date("2014-03-01"), as.Date("2013-03-01"),
         units="weeks")
(x <- difftime(as.Date("2014-03-01"), as.Date("2013-03-01"),
               units="weeks"))
# convert to numeric:
as.numeric(x)

# Let's explain POSIX a bit more. There are two classes in R: POSIXct and
# POSIXlt. "POSIXct datetimes are represented as seconds since January 1, 1970
# GMT while POSIXlt datetimes are represented by a list of 9 components plus an 
# optional tzone attribute." (R News, Vol 4/1, June 2004)

x <- as.POSIXct(Sys.time())
x
class(x)
y <- as.POSIXlt(Sys.time())
y
class(y)
# Use unclass to see the internal "list" structure:
unclass(y)
# notice we can extract specific elements using the names:
y$mon
y$mday
y$sec
# see ?DateTimeClasses for a list of elements in POSIXlt. Unless you really need
# POSIXlt, I would go with POSIXct. In fact I recommend formatting dates as
# simply as possible. If you just have months and days, use as.Date.

# Back to difftime(). We can use it with a POSIX formatted date to easily
# calculate elapsed time in hours, minutes, or seconds.
x <- as.POSIXct("1973-04-05 15:30:00") # born
y <- as.POSIXct("2013-04-05 15:30:00") # turning 40!
difftime(y,x, units = "hours")
difftime(y,x, units = "mins")
difftime(y,x, units = "secs")

# Another handy function to use with dates is diff(). It can quickly tell us the
# elapsed number of days between dates. Let's subset the weather data to have 
# only days with significant precipitation (ie, greater than the trace amount of
# 0.001)
rainyDays <- subset(weather, PrecipitationIn > 0.001, select = Date)
# Now use the diff() function on the vector of dates:
diff(rainyDays$Date)
# To summarize this information we need to convert it to numeric:
summary(as.numeric(diff(rainyDays$Date)))

# At one point we went 16 days without precipitation. When did that happen?
f <- which.max(diff(rainyDays$Date))
rainyDays[f,]
rainyDays[f+1,]
 
# Generating time sequences

# we can use the seq() functiont to generate a sequence of dates. 

# sequence of dates with interval of one day:
seq(as.Date("2015-01-01"), by="days", length=14)

# sequence of dates with interval of two weeks from Jan 1 to Dec 31:
seq(as.Date("2015-01-01"), to=as.Date("2015-12-31"), by="2 weeks")


# The lubridate package

# From the package description: "Lubridate has a consistent, memorable syntax, 
# that makes working with dates fun instead of frustrating." Fun? I'll let you 
# be the judge of that. But I will say this package does make working with dates
# much easier.

# install.packages("lubridate")
library(lubridate)

# Remember the EST column in the weather data and how we had to use strptime
# symbols to specify the format? You don't have to do that with lubridate!
weather$EST[1:3]

# We have month, day, year. So to convert that to a date with lubridate we use
# the mdy() function:
mdy(weather$EST[1:3])

# lubridate has many such functions. Just choose the function whose name models
# the order in which the year ('y'), month ('m') and day ('d') elements appear:
# dmy, myd, ymd, ydm, dym, mdy, ymd_hms

# If that was all lubridate did it would still be a fantastic package. But it
# does much more! lubridate has a very friendly vignette you should read. You
# can also read the original journal article: "Dates and Times Made Easy with
# lubridate" by Garrett Grolemund, Hadley Wickham.
# http://www.jstatsoft.org/v40/i03/

# save data for next set of lecture notes
save(list=c("electionData", "weather", "arrests", "allStocks", "popVa"), 
     file="../data/datasets_L06.Rda")
