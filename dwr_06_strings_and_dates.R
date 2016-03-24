#' ---
#' title: "Data Wrangling in R: Formatting Strings and Dates"
#' author: "Clay Ford"
#' date: "Spring 2016"
#' output: pdf_document
#' ---


# load data from last lecture
setwd("../data")
load("datasets_L05.Rda")


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
tail(popVa)
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

nchar("It's showtime!")
# Notice that spaces and punctuation are counted

popVa$GEO.id[1]
nchar(popVa$GEO.id[1])
popVa$GEO.display.label[1]
nchar(popVa$GEO.display.label[1])

# nchar is vectorized so it will work on entire vectors:
nchar(popVa$GEO.display.label)

# NOTE: nchar() does not work on factors.
# (tmp <- factor(c("apple","apple","berry","berry")))
# nchar(tmp)
# Error in nchar(tmp) : 'nchar()' requires a character vector

# need to convert to character
# nchar(as.character(tmp))
# rm(tmp)

# How does nchar() handle NAs? It returns 2:
(x <- c("UVa","UVa",NA, "GT", "GT"))
is.na(x)
nchar(x)

# Here's one way to skip NAs and only report the number of characters of
# non-missing strings:
nchar(x[!is.na(x)])
rm(x)

# tolower() and toupper() - convert upper-case characters in a character vector
# to lower-case, or vice versa. Non-alphabetic characters are left unchanged. 

tolower("HEY GUYS")
toupper("omg!")

# and again these work on vectors
state.abb[1:5]
tolower(state.abb[1:5])

state.name[1:5]
toupper(state.name[1:5])

# I find tolower() useful when you've read in data with ALLCAP column names and
# you want to convert to lowercase.

dat <- data.frame(ID=1:3, NAME=c("Bill","Ted","John"),AGE=c(23,21,20))
dat
names(dat) <- tolower(names(dat))
dat
rm(dat)

# trimws() - Remove leading and/or trailing whitespace from character strings.

x <- c(" VA    ", "MD ", "   DE ")
x
trimws(x) # default argument: which = "both
trimws(x, which = "right")
trimws(x, which = "left")

# Our arrests data has a column called CommuneName. It's currently stored as a Factor.
str(arrests$CommuneName)
arrests$CommuneName[1:4]

# Let's convert to character. Really no reason to keep as a factor.
arrests$CommuneName <- as.character(arrests$CommuneName)

# But look at all the extra spaces...
arrests$CommuneName[1:4]

# Let's trim it
arrests$CommuneName <- trimws(arrests$CommuneName)
arrests$CommuneName[1:4]


# abbreviate() - Abbreviate strings to at least minlength characters, such that
# they remain unique. By default, minlength = 4

abbreviate(names(popVa))

# A common use of abbreviate is cleaning up long variable names in a data frame.
orig <- names(popVa) # save
names(popVa) <- abbreviate(names(popVa))
str(popVa)
# discard abbreviated names and change back to original names
names(popVa) <- orig

# Warning from the documentation: This is really only suitable for English, and
# does not work correctly with non-ASCII characters.

# paste() and paste0() - Concatenate vectors after converting to character

# paste converts its arguments to character strings, and concatenates them
# (separating them by the string given by the sep argument)

x <- "Irwin"
y <- "Fletcher"
paste(x, y)
# use the sep argument to specify what, if anything, should be pasted between
# the items. For example, to create "Fletcher, Irwin"
paste(y, x, sep=", ")

# There's also paste0() which is basically paste() with sep=""
paste0(x, y)
paste0(21, 12) # also works for numbers, but converts to character

# paste() is vectorized and will work on vectors. Recall the airquality dataset
# that comes with R:
head(airquality)

# Let's say we want to paste the Month and Day together along with 1973 to form
# a new variable called Date. Here's how we do it:
airquality$Date <- paste(airquality$Month, airquality$Day, "1973", sep="/")
head(airquality)

# What about pasting elements in a vector into one character string? Below is a
# vector of three strings:
(z <- c("Irwin","M.","Fletcher"))
# How can I paste the elements together to form one string? Use the collapse
# argument:
paste(z, collapse = " ")


# strsplit() - Split the Elements of a Character Vector

# strsplit() splits the elements of a character vector x into substrings 
# according to a specified split. The basic syntax is strsplit(x, split) where x
# is a character vector and split is a character vector to use for splitting.

# This is sort of like the opposite of paste() with the collapse="" argument.
Fletch <- paste(z, collapse = " ")
Fletch
strsplit(Fletch, split = " ")

# Notice strsplit returns a list object and discards the split character. We can
# use unlist() to quickly get a vector:

unlist(strsplit(Fletch, split = " "))

# another example:

strsplit("212-555-1212", split="-")

# what about splitting on a period? Be careful!

strsplit(c("fig1.jpg","fig2.jpg","fig3.jpg"), split = ".")

# The split argument takes a regular expression and a "." has a special meaning 
# in regular expressions. We either need to "escape" the period so it is treated
# like a literal string or use the fixed argument. To "escape" something in R,
# use two backslashes:

strsplit(c("fig1.jpg","fig2.jpg","fig3.jpg"), split = "\\.")
strsplit(c("fig1.jpg","fig2.jpg","fig3.jpg"), split = ".", fixed = TRUE)

# More on regular expressions in the next class!

# What happens if we split on nothing? Everything is split.
strsplit("abcde", split="")

# Extended example: Distribution of English letters in the screenplay of Airplane!

# Read in screenplay and make all text lower case:
url <- "http://www.awesomefilm.com/script/airplane.txt"
airplane <- tolower(scan(url, what = "character")) # scan reads in one word at a time
airplane[1:10]

# Now split the words into letters
airplaneLetters <- strsplit(airplane, split = "") # Large list! One element for each word
airplaneLetters[1:3]

# Let's unlist into one big character vector:
airplaneLetters <- unlist(airplaneLetters)
length(airplaneLetters)

# Get the count of letters with the table() function
table(airplaneLetters)

# Let's keep only english letters
keep <- airplaneLetters %in% letters
airplaneLetters <- airplaneLetters[keep]

# Just counts of English letters
table(airplaneLetters)

# Sort and save
(lettDist <- sort(table(airplaneLetters), decreasing = TRUE))

# Now present in a data frame
data.frame(letters=names(lettDist), 
           percent=paste0(round(lettDist/length(airplaneLetters),4)*100,"%"))


# Let's split the contents of the GEO.display.label column by the comma and
# store in temp.
head(popVa$GEO.display.label)
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
# sometimes you'll see people do something like this, because `[` is itself a
# function:
# sapply(temp, function(x)`[`(x,1))


# substr() - Extract or replace substrings in a character vector. 

# basic syntax: substr(x, start, stop) where x is a character vector and
# start/stop are integers representing the fist and last elements.
substr("Fletcher", 1, 6)
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

# We can also use substr() <- to replace substrings:
x <- "Megatron"
substr(x,5,8) <- "zord"
x
# This works but probably easier to use sub() and gsub(). Speaking of which...



# sub() - find and replaces first instance
# gsub() - find and replaces all instances

# The basic syntax is sub/gsub(pattern, replacement, x) where pattern is the 
# pattern of characters to be matched, replacement is the replacement for the
# matched pattern, and x is the character vector where matches are sought.

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

# I should point out the creation of the city column could have been carried out
# with one line using a "regular expression". For example I could have submitted
# the following single line of code to extract the city/town names:

temp <- gsub(" city, Virginia$| town, Virginia$", "", popVa$GEO.display.label)
temp[1:5]
rm(temp)

# The dollar sign means "find at the end of the string". The pipe means "or". So
# the expression is find " city, Virginia" or " town, Virginia" at the end of 
# the string. Agan, we'll get into regular expressions in the next class. This 
# is a very simple example. Regular Expressions can get quite complicated and 
# indeed there are entire books devoted to regular expressions.


# grep() and grepl() - Pattern Matching and Replacement

# grep and grepl search for text or strings. grep() returns indices of matches 
# while grepl() returns a logical vector. For example, here's how we can find
# the indices of weather$Events that contain the phrase "Fog-Rain":
grep("Fog-Rain",weather$Events)
# Note: the argument invert=T will return the opposite: indices that do not
# match the string.

# The argument value=T will extract the vector elements containing the match
grep("Fog-Rain",weather$Events, value=T) 

# grepl() returns a logical vector. TRUE if a match is found, FALSE otherwise.
grepl("Fog-Rain",weather$Events)[1:20]

# Let's create city/town indicator in popVa data frame:
popVa$city.ind <- ifelse(grepl("city,", popVa$GEO.display.label),1,0)
popVa[1:10,c("city","city.ind")]

# grep() and grepl() also have an ignore.case argument. When set to TRUE, case
# is ignored so searching for "city" finds "City", "CITY", and "city". 

# grep, grepl, sub and gsub become extremely powerful with regular expressions,
# which we'll explore in the next lecture.


# The stringr package -----------------------------------------------------

# From the stringr vignette (2015-04-29):

# "Strings are not glamorous, high-profile components of R, but they do play a
# big role in many data cleaning and preparations tasks. R provides a solid set
# of string operations, but because they have grown organically over time, they
# can be inconsistent and a little hard to learn. Additionally, they lag behind
# the string operations in other programming languages, so that some things that
# are easy to do in languages like Ruby or Python are rather hard to do in R.
# The stringr package aims to remedy these problems by providing a clean, modern
# interface to common string operations."

# I encourage you to read and work through the stringr vignette.

# Let's look at a few stringr functions. Note the similarity in function names.
# They all begin with "str_"

# install.packages("stringr")
library(stringr)


# str_sub() - equivalent to substr() but allows you to use negative numbers to
# count backward and to specify only the end position.

loc <- "Charlottesville, VA, 22901"
str_sub(loc, end = 15) # extract everything through position 15
str_sub(loc, start = -5) # extract the last 5 characters
str_sub(loc, end = -8) # extract the 8th from last character and everything before it

# Also, with str_sub() replacement strings not do need to be the same length as
# the string they are replacing unlike substr().

x <- "Megatron"
substr(x,5,8) <- "zooooord"
x # only 4 characters replaced

x <- "Megatron"
str_sub(x, 5, 8) <-  "zooooord"
x # all characters replaced

# str_length() - equivalent to nchar(), but it preserves NA's (rather than
# giving them length 2)

(x <- c("UVa","UVa",NA, "GT", "GT"))
is.na(x)
str_length(x)

# str_detect() - similar to grepl

str_detect(weather$Events, "Fog-Rain")[1:20]


# str_replace() - similar to sub() and gsub().

text <- "I said no no no"
str_replace(text, "no", "yes") # first instance
str_replace_all(text, "no", "yes") # all instances


# str_split() and str_split_fixed() - similar to strsplit

head(popVa$GEO.display.label)
temp <- str_split(popVa$GEO.display.label,",")
temp[1:3]

# str_split_fixed() allows you to limit the splits to a certain number. It 
# returns a character matrix instead of a list. Below we only split on the first
# hyphen in weather$Events.
head(weather$Events)
temp <- str_split_fixed(weather$Events,"-", 2)
temp[15:20,]

# str_count() - Count the number of matches in a string

str_count("Mississipi","s")

# How many times does "striker" appear in the screenplay for airplane? First we
# make the airplane object one long character vector.
airplaneV <- paste(airplane, collapse = " ")
length(airplaneV)
# Now do the count
str_count(airplaneV, "striker")

# surely vs. shirley
str_count(airplaneV, "surely")
str_count(airplaneV, "shirley")

# As you can see the stringr functions have a consistent naming scheme (str_*). 
# When combined with regular expressions they provide a powerful arsenal of 
# character manipulation tools. There are several other stringr functions. Be
# sure to read the documentation to learn more and see some good examples.


# Formatting Dates --------------------------------------------------------

# Recall the weather data has a column called EST, which contains the date of
# the record.
weather$EST[1:5]
class(weather$EST)

# It is currently stored as a factor because each date contains slashes, which 
# meant R interpreted the column as character and consequently converted to 
# factor upon import. It would be better to format this column as a date class 
# so we could do things like calculate the elapsed number of days between "snow"
# or plot change in Temperature over time.

# When dealing with dates that contain only month, day and/or year, we can use 
# the as.Date() function. The basic syntax is as.Date(x, format) where x is the 
# object to be converted and format is the display format of the date stated in
# strptime (stir-pee-time) symbols.

# See help(strptime) for a list of symbols. No seriously, go look at it. It may
# seem daunting at first but it's not too bad once you get the hang of it. A
# date such as April 5, 1982 has a strptime format of "%B %d, %Y". %B is full
# month name, %d is day of month as a decimal number, and %Y is a four digit
# year. Let's do a quick example:

# format April 5, 1982 as a date:
x <- "April 5, 1982"
x
class(x)
# Now convert to Date class:
y <- as.Date(x, format="%B %d, %Y")
y
class(y)

# Typically we do this for an entire column in a data frame. Let's convert the 
# EST column in weather. The format is %m/%d/%Y (for example, 1/1/2013)

weather$Date <- as.Date(weather$EST, format="%m/%d/%Y")
weather$Date[1:5]
class(weather$Date)
unclass(weather$Date[1:5]) #  number of days since 1970-01-01
typeof(weather$Date)

# That may seem like a minor conversion, but with a date column formatted as 
# Date we can now easily identify weekdays, months, and quarters. The
# weekdays(), months() and quarters() functions will create character vectors of
# weekdays, months and quarters respectively when applied to a date vector.
weekdays(weather$Date)[1:10]
months(weather$Date)[30:35]
quarters(weather$Date)[85:95]

# It also makes plotting values over time easy:
plot(Max.TemperatureF ~ Date, data=weather, type="l", 
     main="Maximum Daily Temperature in Charlottesville over 2013")

# Once we have data stored as date class, we often want to display dates in a 
# certain format. We can do that with the format() function as follows:
today <- Sys.Date() # Sys.Date() returns today's date according to our computer
today
class(today)
# Use the format() function with strptime codes to format the display of the date:
format(today, "%B %d, %Y")
format(today, "%a %b %d")
format(today, "%Y-%b-%d")
format(today, "%m/%d/%y")
format(today, "%A, %B %e, %Y")

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
# convert to numeric to get rid of the words:

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
# Use unclass to see the internal "list" structure of a POXIXlt object:
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
y <- as.POSIXct("2013-04-05 15:30:00") # turning 40.
difftime(y,x, units = "hours")
difftime(y,x, units = "mins")
difftime(y,x, units = "secs")

# Another handy function to use with dates is diff(). It can quickly tell us the
# elapsed number of days between dates. Let's subset the weather data to have 
# only days with significant precipitation (ie, greater than the trace amount of
# 0.001)
rainyDays <- subset(weather, PrecipitationIn > 0.001, select = Date)
head(rainyDays)
# Now use the diff() function on the vector of dates:
diff(rainyDays$Date)
# To summarize this information we need to convert it to numeric:
summary(as.numeric(diff(rainyDays$Date)))

# At one point we went 16 days without precipitation. When did that happen?
f <- which.max(diff(rainyDays$Date))
f
rainyDays[f,] # start date
rainyDays[f+1,] # end date
 
# Generating time sequences

# we can use the seq() functiont to generate a sequence of dates. 

# sequence of dates with interval of one day:
seq(as.Date("2015-01-01"), by="days", length=14)

# sequence of dates with interval of two weeks from Jan 1 to Dec 31:
seq(as.Date("2015-01-01"), to=as.Date("2015-12-31"), by="2 weeks")


# There is a whole field of statistics involved with time series analysis which 
# demands yet more wrangling of dates and times. One such package that provides 
# help is the zoo package. It's very mature and is well documented. If you're 
# interested in analyzing financial data with R, this is probably one package
# you'll end up using. Another package of note is xts.


# The lubridate package ---------------------------------------------------

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

# A few more lubridate functions:

# today() - The current date
# now() - The current time
# here() - The current time in your local timezone
today()
now()
here()

# am(), pm() - Does date time occur in the am or pm?
am(now())
pm(now())


# leap_year() - is this a leap year?
leap_year(2016)

# lubridate has a very friendly vignette. You can also read the
# original journal article: "Dates and Times Made Easy with lubridate" by
# Garrett Grolemund, Hadley Wickham. http://www.jstatsoft.org/v40/i03/

# save data for next set of lecture notes
save(list=c("electionData", "weather", "arrests", "allStocks", "popVa", "airplane"), 
     file="../data/datasets_L06.Rda")
