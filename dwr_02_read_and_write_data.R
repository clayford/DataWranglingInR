#' ---
#' title: "Data Wrangling in R: Reading and Writing Data"
#' author: "Clay Ford"
#' date: "Spring 2016"
#' output: pdf_document
#' ---


# We usually import, or read, data into R. Data come in many formats: CSV, 
# ASCII, XLSX, JSON, DTA (Stata), SAV (SPSS), XML, HTML, etc. The format of the
# data dictate what function(s) we need to use. 

# In this lecture we cover the following:
# - CSV
# - ASCII/TXT
# - XLSX
# - JSON

setwd("../data/")
# Note: two dots (..) mean "go back up one level in the directory"

# Packages used in this lecture:
# install.packages(c("readxl","jsonlite"))


# read.table and read.csv -------------------------------------------------

# read.table() - for reading in data in a tabular format separated by
# "separators" such as spaces, commas, tabs; reads in data and returns a data
# frame

# Example: 2013 Charlottesville weather
# http://www.wunderground.com

# cville_weather_2013.csv is a CSV file. CSV = Comma Separated Value. That means
# fields are separated by commas. CSV files can be viewed in Excel or any text 
# editor. This particular file also has column headers. In read.table() we need
# to specify these properties with the header and sep arguments.
weather <- read.table("cville_weather_2013.csv", header=TRUE, sep=",")

# Advice: do this after you read in data and carefully examine the output:
str(weather)

# In particular, examine the type of each variable (if possible). Is it what you
# expected?

# Notice anything containing characters has been converted to a "Factor". This 
# is the default behavior of read.table(). This automatic conversion to Factor 
# can be an awesome thing or a horrible thing depending on your needs. We'll 
# talk more about Factors in a future lecture. For now think of it as a way to
# store categorical variables with fixed levels.

# read.csv() is the same as read.table() with different defaults (header = TRUE,
# sep = ","). It's easier to use read.csv() when reading CSV files:
weather <- read.csv("cville_weather_2013.csv")

# After reading in data to a data frame, it's a good idea to inspect the top and
# bottom of the data frame (ie, the first few and last few rows). Two basic
# functions are head() and tail(), which show the first and last 6 records,
# respectively. Use the n= argument to change the number of rows displayed
head(weather)
head(weather[,1:3])
tail(weather, n=3)
tail(weather[,1:3], n=3)

# In RStudio, you can click on the data frame in the Environment pane to see the
# data. Or use the View() function

# NOTE: head() and tail() work on vectors as well:
# Last five Max Temp measures 
tail(weather$Max.TemperatureF, n=5)

# read.table and read.csv have many arguments! See help(read.table) 
# Arguments of note: 

# stringsAsFactors - logical argument indicating whether or not to read
# character vectors as factors. Defaults to TRUE!

# colClasses - A character vector of classes to be assumed for the columns. 

# nrows - the maximum number of rows to read in. Negative and other invalid
# values are ignored. Good for reading in just a few rows of data.
 
# skip - the number of lines of the data file to skip before beginning to read
# data.

# from the help page: read.table() and read.csv() can use a surprising amount of
# memory when reading large files.
# 
# Less memory will be used if colClasses is specified: "logical", "integer", 
# "numeric", "character", "factor", "Date", "POSIXct", or NULL to skip a column.
# 
# Using nrows, even as a mild over-estimate, will help memory usage (but not
# speed).
# 
# Using comment.char = "" will be appreciably faster than the read.table default
# (comment.char = "#")

# example of reading large file:

# datBig: 1,000,000 records, 3 columns: character, numeric, integer; 22 MB.

# Let's compare times to read in a large data set using using defaults and then
# specifying arguments. The system.time() function tells you how long R code
# takes to run.

# default settings
system.time(test <- read.csv("datBig.csv"))
# with colClasses, nrows and  comment.char specified:
system.time(test <- read.csv("datBig.csv", colClasses=c("factor","numeric","integer"),
                             comment.char = "", nrows=1e6))


# QUESTION: What if you have lots of columns? How can I easily set colClasses?
# Read in a few lines, get the class of the columns, save to a vector, then read
# in all the lines using the vector classes in the colClasses argument.

# Let's do the weather data.
weather <- read.csv("cville_weather_2013.csv", nrows=10)

# using class() on one column at a time
class(weather$EST)
class(weather$Max.TemperatureF)

# using class() on all columns at once
classes <- sapply(weather, class)

# sapply(weather, class) applies the class() function to each column of the 
# weather data frame and returns a vector stating the class of each column. The 
# "s" means simplify the output to a vector (instead of a list). Don't worry we
# will delve deeper into apply() functions later!
classes

# we can use this vector of classes in the colClasses argument 
weather <- read.csv("cville_weather_2013.csv", colClasses=classes)

# For a CSV file with 365 records it doesn't make much difference, but it comes
# in handy with larger files.

# read.fwf ----------------------------------------------------------------

# Reads a table of fixed width formatted data into a data frame;
# widths = widths of the fixed-width fields;

# Example: Analysis of Arrests in Paris, June 1848; contains descriptions of 
# 11,616 individuals arrested for alleged participation in the Parisian 
# insurrection.

# Source: http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/49
# Data is ASCII in fixed-width format;
# codebook tells you column that variable starts and how wide it is;

arrests <- read.fwf(file="00049-0001-Data.txt",
                    widths=c(5,1,2,3,2,2,1,2,1,2,2,2,1,1,1,2,18))
head(arrests)

# Notice R added column names: V1, V2, V3, etc. We could have provided column 
# names in read.fwf with the col.names argument. We'll add the column names
# using the names() function. 

# By itself the names function returns the column names of a data frame, like
# so:
names(arrests)

# But calling the names() function on a data frame and simultaneously assigning 
# to it a vector of character names will assign those names to the data frame.

# First make a vector of names from codebook:
cnames <- c("ID","Source","Constant","Occup","DeptBorn","CommuneBorn","Sex",
            "Age","MaritalStatus","Children","Arrondissement","QuarterResided",
            "FirstJudicial","FinalJudicial", "FirstDecision", "FinalOutcome",
            "CommuneName")
cnames

# Now call the names() function on arrests while assigning cnames to the result:
names(arrests) <- cnames
str(arrests)

# 99 children? No. 99 = No information given according to codebook. You'll often
# see missing data coded as "99" or "999" or something similar. You can make R 
# convert these values to NA using the na.strings argument in your read.xxx()
# function. For example:

# arrests <- read.fwf("00049-0001-Data.txt",
#                     widths=c(5,1,2,3,2,2,1,2,1,2,2,2,1,1,1,2,18), na.strings="99")

# Notice that other than Age and CommuneName, everything in arrests is a code of
# some sort.


# Excel files -------------------------------------------------------------

# If possible and convenient, I recommend saving Excel files as CSV before 
# reading into R. However there are packages for reading Excel files directly
# into R.

# read_excel() in readxl package
# read.xlsx() in xlsx package; requires Java
# read.xls() in gdata package; requires Perl

# I will demonstrate the readxl package:
library(readxl)

# Example: 2012 Presidential Election Data

# Specify worksheet name or number with sheetName= argument; If you don't
# specify a worksheet, the first one is selected.

# Set col_names=FALSE if the file contains no column names. The default is TRUE,
# so you can leave out if your xls file indeed has column names.

# See ?read_excel for more information.

electionData <- read_excel("Pres_Election_Data_2012.xlsx", sheet="State")
str(electionData)
# It's a mess; we'll come back to this later.

# Notice that columns with characters were NOT read in as Factor! This function 
# does not behave like read.table. Anything with a character is read in as type
# character.



# The readr package -------------------------------------------------------

# The readr package provides alternatives to the read.xxx functions that come 
# with base R. Two big differences are speed and not importing columns with 
# characters as Factors. I've never been dissatisfied with the base R functions 
# so I haven't been motivated to investigate the readr package too deeply, but
# it appears to be very powerful and flexible. 


# The foreign and haven packages ------------------------------------------

# The foreign package provides functions for reading data from other programs 
# such as Stata, SPSS, Minitab. The foreign package comes with R. Two commonly
# used foreign functions are:

# read.dta() - read in Stata DTA file; frozen and will not support Stata formats
# after version 12.

# read.spss() - reads a file stored by the SPSS save or export commands; was
# orignally written in 2000 and has limited support for changes in SPSS format
# since.

# The haven package is a newer package that provides a lot of the same 
# functionality as the foreign package. Since it is in active development it 
# presumably will provide better support for changes in Stata and SPSS. Its 
# functions are similarly named as the foreign functions with underscores
# replacing the periods. For example, read_dta() and and read_spss().


# Read in multiple files --------------------------------------------------

# historical prices from seven stocks (downloaded from Google finance)

# Let's change our working directory to the "stocks" directory.
setwd("stocks")

# bbby - Bed Bath and Beyond
# flws - 1-800-Flowers.Com 
# foxa - Twenty-First Century Fox Inc
# ftd - FTD Companies Inc
# tfm - The Fresh Market Inc
# twx - Time Warner Inc
# viab - Viacom, Inc.

# I would like to read these files into R without having to do 7 instances of
# the read.csv() function. Below we'll look at two ways we can do this in R.

# First, we'll use the list.files() function. This function will show just files
# in the current working directory (or directory you specify) in the form of a 
# character vector.
list.files() 

stocks <- list.files() # save file names into vector
stocks[1] # file name of first file

# One method: use a "for" loop
for(i in seq_along(stocks)){
  assign(stocks[i], read.csv(file=stocks[i])) 
  }

# Let's break down the code above:
# seq_along() simply outputs the indices of a vector
seq_along(stocks)

# The "for" loop executes the code within, each time incrementing the value of 
# i. The value of i goes from 1 - 7. First time through loop, i = 1. Next time,
# i = 2, and so forth until i = 7.

# within the loop, we "assign" the value of read.csv(stocks[i]), which is a data
# frame, to an object named stocks[i].

# For example, when i = 1,
# assign(stocks[1], read.csv(stocks[1])) imports bbby.csv and names it "bbby.csv"
# For example, when i = 2,
# assign(stocks[2], read.csv(stocks[2])) imports flws.csv and names it "flws.csv"

# LOOPS IN R

# You may have heard that explicit loops in R should be avoided. They can be 
# expensive in time and memory use. But they can't always be avoided, 
# particularly with iterative calculations. Plus the benefit of clarity in your
# code can outweigh gains in efficiency in many circumstances.

# How could we read in the stock data above without using a for loop? We could 
# use the lapply() function. This applies a function to vector (or vectors) and
# returns a list.

# Below we apply the read.csv function to each element in the stocks vector. 
# Basically lapply takes each element of the stocks character vector and uses it
# as the file argument in read.csv. Below, lapply(stocks, read.csv) is basically
# shorthand for the following:
# allStocks <- list(read.csv("bbby.csv"), read.csv("flws.csv"), 
#                   read.csv("foxa.csv"), read.csv("ftd.csv"),
#                   read.csv("tfm.csv"), read.csv("twx.csv"),
#                   read.csv("viab.csv"))

# So lapply "applies" the read.csv function to each element of the stocks
# vector, which are actually CSV file names in our working directory.
allStocks <- lapply(stocks, read.csv)
str(allStocks) # list of 7 data frames

# We can name the list elements as follows:
names(allStocks)
names(allStocks) <- stocks
names(allStocks)
# Instead of 7 data frames we have 1 list of 7 data frames. Is this better?
# That's up to you.

# Something to think about: Notice these seven data frames all have the same
# column headers. What if we wanted a single data frame with all data and an
# extra column indicating which company the row of data refers to?

# We'll come back to that!

# move back up one level:
setwd("../")

# JSON --------------------------------------------------------------------

# JSON (JavaScript Object Notation) is a lightweight data-interchange format. In
# other words, it's a way to add structure to data. Learn more at
# http://www.json.org/.

# install.packages("jsonlite")
library(jsonlite)

# The jsonlite package is a JSON parser. That is, it takes JSON formatted data
# and turns into an R object, like a data frame.

# Example using NFLArrest API: http://nflarrest.com/api/ 

# An API is an "application programming interface" that provides a way for 
# programs to communicate with one another. The NFLArrest API allows programs 
# like my browser or R to easily fetch data in JSON format from NFLArrest.com. 
# Without an API I would have to do something like web-scraping or 
# copying-and-pasting.

# http://nflarrest.com/api/v1/crime returns "the most popular crimes in the
# NFL".

crime <- fromJSON("http://nflarrest.com/api/v1/crime")
class(crime)
str(crime)
head(crime)

# raw json
rawjson <- readLines("http://nflarrest.com/api/v1/crime", warn=FALSE)
class(rawjson)
length(rawjson)

# The readLines() function allows you to "read some or all text lines from a 
# connection." When it comes to URLs, it reads web site code. We'll encounter it
# again later in the course when we do some web scraping.

# R provides character manipulation functions that would allow us to parse this 
# "by hand", but obviously it's nicer to take advantage of existing packages
# that provide functions for this, such as fromJSON().

# That was a very simple API call. Often an API call allows you to provide 
# certain parameters like a date range or some sort of title. For example, we
# can use the API call above with parameters to select crimes in 2014.

url <- "http://nflarrest.com/api/v1/crime?start_date=2014-01-01&end_date=2014-12-31"
crime2014 <- fromJSON(url)
head(crime2014)

# Other sites that have APIs for getting data include Twitter, NY Times, Weather
# Underground, NASA and the Census Bureau. There are many others. Using an API 
# sometimes requires you to authenticate with an API key, which is basically a
# long string of characters and numbers.

rm(crime, rawjson, crime2014)


# Downloading files from the internet -------------------------------------

# When our source data is accessible via internet, we usually download it to our
# computer by clicking on a link. Furthermore, if it's in a zip file we then 
# extract it. THEN we open R and read it in. However, R comes with several 
# functions for downloading and decompressing archived files that can automate 
# these steps. Four functions of interest:

# download.file() -  download a file from the Internet
# untar() - Extract files from or list a tar archive
# unzip() - Extract files from or list a zip archive
# basename() - removes all of the path up to and including the last path separator

# Example: Download files for Linear Modeling in R workshop:

url <- "http://static.lib.virginia.edu/statlab/materials/workshops/LinearModelingR.zip"
download.file(url, destfile = basename(url))
unzip(basename(url), list = TRUE) # list files, but don't extract
unzip(basename(url), exdir = "LMinR") # extract files to new directory called "LMinR"
dir("LMinR/") # list contents of "LMinR" directory
prostate <- read.csv("LMinR/prostate.csv") # read in the CSV file I downloaded


# Reading Text from Files -------------------------------------------------

# Analyzing text is a big thing these days. R can do it, though many say a pure 
# programming language like Python is better for that sort of thing. I'm sure it
# is, but R gets the job done most of the time. Let's read in the screen play
# for the 1980 movie, Airplane!

url <- "http://www.awesomefilm.com/script/airplane.txt"

# First we'll use readLines; it literally reads the lines of the text file
airplane1 <- readLines(url)
airplane1[1:10]
tail(airplane1)
is.vector(airplane1)
length(airplane1) # 1258 lines

# We have the lines, but some of the lines are due to wrapping and don't 
# necessarily correspond to a character's lines. 

# All lines by Murdock
airplane1[grep("^Murdock", airplane1)] # find lines that begin with "Murdock"

# Due to wrapping, we're missing lots of dialog.

# We could also scan in the words (instead of lines) using scan()
airplane2 <- scan(url, what = "character") # what to specify what I'm scanning
airplane2[1:10]
tail(airplane2)
is.vector(airplane2)
length(airplane2) # 10264 words (or items separated by white space)

# scan() is actually quite powerful, and I admit I don't use it that often. It 
# has many arguments and a long help page. However I do find it handy for
# reading in text, especially if I'm interested in the words. 

# Top 10 most frequent words
sort(table(airplane2), decreasing = T)[1:10]

# Due to white space, we're counting things like ":" and "(" as words. Working
# with text data often requires very careful clean up. 


# Writing Data ------------------------------------------------------------

# We can write data to a csv file using the write.csv() function. The basic 
# syntax is write.csv(data, file, row.names) where data is typically a data 
# frame, file is the name of the csv file you want to create, and row.names is a
# logical setting indicating whether or not you want to include row names. I
# suggest setting to FALSE.

# Let's write the arrests data frame to a csv file:
write.csv(arrests, file="arrests.csv", row.names=FALSE)

# see also write.table()

# We can also save objects in our workspace (or memory) to a single RData file 
# with the save() function. The syntax is save(list, file) where list are the
# objects we want to save and file is the name of the RData file we wish to
# create. Use the ".Rda" or ".Rdata" extension for the RData file.

save("electionData", "weather", "arrests", "allStocks", file="datasets_L02.Rda")

# NOTE: we actually use datasets_L02.Rda in the next lecture.

# remove all data
rm(list=ls())
# How that works: ls() is a function that lists all objects in the workspace as 
# a vector. list is an argument that takes a vector of data. Hence rm(list=ls())
# clears the workspace. Verify:
ls()

# load an Rdata file using the load() function:
load("datasets_L02.Rda")

# Rda files efficiently store files and are very fast to load. Let's
# demonstrate.

# Recall the datBig.csv file. It's about 23 MB in size.
# The file.info function creates a data frame of file attributes.
fout <- file.info("../data/datBig.csv")
fout$size

# The size element shows file size in bytes.
# We can convert to Mb as follows:
(rsize <- fout$size / 1048576)

# Now let's read in the datBig.csv file:
dBig <- read.csv("datBig.csv", colClasses=c("factor","numeric","integer"),
                 comment.char = "", nrows=1e6)
print(object.size(dBig), units = "Mb")
# already being stored somewhat more efficiently

# Now save as an Rda file
save("dBig", file="dBig.Rda")
# get file size in bytes and convert to Mb
fout2 <- file.info("dBig.Rda")
(Rsize <- fout2$size / 1048576)

# Now stored as a 7 Mb file, and ready to quickly read into R!
rm(dBig)
system.time(load("dBig.Rda"))

