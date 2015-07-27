#' ---
#' title: "Data Wrangling in R: Reading and Writing Data"
#' author: "Clay Ford"
#' date: "Spring 2015"
#' output: html_document
#' ---


# We usually import, or read, data into R. Data come in many formats: CSV, 
# ASCII, XLSX, JSON, DTA (Stata), SAV (SPSS), XML, HTML, etc. The format of the
# data dictate what function we need to use. 

# In this lecture we cover the following:
# - CSV
# - ASCII/TXT
# - XLSX

setwd("../data/")
# Note: two dots (..) mean "go back up one level in the directory"


# read.table and read.csv -------------------------------------------------

# read.table() - for reading in data in a tabular format separated by
# "separators" such as spaces, commas, tabs; reads in data and returns a data
# frame

# Example: 2013 Charlottesville weather
# http://www.wunderground.com/history/airport/KCHO/2014/2/11/CustomHistory.html

# cville_weather_2013.csv is a CSV file. CSV = Comma Separated Value. That means
# fields are separated by commas. CSV files can be viewed in Excel or any text 
# editor. This particular file also has column headers. In read.table() we need
# to specify these properties with the header and sep arguments.
weather <- read.table("cville_weather_2013.csv", header=TRUE, sep=",")
str(weather)

# read.csv() is the same as read.table() with different defaults (header = TRUE,
# sep = ","). It's easier to use read.csv() when reading CSV files:
weather <- read.csv("cville_weather_2013.csv")

# After reading in data to a data frame, it's a good idea to inspect the top and
# bottom of the data frame (ie, the first few and last few rows). Two basic
# functions are head() and tail(), which show the first and last 6 records,
# respectively. Use the n= argument to change the number of rows displayed
head(weather)
tail(weather, n=3)

# NOTE: head() and tail() work on vectors as well:
tail(weather$Max.TemperatureF, n=5)

# read.table and read.csv have many arguments! See help(read.table) 
# Arguments of note: 

# stringsAsFactors - logical vector indicating whether or not to read character 
# vectors as factors. Defaults to TRUE.

# colClasses - A character vector of classes to be assumed for the columns. 

# nrows - the maximum number of rows to read in. Negative and other invalid
# values are ignored.
 
# skip - the number of lines of the data file to skip before beginning to read
# data.

# from the help page: These functions can use a surprising amount of memory when
# reading large files.
# 
# Less memory will be used if colClasses is specified: "logical", "integer",
# "numeric", "character", "factor", "Date", "POSIXct", or NULL to skip a column.
# 
# Using nrows, even as a mild over-estimate, will help memory usage (but not speed).
# 
# Using comment.char = "" will be appreciably faster than the read.table 
# default (comment.char = "#")

# example of reading large file
# datBig: 1,000,000 records with 3 columns: charcter, numeric, integer

# Let's compare times to read in a large data set using using defaults and then
# specifying arguments. The system.time() function tells you how long R code
# takes to run.

# default settings
system.time(test <- read.csv("datBig.csv"))
# with colClasses, nrows and  comment.char specified:
system.time(test <- read.csv("datBig.csv", colClasses=c("factor","numeric","integer"),
                             comment.char = "", nrows=1e6))


# QUESTION: What if you have lots of columns? Read in a few lines, get the class
# of the columns, save to a vector, then read in all the lines using the vector
# classes in the colClasses argument.

# Let's do the weather data.
weather <- read.csv("cville_weather_2013.csv", nrows=10)
classes <- sapply(weather, class)

# sapply(weather, class) applies the class() function to each column of the 
# weather data frame and returns a vector stating the class of each column.
# Don't worry we will delve deeper into apply() functions later!
classes

# we can use this vector of classes in the colClasses argument 
weather <- read.csv("cville_weather_2013.csv", colClasses=classes)

# For a CSV file with 365 records it doesn't make much difference, but it comes
# in handy with larger files.

# QUESTION: How can I tell how many rows I have in my data? A little hack:
# Combine length() and count.fields() functions, like so:
length(count.fields(file="cville_weather_2013.csv", sep=","))
# The next file has data with no separators:
length(count.fields(file="00049-0001-Data.txt", sep=""))

# read.fwf ----------------------------------------------------------------

# Reads a table of fixed width formatted data into a data frame;
# widths = widths of the fixed-width fields;

# Example: Analysis of Arrests in Paris, June 1848
# contains descriptions of 11,616 individuals arrested for alledged 
# participation in the Parisian insurrection.

# Source: http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/49
# Data is ASCII in fixed-width format;
# codebook tells you column that variable starts and how wide it is;

arrests <- read.fwf(file="00049-0001-Data.txt",
                    widths=c(5,1,2,3,2,2,1,2,1,2,2,2,1,1,1,2,18))
head(arrests)

# Notice R added column names: V1, V2, V3, ...

# We can add the real column names using the names() function. By itself the
# names function returns the column names of a data frame, like so:
names(arrests)

# But calling the names() function on a data frame and simultaneously assigning 
# to it a vector of character names will assign those names to the data frame.
# This is best explained with an example

# First make a vector of names from codebook:
cnames <- c("ID","Source","Constant","Occup","DeptBorn","CommuneBorn","Sex",
            "Age","MaritalStatus","Children","Arrondissement","QuarterResided",
            "FirstJudicial","FinalJudicial", "FirstDecision", "FinalOutcome",
            "CommuneName")
cnames

# Now call the names() function on arrests while assigning cnames to the result:
names(arrests) <- cnames
head(arrests)

# 99 children? No. 99 = No information given according to codebook. You'll often
# see missing data coded as "99" or "999" or something similar. You can make R 
# convert these values to NA using the na.strings argument in your read.xxx()
# function. For example:

# arrests <- read.fwf("00049-0001-Data.txt",
#                     widths=c(5,1,2,3,2,2,1,2,1,2,2,2,1,1,1,2,18), na.strings="99")

# Notice that other than Age and CommuneName, everything in arrests is a code.
str(arrests)

# Excel files -------------------------------------------------------------

# If possible and convenient, I recommend saving Excel files as CSV before 
# reading into R. However there are packages for reading Excel files directly
# into R.

# read.xlsx() in xlsx package; requires Java
# read.xls() in gdata package; requires Perl
# read_excel() in readxl package; newer package

# I will demonstrate the xlsx package:
# install.packages("xlsx") # only need to do this once
library(xlsx)

# Example: 2012 Presidential Election Data
# need to specify worksheet name or number: sheetName="State";
# can also use sheetIndex=2
electionData <- read.xlsx("Pres_Election_Data_2012.xlsx", sheetName="State")
str(electionData)
# It's a mess; we'll come back to this later


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
# in the current working directory (not other directories) in the form of a
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
# i =2, and so forth until i = 7.

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
str(allStocks)

# We can name the list elements as follows:
names(allStocks) <- stocks

# Instead of 7 data frames we have 1 list of 7 data frames. Is this better?
# That's up to you.

# Something to think about: Notice these seven data frames all have the same
# column headers. What if we wanted a single data frame with all data and an
# extra column indicating which company the row of data refers to?

# We'll come back to that!


# Writing Data ------------------------------------------------------------

# set working directort back to "data"
setwd("../")

# We can write data to a csv file using the write.csv() function. The basic 
# syntax is write.csv(data, file, row.names) where data is typically a data 
# frame, file is the name of the csv file you want to create, and row.names is a
# logical setting indicating whether or not you want to include row names. I
# suggest setting to FALSE.

# Let's write the arrests data frame to a csv file:
write.csv(arrests, file="arrests.csv", row.names=FALSE)

# see also write.table()

# We can also save objects in our workspace (or memory) to a single RData file 
# with the save() function. The syntax is save(list, file) where list is a 
# vector of objects we want to save and file is the name of the RData file we
# wish to create. Use the ".Rda" or ".Rdata" extension for the RData file.

save(list=c("electionData", "weather", "arrests", "allStocks"), file="datasets_L02.Rda")

# NOTE: we actually use datasets_L02.Rda in the next lecture.

# remove all data
rm(list=ls())
# How that works: ls() is a function that lists all objects in the workspace as 
# a vector. list is an argument that takes a vector of data. Hence rm(list=ls())
# clears the workspace. Verify:
ls()

# load an Rdata file using the load() function:
load("datasets_L02.Rda")


 
