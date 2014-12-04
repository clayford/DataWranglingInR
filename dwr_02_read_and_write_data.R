#' ---
#' title: "Data Wrangling in R: Reading and Writing Data"
#' author: "Clay Ford"
#' date: "Spring 2015"
#' output: pdf_document
#' ---



# read.table and read.csv -------------------------------------------------


# We usually import, or read, data into R. Data come in many formats: CSV, 
# ASCII, XLSX, JSON, DTA (Stata), XML, HTML, etc. The format of the data dictate
# what function we need to use.

# data.table() - for reading in data in a tabular format separated by
# "separators" such as spaces, commas, tabs; reads in data and returns a data
# frame

setwd("../data/")

# Example: 2013 Charlottesville weather
# http://www.wunderground.com/history/airport/KCHO/2014/2/11/CustomHistory.html
weather <- read.table("cville_weather_2013.csv", header=TRUE, sep=",")
str(weather)

# read.csv() is the same as read.table() with different defaults;
# Easier to use read.csv() when reading CSV files: 
weather <- read.csv("cville_weather_2013.csv")

# read.table and read.csv have many arguments! See help(read.table) 
# Arguments of note: 

# stringsAsFactors - logical indicating whether or not to read character
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
# default.

# example of reading large file
# datBig: 1,000,000 records with 3 columns: charcter, numeric, integer

# compare times to read in data
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
# weather data frame and returns a vector stating the class of each column
classes

# we can use this vector of classes in the colClasses argument 
weather <- read.csv("cville_weather_2013.csv", colClasses=classes)

# For a CSV file with 365 records it doesn't make much difference, but it comes
# in handy with larger files.

# QUESTION: How can I tell how many rows I have in my data? A little hack:
# Combine length() and count.fields() functions
length(count.fields("cville_weather_2013.csv", sep=","))

# can also the Unix tool wc if you know your way around a terminal. 

# read.fwf ----------------------------------------------------------------

# Reads a table of fixed width formatted data into a data frame
# widths = widths of the fixed-width fields;

# Example: Analysis of Arrests in Paris, June 1848
# contains descriptions of 11,616 individuals arrested for alledged 
# participation in the Parisian insurrection.

# Source: http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/49
# Data is ASCII in fixed-width format;
# codebook tells you column that variable starts and how wide it is;

arrests <- read.fwf("ICPSR_00049/ICPSR_00049/DS0001/00049-0001-Data.txt",
                    widths=c(5,1,2,3,2,2,1,2,1,2,2,2,1,1,1,2,18))
head(arrests)

# add column names
names(arrests)
# names from codebook
names(arrests) <- c("ID","Source","Constant","Occup","DeptBorn","CommuneBorn","Sex",
                    "Age","MaritalStatus","Children","Arrondissement","QuarterResided",
                    "FirstJudicial","FinalJudicial", "FirstDecision", "FinalOutcome",
                    "CommuneName")
head(arrests)
# 99 children? No. 99 = No information given according to codebook
str(arrests)

# Other than Age and CommuneName, everything in arrests is a code.


# Excel files -------------------------------------------------------------

# If possible and convenient, I recommend saving Excel files as CSV before 
# reading into R. However there are packages for reading Excel files directly
# into R.

# read.xlsx() in xlsx package; requires Java
# read.xls() in gdata package; requires Perl
# install.packages("xlsx") # only need to do this once
library(xlsx)

# Example: 2012 Presidential Election Data
# need to specify worksheet name or number: sheetName="State"
# can also use sheetIndex=2
electionData <- read.xlsx("Pres_Election_Data_2012.xlsx", sheetName="State")
str(electionData)
# It's a mess; we'll come back to this later


# The foreign package -----------------------------------------------------

# The foreign package provides functions for reading data from other programs
# such as Stata, SPSS, Minitab. The foreign package comes with R.

# read.dta() - read in Stata DTA file; frozen and will not support Stata formats
# after version 12.

# read.spss() - reads a file stored by the SPSS save or export commands; was
# orignally written in 2000 and has limited support for changes in SPSS format
# since.



# Read in multiple files --------------------------------------------------

# historical prices from four stocks (downloaded from Google finance)
setwd("stocks/")
list.files() # see just files in directory (not other directories)
stocks <- list.files() # save file names into vector
stocks[1] # file name of first file

# quick and dirty: use a for loop
for(i in seq_along(stocks)){
  assign(stocks[i], read.csv(stocks[i])) 
}

# Let's break down the code above:
# seq_along(stocks) simply outputs the vector indices of stocks 
seq_along(stocks)

# the "for loop" executes the code within, each time incrementing the value of
# i.

# within the loop, we "assign" value of read.csv(stocks[i]) to an object named
# stocks[i]. 

# For example...
stocks[1]
# assign(stocks[1], read.csv(stocks[1])) imports CSV data and names it "bbby.csv"

# LOOPS IN R

# You may have heard that explicit loops in R should be avoided. They can be 
# expensive in time and memory use. But they can't always be avoided, 
# particularly with iterative calculations. Plus the benefit of clarity in your
# code can outweigh gains in efficiency in many circumstances.

# How could we read in the stock data above without using a for loop? We could
# use the lapply() function. This applies a function to a vector and returns a list.

# apply the read.csv function to each element in the stocks vector
allStocks <- lapply(stocks, read.csv)
str(allStocks)
# add names to data frames
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

# write a csv file of the arrest data:
write.csv(arrests,file="arrests.csv", row.names=FALSE)
# row.names=FALSE prevents the row numbers in R from being added as a column.
# see also write.table()

# save R objects; use ".Rda" or ".Rdata" extension
save(list=c("electionData", "weather", "arrests", "allStocks"), file="datasets_L02.Rda")

# remove all data
rm(list=ls())
ls()
# load data sets
load("datasets_L01.Rda")
ls()
