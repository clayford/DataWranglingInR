#' ---
#' title: "Data Wrangling in R: Regular Expressions"
#' author: "Clay Ford"
#' date: "Spring 2016"
#' output: pdf_document
#' ---

setwd("../data")
load("datasets_L06.Rda")

# This lecture uses the following packages:

# install.packages("stringr")
library(stringr)
# install.packages("qdapRegex")
library(qdapRegex)


# Intro -------------------------------------------------------------------

# A 'regular expression' is a pattern that describes a set of strings. 
# Examples:

# - all 5-digit numbers in a document 
# - all 5-digit numbers ending in 00
# - words spelled in ALL CAPS
# - words in brackets or delimiters [],<>,(),{}
# - words at the end of a sentence
# - all email addresses
# - dates in a certain format

# These are examples of string patterns. Regular Expressions are the language we
# use to describe the pattern. You should know, however, regular expressions are
# a language into itself. There are entire books devoted to regular expressions.

# Quote floating around internet: "Some people, when confronted with a problem, 
# think 'I know, I'll use regular expressions.' Now they have two problems." 
# Regular expressions can be tricky to get right, especially for complex
# patterns.

# We will only dabble in regular expressions. Key lesson: recognize when you 
# need a regular expression and know enough to cobble one together using your
# knowledge, wits and Google.

# Two PDF files you may want to download and save for reference:
# http://biostat.mc.vanderbilt.edu/wiki/pub/Main/SvetlanaEdenRFiles/regExprTalk.pdf
# http://gastonsanchez.com/Handling_and_Processing_Strings_in_R.pdf

# Good pages to print off/bookmark:
# http://www.cheatography.com/davechild/cheat-sheets/regular-expressions/
# http://regexlib.com/CheatSheet.aspx
# or just Google "regex cheatsheet"

# Good library book:
# Go to virgo, search for "Regular expressions cookbook"

# RegEx tutorials: 
# http://www.rexegg.com/
# http://www.regular-expressions.info/

# Regular Expression Basics -----------------------------------------------

# Regular expressions are composed of three components:

# (1) literal characters
# (2) modifiers (or metacharacters)
# (3) character classes

# (1) LITERAL CHARACTERS 

# These are the literal characters you want to match. If you want to find the
# word "factor", you search for "factor", the literal characters.

# (2) MODIFIERS

# Modifiers define patterns;
# meet the modifiers:
# $ * + . ? [ ] ^ { } | ( ) \

# precede these with double backslash (in R!) if you want to treat them as
# literal characters.

# ^  start of string
# $  end of string
# .  any character except new line
# *  0 or more
# +  1 or more
# ?  0 or 1
# |  or (alternative patterns)
# {} quantifier brackets: exactly {n}; at least {n,}; between {n,m}
# () group patterns together
# \  escape character (needs to be escaped itself in R: \\)
# [] character class brackets (not to be confused with R's subsetting brackets!)


# (3) CHARACTER CLASSES
# a range of characters to be matched;
# placed in brackets: []
# For example: [a-q] means all letters from a - q;
# [a-zA-Z] means all alphabetic characters;
# [0-9A-Za-z] means all alphanumeric characters;
# The ^ symbol means "not" when used in brackets, so [^abc] means "Not (a or b
# or c)"

# From R documentation: "Because their interpretation is locale- and 
# implementation-dependent, character ranges are best avoided." Good advice if
# you're sharing R code. Otherwise, fine to use on your own.

# PREDEFINED CHARACTER CLASSES

# [:lower:] - Lower-case letters in the current locale. [a-z]
# 
# [:upper:] - Upper-case letters in the current locale. [A-Z]
# 
# [:alpha:] - Alphabetic characters: [:lower:] and [:upper:]. [a-zA-Z]
# 
# [:digit:] - Digits: 0 1 2 3 4 5 6 7 8 9. [0-9]
# 
# [:alnum:] - Alphanumeric characters: [:alpha:] and [:digit:]. [0-9A-Za-z]
# 
# [:punct:] - Punctuation characters: ! " # $ % & ' ( ) * + , - . / : ; < = > ?
# @ [ \ ] ^ _ ` { | } ~.
# 
# [:graph:] - Graphical characters: [:alnum:] and [:punct:].
# 
# [:blank:] - Blank characters: space and tab, and possibly other
# locale-dependent characters such as non-breaking space.
# 
# [:space:] - Space characters: tab, newline, vertical tab, form feed, carriage
# return, space and possibly other locale-dependent characters.
# 
# [:print:] - Printable characters: [:alnum:], [:punct:] and space.

# Note that the brackets in these class names are part of the symbolic names, 
# and must be included in addition to the brackets delimiting the bracket list!

# More regex codes! (Yay! More stuff!) Be sure to escape that backslash!

# \b - Word boundary
# \d - any decimal digit
# \w - any word character
# \s - any white-space character
# \n - a New line

# see ?regex for an indepth overview of regular expressions.

# RegEx examples ----------------------------------------------------------

# Let's create some sample text to demonstrate regular expressions:

someText <- c("  here's a sentence", 
              "This is me typing at 2:02 in the morning",
              "Back in 1995 I was only 22.",
              "You saw 4 monkeys?", 
              "There are 10 kinds of people,    those that understand binary       
              and the other 9 that don't care",
              "Who likes pancakes? I do. I really really like pancakes!", 
              "     <strong>Bolded text is bold and daring</strong>",
              "figure1.jpg", "cover.jpg", "report.pdf", "log.txt",
              "I'm a one man wolfpack and I weigh 222",
              "OMG, a 3-eyed cyclops!!!",
              "2112 is an awesome album.",
              "2222 is my PIN")
someText

# Examples of SUPER BASIC regex patterns:

# find elements in vector beginning with 1 or more spaces
grep("^ +", someText, value=T) 
grep("^[[:blank:]]+", someText, value=T) 

# find elements containing a question mark; need to "escape" the "?"
grep("\\?", someText, value=T) 

# find elements ending with a question mark
grep("\\?$", someText, value=T) 

# find elements containing one or more numbers
grep("[0-9]+", someText, value=T) 
grep("[[:digit:]]+", someText, value=T) 

# find elements containing numbers with 2 digits
grep("[0-9]{2}", someText, value=T)
grep("[[:digit:]]{2}", someText, value=T) 

# text ending with .jpg; need to escape the "."
grep("\\.jpg$", someText, value=T) 

# text ending with a 3=character file extension
grep("\\.[[:alpha:]]{3}$", someText, value=T) 
grep("\\.\\w{3}$", someText, value=T) 

# text beginning with only letters, and containing only letters, ending in .jpg
grep("^[a-zA-Z]+\\.jpg", someText, value=T)
grep("^[[:alpha:]]+\\.jpg", someText, value=T)

# text containing two consecutive "really "
grep("(really ){2}",someText, value=T) 

# text containing two or more !
grep("!{2,}",someText, value=T) 

# Contraction beginning with 3 letters
grep(" [[:alpha:]]{3}'", someText, value = T)
grep("\\b[[:alpha:]]{3}'", someText, value = T)

# text with 3-character words
grep("\\b\\w{3}\\b", someText, value = T)

# text with 3-character words but no file names
grep("\\b\\w{3}\\b[^[:punct:]]", someText, value = T)

# text with ALL CAPS (two or more CAPS)
grep("\\b[[:upper:]]{2,}\\b", someText, value = T)

# text with a new line
grep("\\n", someText, value = T)

# matching 0 or more times
grep("2*2", someText, value = T)

# matching 1 or more times
grep("2+2", someText, value = T)


# Search/Replace with RegEx -----------------------------------------------

# Recall sub() and gsub() functions. These perform replacement of the first and 
# all matches respectively. In a previous lecture we used them to search/replace
# literal strings. Now let's use them with regular expressions. A few examples:

# Replace Repeated Whitespace with a Single Space
gsub(" +"," ", someText)
gsub("\\s+"," ",someText) # removes \n!

# Trim Leading and Trailing Whitespace: 
gsub("^ +| +$","", someText)

# Or better yet, just use the built-in function
trimws(someText)

# Replace a new line with a space
gsub("\\n"," ",someText)

# Remove HTML/XML tags (basic)
# "<" followed by anything but ">" and ending with ">" 
gsub("<[^>]*>","",someText)

# Or better yet, just use the qdapRegex function rm_angle()
rm_angle(someText)


# Extract with RegEx ------------------------------------------------------

# The base R functions regexpr() and gregexpr() along with regmatches() can be 
# used to extract character matches, but I find the str_extract() and 
# str_extract_all() in the stringr package to be easier and faster to use. 
# str_extract() extracts first piece of a string that matches a pattern while
# str_extract_all() extracts all matches. A few examples:

# Extract one- or two-digit numbers:
# first match
str_extract(someText, "[0-9]{1,2}") 
# all matches; returns a list
str_extract_all(someText, "[0-9]{1,2}") 
# can use the base R function unlist() function to get just the numbers in a
# vector:
unlist(str_extract_all(someText, "[0-9]{1,2}"))

# Extract a string that contains a . followed by 3 lower-case letters (file
# extensions)
str_extract(someText,"\\.[a-z]{3}")

# just the file extenions without a period (not very elegant but works)
str_extract(someText,"(jpg|tif|pdf|txt)$")

# Extract text beginning with only letters, and containing only letters,
# ending in .jpg
str_extract(someText, "^[a-z]+\\.jpg")

# to get just the text
tmp <- str_extract(someText, "^[a-z]+\\.jpg")
tmp[!is.na(tmp)]


# Web scraping ------------------------------------------------------------

# Regular Expressions can be very helpful when doing web scraping. Let's scrape 
# some data and demonstrate. Simply give a URL as an argument to the readLines()
# function. The readLines() function reads in text lines. The following reads in
# the HTML code of a web page into a single vector.

# 113th Congress Senate Bills: first 100 results.
senate_bills <- readLines("http://thomas.loc.gov/cgi-bin/bdquery/d?d113:0:./list/bss/d113SN.lst:")

# Notice senate_bills is a vector, not a data frame. Each element of text
# corresponds to one line of HTML code:
senate_bills[1:10]

# We'd like to create a data frame that includes bill number, bill title,
# sponsor, and number of co-sponsors.

# In the HTML we see that bill number, title, and sponsor are in lines that
# begin like this: "<p><b>15.</b>". We can use regular expressions to find all
# 1-3 digit numbers followed by a period and </b>.


# grep() can find the indices of such patterns,
k <- grep("[0-9]{1,3}\\.</b>", senate_bills)
k[1:4]
# Use k to subset the data
temp <- senate_bills[k]
head(temp)
tail(temp)

# Now replace the HTML tags with space
temp <- gsub("<[^>]*>", " ",temp)
head(temp)
tail(temp)

# break vector elements by ":"
temp <- strsplit(temp,":")

# Let's see what we have so far:
head(temp)

# To get the bill numbers we can pull out the first element of each list
# component as follows:
bill <- sapply(temp,function(x)x[1])
head(bill)

# Now we can use str_extract() to pull out the bill numbers. I've decided to
# keep the "S":
bill <-str_extract(bill, "S\\.[0-9]{1,3}")
head(bill)


# Now let's get the bill title. It's in the second element.
temp[[1]]
# pull out second element of each list component
title <- sapply(temp,function(x)x[2])
title[1:4]
# get rid of " Sponsor" at end
title <- gsub(" Sponsor$","",title)
# get rid of leading and trailing spaces
title <- trimws(title) 
head(title)

# Now get the bill sponsor. It's in the third element.
temp[[1]]
sponsor <- sapply(temp,function(x)x[3])
sponsor <- trimws(sponsor) # get rid of leading spaces
head(sponsor)

# Get number of cosponsors by first finding those vector elements that contain 
# the string "Cosponsors". Have to be careful; not all bills have Cosponsors
# (ie, Cosponsors (None) ) but all have the word "Cosponsors".

k <- grep("Cosponsors",senate_bills)
# subset vector to contain only those matching elements
temp <- senate_bills[k]
head(temp)

# Now extract number of cosponsors; either None or a 1-2 digit number.
cosponsors <- str_extract(temp, pattern = "\\([[:alnum:]]{1,4}\\)")
# Get rid of parentheses
cosponsors <- gsub(pattern = "[\\(|\\)]", replacement = "", cosponsors)
# Replace "None" with 0 and convert to numeric
cosponsors <- as.numeric(gsub("None",0,cosponsors))
summary(cosponsors)

# And finally create data frame
senate_bills <- data.frame(bill, title, sponsor, cosponsors, 
                           stringsAsFactors = FALSE)
head(senate_bills)

# What if we wanted to do this for all results? We have to iterate through the URLs.

# http://thomas.loc.gov/cgi-bin/bdquery/d?d113:0:./list/bss/d113SN.lst:[[o]]&items=100&
# http://thomas.loc.gov/cgi-bin/bdquery/d?d113:100:./list/bss/d113SN.lst:[[o]]&items=100&
# http://thomas.loc.gov/cgi-bin/bdquery/d?d113:200:./list/bss/d113SN.lst:[[o]]&items=100&
# ...  
# http://thomas.loc.gov/cgi-bin/bdquery/d?d113:3000:./list/bss/d113SN.lst:[[o]]&items=100&

# We also may want to create a data frame in advance to store the data
  
SenateBills <- data.frame(bill=character(3020), title=character(3020), 
                          sponsor=character(3020), 
                          cosponsors=numeric(3020), 
                          stringsAsFactors = FALSE)

# Now cycle through the URLS using the code from above. I suppose ideally I
# would determine the upper bound of my sequence (3000) programmatically, but
# this is a one-off for the 113th congress so I'm cutting myself some slack.

for(i in seq(0,3000,100)){
  senate_bills <- readLines(paste0("http://thomas.loc.gov/cgi-bin/bdquery/d?d113:",i,":./list/bss/d113SN.lst:"))
  # bill number
  k <- grep("[0-9]{1,3}\\.</b>", senate_bills)
  temp <- senate_bills[k]
  temp <- gsub("<[^>]*>", " ",temp)
  temp <- strsplit(temp,":")
  bill <- sapply(temp,function(x)x[1])
  bill <- str_extract(bill, "S\\.[0-9]{1,4}") # need to increase to 4 digits
  # title
  title <- sapply(temp,function(x)x[2])
  title <- gsub(" Sponsor$","",title)
  title <- trimws(title) 
  # sponsor
  sponsor <- sapply(temp,function(x)x[3])
  sponsor <- trimws(sponsor) 
  # coponsors
  k <- grep("Cosponsors",senate_bills)
  temp <- senate_bills[k]
  cosponsors <- str_extract(temp, pattern = "\\([[:alnum:]]{1,4}\\)")
  cosponsors <- gsub(pattern = "[\\(|\\)]", replacement = "", cosponsors)
  cosponsors <- as.numeric(gsub("None",0,cosponsors))
  # add to data frame
  rows <- (i+1):(i+length(k))
  SenateBills[rows,] <- data.frame(bill, title, sponsor, cosponsors, stringsAsFactors = FALSE)
}


# For another web scraping tutorial of mine, see:
# https://github.com/UVa-R-Users-Group/meetup/tree/master/2014-10-07-web-scraping

# The rvest package by Hadley Wickham allows you to "Easily Harvest (Scrape) Web
# Pages":
# http://blog.rstudio.org/2014/11/24/rvest-easy-web-scraping-with-r/

# The XML package also has some functions for converting HTML tables to data
# frames.


# RegEx within data frames ------------------------------------------------

# Recall our allStocks data. We wanted to add a column indicating which stock 
# each row belongs to. We can use gsub() and regular expressions to easily
# do this.
head(allStocks)

# Notice the row name contains the name of the stock. We can extract the row
# names and formally add them to the data frame using the rownames() function.

# first three row names:
rownames(allStocks)[1:3]

# extract all row names and add to data frame:
allStocks$Stock <- rownames(allStocks)
head(allStocks)

# Let's reset the row names:
rownames(allStocks) <- NULL
head(allStocks)

# Now we find the pattern "\\.csv\\.[0-9]{1,3}" and replace with nothing. Recall
# that "." is metacharacter that has to be escaped. [0-9]{1,3} translates to all
# numbers ranging from 0 - 999.
allStocks$Stock <- gsub(pattern = "\\.csv\\.[0-9]{1,3}", 
                        replacement = "", 
                        allStocks$Stock)

# and let's make our new variable a factor:
allStocks$Stock <- factor(allStocks$Stock)
head(allStocks)
tail(allStocks)
summary(allStocks$Stock)

# While we're at it, let's fix the Date. (Currently a factor.)
allStocks$Date <- as.Date(allStocks$Date, format="%d-%b-%y")

# And just for fun, graph closing price over time for all stocks one on graph:
library(ggplot2)
ggplot(allStocks, aes(x=Date, y = Close, color=Stock)) + geom_line()


# Now let's finish cleaning up the 2012 election data!
names(electionData)
# I want to drop everything to the right of the "NA.34 NA" column. Frankly I'm
# not sure what those columns contain.

# Get the column number of the column with header "NA.34 NA"
fir <- grep("NA.34 NA", names(electionData))
fir
# get the column number of the last column in the data frame
las <- ncol(electionData)
las
# Now subset the data frame; keep all columns except 72-82
electionData <- electionData[,-c(fir:las)]

# drop columns with names of "NA.1, NA.2, etc"; these are proportions. I can
# always derive them later if I want them.
ind <- grep("NA\\.[0-9]{1,2}", names(electionData))
ind
electionData <- electionData[,-ind]

# and some final clean up
names(electionData)[3] <- "Total.Popular.Vote" 
names(electionData)[5] <- "Elec Vote R" 
electionData$"Pop Vote D" <- NULL
rownames(electionData) <- NULL

# still some lingering character columns
which(sapply(electionData, is.character))
# convert to numeric
electionData[,2:6] <- sapply(electionData[,2:6], as.numeric)

# Now our election data contains only number of votes. 


# Another extended example ------------------------------------------------

# Let's add Occupation names to the arrests data. Recall that the Occup columns 
# contains numeric codes for Occupations. I'd like to make that column a factor
# where the codes are associated with levels that define the code number.
arrests$Occup[1:5]


# First we read in a document that contains Occupation code numbers and the
# occupation name. I created this from the codebook that accompanied this data.
oc <- readLines("../data/00049-Occupation-codes.txt", warn=FALSE)

# trim whitespace
oc <- trimws(oc)

# Notice all code numbers are in the first three positions. Let's use stringr 
# for the str_extract() function. Notice we need to convert to integer to match
# the integer codes in the arrests data frame.
codeNums <- as.integer(str_extract(string = oc, pattern = "^[0-9]{3}"))

# Getting the code names is a little harder. There are probably a dozen 
# different ways to proceed from this point on, but here's how I decided to do
# it. Basically extract everything except numbers.
codeNames <- trimws(str_extract(string = oc, pattern = "[^[:digit:]]+"))
head(codeNames)
tail(codeNames)

# Now I can make Occup a factor with levels equal to codeNums and labels equal 
# to codeNames. I'm going to make a new column so we can compare to the original
# column.
arrests$Occup2 <- factor(arrests$Occup, levels = codeNums, labels = codeNames)

# some quick counts; they seem to match our source file
head(summary(arrests$Occup2))
tail(summary(arrests$Occup2))

# Apparently there are no codes in the data for Cannot Read Film (997) or None
# listed (998) despite them being listed in the code book.
nrow(subset(arrests, Occup %in% c(997,998)))

# Which codes are we using that don't have matches in the data?
setdiff(codeNums,arrests$Occup)

# 174 = secret society; codebook reports 0 so that makes sense.

# Which codes are in the data that we don't have matches for in codeNums?
setdiff(arrests$Occup, codeNums)

# 1, 2, and 178 are not listed in the codebook!

k <- setdiff(arrests$Occup, codeNums)
head(subset(arrests, Occup %in% k, select = c("Occup","Occup2")))
nrow(subset(arrests, Occup %in% k))
# 403 records with Occup code that doesn't match codebook

# Bottom line: this data, as provided by ICPSR, is a bit dirty.

# qdapRegex package -------------------------------------------------------

# The qdapRegex package has some pre-defined functions for Regular Expression 
# Removal, Extraction, and Replacement. Let's explore some of them by way of an
# example.

# I have a text file of studio albums by the Canadian rock band, Rush. Read it
# in:
rushSA <- readLines("rush_albums.txt")
rushSA

# I'd like to make a data frame with two columns: album title and year released.
# We'll use qdapRegex functions to do this.

# First let's trim the white space:
rushSA <- trimws(rushSA)

# The qdapRegex package has a function called rm_between() that will 
# Remove/Replace/Extract Strings Between 2 Markers. I want to use it to extract 
# album release year between parentheses. Note I have to use the extract=TRUE
# argument:
year <- rm_between(rushSA, left = "(", right=")", extract=TRUE)
# That returns a list; I can use unlist() to make it a vector:
year <- unlist(year)
year

# I need to remove the string ", cover album". Could use gsub() to find and
# replace with nothing, but a more general approach would be to extract all
# numbers. Remember the tidyr helper function, extract_numeric?

year <- tidyr::extract_numeric(year)
year

# Now get the album titles; this time use the rm_between() function without the 
# extract=TRUE argument. This removes everything between the (), including the
# parentheses.
album <- rm_between(rushSA, left = "(", right=")")
album

# And now our data frame: 
rushStudioAlbums <- data.frame(year, album)
head(rushStudioAlbums)

# There is also a package called stringi that bills itself as "THE string 
# processing package for R". As I understand it, stringr is wrapper for stringi.
# Learn more :http://www.rexamine.com/resources/stringi/

# save data for next set of lecture notes
save(list=c("electionData", "weather", "arrests", "allStocks", "popVa","airplane",
            "SenateBills"), file="../data/datasets_L07.Rda")
