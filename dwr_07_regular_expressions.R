#' ---
#' title: "Data Wrangling in R: Regular Expressions"
#' author: "Clay Ford"
#' date: "Spring 2015"
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

# A 'regular expression' is a pattern that describes a set of strings. For 
# example, say you want to find all 5-digit numbers in a document, or find all 5
# digit numbers ending in "00", or find all 5 digit numbers at the end of a 
# sentence. These are examples of string patterns. Regular Expressions are the
# language we use to describe the pattern.

# You should know, however, regular expressions are a language into itself.
# There are entire books devoted to regular expressions. 

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
# Good page to print off/bookmark:
# http://www.cheatography.com/davechild/cheat-sheets/regular-expressions/
# Good library book:
# Go to virgo, search for "Regular expressions cookbook"

# RegEx tutorial: http://www.rexegg.com/

# Regular Expression Basics -----------------------------------------------

# Regular expressions are composed of three components:

# (1) literal characters
# (2) modifiers (or metacharacters)
# (3) character classes

# (1) LITERAL CHARACTERS 

# These are the literal characters you want to match. If you want to find the
# word "good", you search for "good", the literal characters.

# (2) MODIFIERS

# Modifiers define patterns;
# meet the modifiers:
# $ * + . ? [ ] ^ { } | ( ) \
# precede these with double backslash if you want to treat them as regular text

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

# (3) CHARACTER CLASSES
# a range of characters to be matched;
# placed in brackets: []
# For example: [a-q] means all letters from a - q
# The ^ symbol means "not" when used in brackets, so [^abc] means "Not (a or b
# or c)"

# see help(regex) for an indepth overview of regular expressions.

# RegEx examples ----------------------------------------------------------

# Let's create some sample text to demonstrate regular expressions:

someText <- c("  here's a sentence", 
              "This is me typing  ", 
              "You saw 4 monkeys?", 
              "There are 10 kinds of people,    those that understand binary       
              and the other 9 that don't care",
              "Who likes pancakes? I do. I really really like pancakes!", 
              "     <strong>Bolded text is bold and daring</strong>",
              "figure1.jpg", 
              "cover.jpg")
someText

# find elements in vector beginning with 1 or more spaces
grep("^ +", someText, value=T) 
# find elements containing a question mark; need to "escape" the "?"
grep("\\?", someText, value=T) 
# find elements ending with a question mark
grep("\\?$", someText, value=T) 
# find elements containing one or more numbers
grep("[0-9]+", someText, value=T) 
# find elements containing numbers with 2 digits
grep("[0-9]{2}", someText, value=T)
# text ending with .jpg; need to escape the "."
grep("\\.jpg$", someText, value=T) 
# text beginning with only letters, and containing only letters, ending in .jpg
grep("^[a-zA-Z]+\\.jpg", someText, value=T)
# text containing two consecutive "really "
grep("(really ){2}",someText, value=T) 


# Search/Replace with RegEx -----------------------------------------------

# Recall sub() and gsub() functions. These perform replacement of the first and 
# all matches respectively. In a previous lecture we used them to search/replace
# literal strings. Now let's use them with regular expressions. A few examples:

# (1) Replace Repeated Whitespace with a Single Space
gsub(" +"," ", someText)

# (2) Trim Leading and Trailing Whitespace: 
gsub("^ +| +$","", someText)

# Or better yet, just use the stringr function str_trim()
str_trim(someText)

# (3) Remove HTML/XML tags (basic)
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

# (1) extract one- or two-digit numbers:
# first match
str_extract(someText, "[0-9]{1,2}") 
# all matches; returns a list
str_extract_all(someText, "[0-9]{1,2}") 
# can use the base R function unlist() function to get just the numbers in a
# vector:
unlist(str_extract_all(someText, "[0-9]{1,2}"))

# (2) extract a string that contains a . followed by 3 lower-case letters:
str_extract(someText,"\\.[a-z]{3}")

# (3) extract text beginning with only letters, and containing only letters,
# ending in .jpg
str_extract(someText, "^[a-z]+\\.jpg")


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
temp[1:4]
# Now replace the HTML tags with space
temp <- gsub("<[^>]*>", " ",temp)
temp[1:4]
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


# Now let's get the bill title:
temp[[1]][2]
# pull out second element of each list component
title <- sapply(temp,function(x)x[2])
title[1:4]
# get rid of " Sponsor" at end
title <- gsub(" Sponsor$","",title)
# get rid of leading and trailing spaces
title <- str_trim(title) 
title[1:4]

# Now get the bill sponsor:
temp[[1]][3]
sponsor <- sapply(temp,function(x)x[3])
sponsor <- str_trim(sponsor) # get rid of leading spaces
sponsor[1:3]

# Get number of cosponsors by first finding those vector elements that contain 
# the string "Cosponsors". Have to be careful; not all bills have Cosponsors,
# but all have the word "Cosponsors".

k <- grep("Cosponsors",senate_bills)
# subset vector to contain only those matching elements
temp <- senate_bills[k]
head(temp)
# split on "</a>"
temp <- strsplit(temp,"Cosponsors")
head(temp)
# Number of cosponsors in second element
cosponsors <- sapply(temp,function(x)x[2])
# let's view them all:
cosponsors

# Appears we need to extract one- to two-digit numbers or the word "None":
cosponsors <- str_extract(cosponsors,"[0-9]{1,3}|None")
cosponsors

# Replace "None" with 0 and convert to numeric
cosponsors <- as.numeric(gsub("None",0,cosponsors))
summary(cosponsors)

# And finally create data frame
senate_bills <- data.frame(bill, title, sponsor, cosponsors)
head(senate_bills)

# For another web scraping tutorial of mine, see:
# https://github.com/UVa-R-Users-Group/meetup/tree/master/2014-10-07-web-scraping

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

# While we're at it, let's fix the Date. (Currently a factor.)
allStocks$Date <- as.Date(allStocks$Date, format="%d-%b-%y")

# And just for fun, graph closing price over time for all stocks one on graph:
library(ggplot2)
ggplot(allStocks, aes(x=Date, y = Close, color=Stock)) + geom_line()


# Now let's finish cleaning up the 2012 election data!
names(electionData)
# I want to drop everything to the right of the "NA..34 NA" column. Frankly I'm
# not sure what those columns contain.

# Get the column number of the column with header "NA..34 NA"
fir <- grep("NA..34 NA", names(electionData))
fir
# get the column number of the last column in the data frame
las <- ncol(electionData)
las
# Now subset the data frame; keep all columns except 71-82
electionData <- electionData[,-c(fir:las)]

# drop columns with names of "NA..1, NA..2, etc"
ind <- grep("NA\\.\\.[0-9]{1,2}", names(electionData))
ind
electionData <- electionData[,-ind]

# and some final clean up
names(electionData)[3] <- "Total.Popular.Vote" 
names(electionData)[5] <- "Elec.Vote R" 
electionData$"Pop.Vote D" <- NULL
rownames(electionData) <- NULL

# Now our election data contains only number of votes. 



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

# First let's trim the white space using the rm_white_lead_trail() function.
rushSA <- rm_white_lead_trail(rushSA)

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
# numbers.
year <- unlist(rm_number(year, extract = TRUE, pattern = "[0-9]+"))
year

# Now get the album titles; this time use the rm_between() function without the 
# extract=TRUE argument. This removes everything between the (), including the
# parentheses.
album <- rm_between(rushSA, left = "(", right=")")
album

# And now our data frame: 
rushStudioAlbums <- data.frame(year, album)
head(rushStudioAlbums)

# There is also a package called stringi that I know very little about, except 
# that it bills itself as "THE string processing package for R". Might be worth a
# look: http://www.rexamine.com/resources/stringi/

# save data for next set of lecture notes
save(list=c("electionData", "weather", "arrests", "allStocks", "popVa",
            "senate_bills"), file="../data/datasets_L07.Rda")
