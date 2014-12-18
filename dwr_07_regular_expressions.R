#' ---
#' title: "Data Wrangling in R: Regular Expressions"
#' author: "Clay Ford"
#' date: "Spring 2015"
#' output: pdf_document
#' ---

load("../data/datasets_L06.Rda")

# A 'regular expression' is a pattern that describes a set of strings. Regular
# expressions are a language into itself.

# quote floating around internet: Some people, when confronted with a problem,
# think "I know, I'll use regular expressions." Now they have two problems.

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

# Let's create some sample text to demonstrate regular expressions:

someText <- c("  here's a sentence", "This is me typing  ", "You saw 4 monkeys?", 
              "There are 10 kinds of people,    those that understand binary       
              and the other 9 that don't care",
              "Who likes pancakes? I do. I really really like pancakes!", 
              "     <strong>Bolded text is bold and daring</strong>",
              "figure1.jpg", "cover.jpg")


# Functions ---------------------------------------------------------------

# Recall: grep() and grepl() search for matches:
grep("pancake",someText) # return indices
grep("pancake",someText, invert=T) # the reverse
grep("pancake",someText, value=T) # return matching elements
grepl("pancake",someText) # return logical vector

# Two more functions: regexpr() and gregexpr() These functions return an integer
# vector of the same length as text giving the starting position of the first
# match or -1 if there is no match, plus length of matched text.

# regexpr() reports first match
regexpr("pancake",someText)  
# the first match is in the 5th element of the vector, begins at position 11,
# and is 7 characters long. -1 indicates no match.

# gregexpr() reports all matches
gregexpr("pancake",someText)
# This shows all matches. The first match is in the 5th element of the vector, 
# begins at position 11, and is 7 characters long. The second match is in the
# 5th element of the vector, begins at position 48, and is 7 characters long.

# regexpr() and gregexpr() are often used with regmatches(). regmatches allows
# you to extract or replace matched substrings from match data obtained by
# regexpr and gregexpr. For example, using regexr():
mdata <- gregexpr("pancake",someText)
# extract the match
regmatches(someText,mdata) 
# replace the FIRST match
regmatches(someText,mdata) <- "waffle" 
someText[5]


# Regular Expression Basics -----------------------------------------------

# Now let's talk regular expressions. The previous expressions, along with sub
# and gsub, can be quite powerful when used with regular expressions. 

# Regular expressions are composed of three components:

# (1) literal characters
# (2) modifiers (or metacharacters)
# (3) character classes

# MODIFIERS
# use modifiers to define patterns;
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

# CHARACTER CLASSES
# a range of characters to be matched;
# placed in brackets: []

# see help(regex) for an indepth overview of regular expressions.

# RegEx examples ----------------------------------------------------------

# find elements in vector beginning with 1 or more spaces
grep("^ +", someText, value=T) 
# find elements containing a question
grep("\\?", someText, value=T) 
# find elements ending with a question
grep("\\?$", someText, value=T) 
# find elements containing one or more numbers
grep("[0-9]+", someText, value=T) 
# find elements containing numbers with 2 digits
grep("[0-9]{2}", someText, value=T)
# file names ending with .jpg
grep("\\.jpg$", someText, value=T) 
# file names with only letters ending in .jpg
grep("^[a-zA-Z]+\\.jpg", someText, value=T)
# text containing two consecutive "really "
grep("(really ){2}",someText, value=T) 


# Search/Replace with RegEx -----------------------------------------------

# Recall sub() and gsub() functions. These perform replacement of the first and 
# all matches respectively. In a previous lecture we used them to search/replace
# literal strings. Now let's use them with regular expressions.

# (1) Replace Repeated Whitespace with a Single Space
gsub(" +"," ", someText)

# (2) Trim Leading and Trailing Whitespace: use two substitutions: one to remove
# leading whitespace, and another to remove trailing whitespace:
gsub("^\\s+","", someText)
gsub("\\s+$","", someText)

# (3) Remove HTML/XML tags (basic)
# "<" followed by anything but ">" and ending with ">" 
gsub("<[^>]*>","",someText)

# Regular Expressions can be very helpful when doing web scraping. Let's scrape 
# some data and demonstrate. Simply give a URL as an argument to the readLines()
# function. This will read in the HTML code of the web page into a single vector.

# 113th Congress Senate Bills: first 100 results.
senate_bills <- readLines("http://thomas.loc.gov/cgi-bin/bdquery/d?d113:0:./list/bss/d113SN.lst:")
senate_bills[1:10]

# We'd like to create a data frame that includes bill number, bill title,
# sponsor, and number of co-sponsors.

# In the HTML we see that bill number, title, and sponsor are in lines that
# begin like this: "<p><b>15.</b>". We can use regular expressions to find all
# 1-3 digit numbers followed by a period.

# [0-9]{1,3} = numbers 0-9 of length ranging from 1 to 3
# grep() will find the indices of such patterns,
k <- grep("[0-9]{1,3}.</b>", senate_bills)
k[1:4]
# Use k to subset the data
temp <- senate_bills[k]
# Now replace the HTML tags with space
temp <- gsub("<[^>]*>", " ",temp)
# break vector elements by ":"
temp <- strsplit(temp,":")

# Let's see what we have so far:
head(temp)

# To get the bill numbers we can pull out the first element of each list
# component as follows:
bill <- sapply(temp,function(x)x[1])
# we can define a regular expression as a variable and use that regexpr(). This
# regular expression finds the letter S followed by 1-3 numbers:
pattern <- "S.[0-9]{1,3}"
# identify starting position of match
m <- regexpr(pattern, bill)
# extract the matches
bill <- regmatches(bill, m)
bill[1:4]

# Now let's get the bill title:
temp[[1]][2]
# pull out second element of each list component
title <- sapply(temp,function(x)x[2])
# get rid of leading spaces
title <- gsub("^ +","",title) 
# get rid of " Sponsor" at end
title <- gsub(" Sponsor$","",title)
# get rid of trailing spaces
title <- gsub("\\s+$","",title) 
title[1:3]

# Now get the bill sponsor:
temp[[1]][3]
sponsor <- sapply(temp,function(x)x[3])
sponsor <- gsub("^ +","",sponsor) # get rid of leading spaces
sponsor[1:3]

# Get number of cosponsors
k <- grep("Cosponsors",senate_bills)
temp <- senate_bills[k]
temp <- strsplit(temp,"Cosponsors")
head(temp)
# Number of cosponsors in second element
cosponsors <- sapply(temp,function(x)x[2])
# replace "<....(" with nothing
cosponsors <- gsub("<.*\\(","",cosponsors)
# replace ")" with nothing
cosponsors <- gsub("\\)","",cosponsors) 
# replace "None" with 0
cosponsors <- gsub("None",0,cosponsors)
# Finally convert to numeric
cosponsors <- as.numeric(cosponsors)

# create data frame
senate_bills <- data.frame(bill, title, sponsor, cosponsors)
head(senate_bills)

# Recall our allStocks data. We wanted to add a column indicating which stock 
# each row belongs to. We can use strsplit() and regular expressions to easily
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

# Now we can use strsplit to split the Stock vector at the period. The period is
# a modifier in regular expressions, so we need to escape with a backslash,
# which itself needs to be escaped!
tmp <- strsplit(allStocks$Stock, split = "\\.")
tmp[[1]]

# Extract the first element and add to the allStocks data frame:
tmp <- sapply(tmp, function(x)x[1])
tmp[1:3]
allStocks$Stock <- factor(tmp)
head(allStocks)
tail(allStocks)

# While we're at it, let's fix the Date:
class(allStocks$Date)

# Currently a factor. It should be a date.
allStocks$Date <- as.Date(allStocks$Date, format="%d-%b-%y")

# Now let's finish cleaning up the 2012 election data
names(electionData)
# I want to drop everything to the right of the "NA..34 NA" column. Frankly I'm
# not sure what those columns contain.

# Get the column number of the column with header "NA..34 NA"
fir <- grep("NA..34 NA", names(electionData))
# get the column number of the last column in the data frame
las <- ncol(electionData)
# Now subset the data frame:
electionData <- electionData[,-c(fir:las)]

# drop columns with names of "NA..1, NA..2, etc"
ind <- grep("NA\\.\\.[0-9]{1,2}", names(electionData))
ind
electionData <- electionData[,-ind]

# and some final clean up
names(electionData)[1] <- "State"
names(electionData)[3] <- "Total.Popular.Vote" 
names(electionData)[5] <- "Elec.Vote R" 
electionData$"Pop.Vote D" <- NULL

# Now our election data contains only number of votes. 

# Here's a quick way to create a city indicator in our popVa data set. Recall 
# some places are called "town" and others "city". Let's create an indicator
# that takes the value 1 if the place is a city and 0 otherwise.
popVa$city.ind <- ifelse(grepl(pattern = "city",popVa$GEO.display.label),1,0)


# stringr, qdapRegex and stringi packages

# The stringr package can also use regular expressions. In fact that's pretty
# much the idea.
library(stringr)

# The qdapRegex package has some pre-defined functions for Regular Expression
# Removal, Extraction, and Replacement. 
# install.packages("qdapRegex")
library(qdapRegex)

# I have a text file of studio albums by the Canadian rock band, Rush. Let's
# read it in:
rushSA <- readLines("../data/rush_albums.txt")
rushSA

# I'd like to make a data frame with two columns: album title and year released.
# Let's use stringr and qdapRegex functions along with some regular expressions
# to do this.

# The qdapRegex package has a function called rm_between() that will 
# Remove/Replace/Extract Strings Between 2 Markers. I want to use it to extract 
# album release year between parentheses. Note I have to use the extract=TRUE
# argument:
year <- rm_between(rushSA, left = "(", right=")", extract=TRUE)
# That returns a list; I can use unlist() to make it a vector:
year <- unlist(year)
year
# Need to remove the string ", cover album". Let's use the str_extract function
# from the stringr package:
year <- str_extract(year, "[0-9]{4}")
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
# that it bills itself as "THE string processing package for R" Might be worth a
# look: http://www.rexamine.com/resources/stringi/

# save data for next set of lecture notes
save(list=c("electionData", "weather", "arrests", "allStocks", "popVa",
            "senate_bills"), file="../data/datasets_L07.Rda")
