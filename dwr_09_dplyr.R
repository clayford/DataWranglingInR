#' ---
#' title: "Data Wrangling in R: plyr and dplyr"
#' author: "Clay Ford"
#' date: "Spring 2015"
#' output: pdf_document
#' ---

load("../data/datasets_L07.Rda")

# This lecture continues with aggregation and introduces two packages: plyr and
# dplyr. 

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
temps <- split(weather$Max.TemperatureF, weather$Events) # split
temps
maxmeans <- sapply(temps, mean) # apply a function to each group
maxmeans
data.frame(event=names(maxmeans), meanMaxTemp=maxmeans, row.names = NULL) # combine results

# Let's do the same with plyr
ddply(weather, "Events", summarize, meanMaxTemp=mean(Max.TemperatureF))

# Let's break that down:
# dd in ddply means data frame in, data frame out
# first argument: input data frame
# second argument: grouping variable to split data frame by
# third argument: function to apply to each group
# fourth argument: argument passed to third function

# Note we can do the same with aggregate()
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
# calculation of seMaxTemp! We cannot do that with aggregate() or transform().


# dplyr -------------------------------------------------------------------

# dplyr is the next iteration of plyr, focusing only on data frames. It is much 
# faster than plyr and easier to use (in my opinion). In addition it comes with
# a fantastic Introductory vignette in the documentation. 

# dplyr provides data manipulation verbs that work on a single data frame. The 
# dplyr philosophy is to have small functions that each do one thing well. The
# current verbs include:

# filter() - select a subset of the rows of a data frame

# slice() - select rows by position

# arrange() - reorder (sort) rows by columns

# select() - select columns

# rename() - rename variables (column headers)

# distinct()- return the unique values in a data frame

# mutate() - add new columns that are functions of existing columns

# transmute() - like mutate, but keeps only the new columns

# summarise() - summarize values

# sample_n() - randomly sample fixed number of rows of a data frame

# sample_frac() - randomly sample fixed fraction of rows of a data frame

# For all these functions, the first argument is a data frame. The subsequent
# arguments describe what to do with it, and you can refer to columns in the
# data frame directly without using $. And they all return a new data frame.

# In addition:
# group_by() describes how to break a dataset down into groups of rows

# Example data: Sean Lahman's Baseball Database. This package provides the
# tables from Sean Lahman's Baseball Database as a set of R data.frames. It
# uses the data on pitching, hitting and fielding performance and other tables
# from 1871 through 2013, as recorded in the 2014 version of the database.

# install.packages("Lahman")
library(Lahman) # baseball data

# Let's use the Batting data frame
head(Batting, n=10)
nrow(Batting)

# Say we want to find the top 5 highest number of "Game as batter" 
# (G_batting) for each player

# first, try it with plyr
# this takes about 10 seconds on my computer
games <- ddply(Batting, "playerID", summarize, total = sum(G_batting, na.rm=T))
head(arrange(games, desc(total)), 5)

# now try it with dplyr
# first detach plyr
detach("package:plyr", unload=TRUE) # prevent conflicts with dplyr
# now load dplyr
library(dplyr)

# First group Batting by playerID; create new data frame
players <- group_by(Batting, playerID)
# next sum "Game as batter" for each player and sort
games <- summarize(players, total = sum(G_batting, na.rm=T)) # note the speed!
head(arrange(games, desc(total)), 5)

# how to do with base R functions: 
games2 <- aggregate(G_batting ~ playerID, data=Batting, sum)
head(games2[order(games2$G_batting, decreasing=T),],5)

# so dplyr is not much faster than base R (in this example), but the syntax is
# easier to understand and learn.

# dplyr also provides ability to chain operations using the %>% operator. Use
# Ctrl + Shift + M to quickly enter the chaining operator: %>%


# Here's the same thing as above;
# functions chained left-to-right with the %>% operator
Batting %>%
  group_by(playerID) %>%
  summarize(total = sum(G_batting)) %>%
  arrange(desc(total)) %>%
  head(5)

# In words, that says, "take the Batting data frame, break into groups by 
# playerID, within each group take the sum of G_batting and return a new data 
# frame containing the sum, sort the new data frame in descending order and
# return a new data frame, and finally return the first 5 rows of the new data
# frame.


# The dplyr verbs ---------------------------------------------------------

# first, may want to consider converting data frame to tbl_df, a wrapper that
# doesn't print a lot of data to the screen.
weather <- tbl_df(weather)
weather

# filter() - select a subset of the rows of a data frame
filter(tbl_df(popVa), city.ind==1) # cities
filter(tbl_df(popVa), respop72012>30000 & city.ind==0) # towns w/ population > 30,000 

# slice() - select rows by position
slice(weather,1:10)

# arrange() - reorder (sort) rows by columns
arrange(tbl_df(popVa), respop72012)
arrange(tbl_df(popVa), desc(respop72012)) # uses desc() helper function
arrange(weather, Max.TemperatureF, Max.Dew.PointF)

# select() - select columns
select(weather, Max.TemperatureF, Min.TemperatureF, Temp.Range)[1:4,]
select(electionData, 12:15)

# rename() - rename variables (column headers)
weather <- rename(weather, Snowed = snow)
# using rename() when variable name has spaces (use backticks)
electionData <- rename(electionData, MOV = `Margin.of.Victory Votes`)

# distinct()- return the unique values in a data frame; often used with select()
distinct(select(weather, Events))

# mutate() - add new columns that are functions of existing columns
weather <- mutate(weather, Dew.Point.range= Max.Dew.PointF - Min.DewpointF)
weather$Dew.Point.range
weather <- mutate(weather, Temp.Centered=Max.TemperatureF-mean(Max.TemperatureF))
weather$Temp.Centered[1:5]
sum(weather$Temp.Centered) # should sum to 0, or thereabouts

# transmute() - like mutate, but keeps only the new columns
changes <- transmute(allStocks, Change = Close - Open)
head(changes)

# summarise() - summarize values and collapse a data frame to a single row
summarise(weather, meanMaxTemp=mean(Max.TemperatureF))
summarise(popVa, min(respop72012))

# sample_n() - randomly sample fixed number of rows of a data frame
sample_n(weather, 5)

# sample_frac() - randomly sample fixed fraction of rows of a data frame
sample_frac(weather, 0.01)

# dplyr provides numerous helper functions that work well with summarise:
# n(): number of observations in the current group
# n_distinct(x): count the number of unique values in x.
# first(x), last(x) and nth(x, n): similar to x[1], x[length(x)], x[n] 

# There also are a number of helper functions you can use within select(), like
# starts_with(), ends_with(), matches() and contains(). These let you quickly
# match larger blocks of variable that meet some criterion.

# select weather columns that contain "Max"
select(weather, starts_with("Max."))[1:4,]
select(popVa, contains("2010"))[1:4,]

# Chaining ----------------------------------------------------------------

# The dplyr verbs work best when chained together! dplyr provides the %>%
# operator for chaining dplyr verbs (functions). Let's work through some
# examples to show how it works.

# Find the minimum and maximum stock price for each stock
allStocks %>% 
  group_by(Stock) %>%
  summarise(Min=min(Low), Max=max(High))

# Find the largest change in Open and Close price for each stock
allStocks %>%
  group_by(Stock) %>%
  mutate(Change = Close - Open) %>%
  summarise(LargestGain = max(Change), LargestLoss = min(Change))

# get mean pop'n in cities vs towns in popVa
popVa %>%
  group_by(city.ind) %>%
  summarize(meanPop=mean(respop72012))

# get % change in population from April 2010 to July 2012,
# rounded to one place, and sort descending
popVa %>%
  select(city, rescen42010, respop72012) %>%
  mutate(percentChange=round((respop72012-rescen42010)/rescen42010*100,1),
         absoluetChange=respop72012-rescen42010) %>%
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
  arrange(desc(percentChange)) %>%
  select(city,percentChange, respop72012, city.ind) %>%
  head(n=10)

# cosponsors of senate bills: the top 10 higgest cosponsored bills
senate_bills %>%
  filter(cosponsors > 0) %>%
  arrange(desc(cosponsors)) %>%
  select(title, cosponsors) %>%
  head(n=10)

# number of bills per sponsor (senator);
# for those with at least two bills;
# sorted descending
senate_bills %>%
  group_by(sponsor) %>%
  summarize(total=n()) %>%
  arrange(desc(total)) %>%
  filter(total>1)

# calculate mean max temperature per month
weather %>%
  group_by(months(Date)) %>%
  summarize(meanMaxTemp=round(mean(Max.TemperatureF)))

# Notice the months are sorted alphabetically. An easy way to fix is to use the 
# month() function in the lubridate package. Notice below we can also create a
# name for our grouping variable in the group_by() function:
library(lubridate)
weather %>%
  group_by(Month=month(Date, label=T)) %>%
  summarize(meanMaxTemp=round(mean(Max.TemperatureF)))

# save the previous summary and graph a dot chart
meanMax <- weather %>%
  group_by(Month=month(Date, label=T)) %>%
  summarize(meanMaxTemp=round(mean(Max.TemperatureF)))

dotchart(x = meanMax$meanMaxTemp, 
         labels = meanMax$Month, lcolor="black", pch=19,
         main="Mean Max C'ville Temp by Month, 2012")

# electionData still needs work. Notice there are 5 columns that are formatted 
# as factor that should be numeric, and some variable names have spaces, which 
# is poor style. 
sapply(electionData, class)[2:6]

# Let's see how we can address these issues using dplyr:

# To begin I make a function to convert a factor to numeric. Notice I have to
# first convert to character THEN numeric.
makeNum <- function(x) as.numeric(as.character(x))
# make sure it works
makeNum(electionData$Total.Popular.Vote)[1:3]

# Now we'll use the rename and mutate verbs:
electionData <- electionData %>% 
  rename(TEV = `Total Elec Vote`, TPR = `Total.Popular.Vote`, 
         EVD = `Elec.Vote D`, EVR = `Elec.Vote R`) %>%
  mutate(TEV = makeNum(TEV), TPR = makeNum(TPR),
         EVD = makeNum(EVD), EVR = makeNum(EVR),
         MOV = makeNum(MOV),
         MOV2 = ifelse(is.na(EVD),MOV*-1,MOV),
         State = tolower(State),
         Blue = ifelse(!is.na(EVD),1,0)) 

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
# Use the map_data() function from the maps package to create a data from of US
# map data.
states <- map_data("state")
# Now merge the map data with the election data by state name.
choro <- merge(states, electionData, by.x = "region", by.y = "State")
# reorder the rows because order matters when coloring in states
choro <- arrange(choro, order)
# now plot the map using ggplot
ggplot(choro, aes(long, lat, group=group, fill=MOV2)) +
  geom_polygon(color="black") +
  scale_fill_gradient2(low="red", mid="white", high="blue")

# Notice how ggplot and dplyr are similar in the way they allow you to chain 
# together commands. This is because they were both designed by the same person,
# Hadley Wickham. 

# save data for next set of lecture notes
save(list=c("electionData", "weather", "arrests", "allStocks", "popVa",
            "senate_bills"), file="../data/datasets_L09.Rda")