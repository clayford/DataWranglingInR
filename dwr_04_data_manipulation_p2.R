#' ---
#' title: "Data Wrangling in R: Data Manipulation Part 2"
#' author: "Clay Ford"
#' date: "Spring 2015"
#' output: pdf_document
#' ---


# load data from last lecture
load("../data/datasets_L03.Rda")

# Sorting data ------------------------------------------------------------

# To sort a vector you can use the sort() function. To sort a data frame use the
# order() function. By default, sort order is ascending.

# sorting a vector:
sort(allStocks$bbby.csv$Volume)[1:10]
sort(allStocks$bbby.csv$Volume, decreasing = TRUE)[1:10]

# Also note the difference...
# sorts ENTIRE vector and then displays top 10 in ascending order:
sort(allStocks$bbby.csv$Volume)[1:10]
# subsets first 10 of vector and then sorts those 10 in ascending order:
sort(allStocks$bbby.csv$Volume[1:10])


# Sorting a data frame is somewhat tricky. We use the order() function on a 
# vector (or vectors) which returns the index numbers of the original vector(s)
# placed in the necessary order to sort the vector/vector(s)

# order on a numeric vector
x <- c(5,4,7,12,1)
order(x)

# order on a character vector
y <- c("Red","Green","Green","Red","Green")
order(y)

# create a data frame and sort on y then x; note the use of brackets
df <- data.frame(x,y)
df
order(y,x) # how to order row numbers to sort data frame
df[order(y,x),]

# Sort the weather data on Max.TemperatureF and only show the first 6 rows and
# first 3 columns:
head(weather[order(weather$Max.TemperatureF),c(1:3)])

# sorting on two columns, first ascending, the second descending:
# Notice the minus sign in front of the second variable. That means
# sort descending.
head(weather[order(weather$Max.TemperatureF, -weather$Mean.TemperatureF),c(1:3)])


# Subsetting data ---------------------------------------------------------

# We often desire to look at or analyze a subset of data that meet certain 
# conditions. Maybe we want to look at all individuals over the age of 40, or 
# all males over the age of 40, or all males over the age of 40 who weigh more
# than 250 lbs, and so on. We

# One way to subset data is combining conditions with subscripting brackets. For
# example, our weather data has an Events column.
summary(weather$Events)

# Say we wanted to select only days that experienced "Rain-Snow" events. I'm 
# also only selecting a few columns strictly for presentation purposes, though
# selecting columns is also part of subsetting data.
weather[weather$Events=="Rain-Snow", c(1:5)]
weather[weather$Events=="Rain-Snow" & weather$Min.TemperatureF<32, c(1:5)]

# When working with data frames it's usually easier to use the subset() function
# to subset. The basic syntax is subset(x, subset, select) where x is a data 
# frame, subset is the subsetting condition, and select indicates the columns to
# keep. The following duplicates what we did with subsetting brackets.
subset(weather, subset= Events=="Rain-Snow", c(1:5))
subset(weather, Events=="Rain-Snow" & Min.TemperatureF<32, c(1:5))
# don't have to specify subset= since it's the 2nd argument.

# can also exclude columns using - (minus sign)
subset(weather, subset= Events=="Rain-Snow", select= -c(6:27))

# save the result
rsDays <- subset(weather, subset= Events=="Rain-Snow",
                    select=c(EST,Cloud.Cover.Index))
# note the row numbers are preserved from original data frame
rsDays 
# To reset the row numbers, assign NULL to the the row.names() function like so:
row.names(rsDays) <- NULL 
rsDays

# We can subset data using multiple conditions. Here we select records with 
# Maximum humidity less than 80 and a weather event of Rain. We also select only
# the EST (Date) Mean.TemperatureF and Mean.VisibilityMiles columns.
subset(weather, Max.Humidity < 80 & Events=="Rain",
       select=c(EST, Mean.TemperatureF, Mean.VisibilityMiles))

# Conditional operators:
# &   AND
# |   OR
# ==  EQUAL
# !=  NOT EQUAL

# When a function supports "formula" notation, subsetting is often supported via
# a subset argument. A common use is in plotting. For example, say we 
# wanted to plot mean temperature versus mean pressure, but
# only for days where Max.Humidity < 100. We can use the subset argument in the
# call to plot() as follows:
plot(Mean.TemperatureF ~ Mean.Sea.Level.PressureIn, 
     data=weather, subset= Max.Humidity < 100)
nrow(subset(weather, subset= Max.Humidity < 100))
# This can allow us to work with one data frame instead of several subsetted
# data frames.

# Another way of subsetting data is via the split() function. split divides the 
# data in the vector x into the groups defined by f. The basic syntax is split(x
# , f).

# Let's split the mean temperatures from our weather data by event.
head(weather$Mean.TemperatureF)
summary(weather$Events)
split(weather$Mean.TemperatureF, weather$Events)

# Notice it returned a list. It does this to allow the sizes of the groups to 
# differ. We can save the output of split and then apply a function to it, like
# so:
temps <- split(weather$Mean.TemperatureF, weather$Events)
# Since temps is a list, we need to use either lapply or sapply. I choose sapply
# to simplify the output:
sapply(temps,mean)

# You'll recall we did the same thing in the last lecture using tapply.

# We should pause here and take note of a couple of things. First there are many
# observations in weather that have no Event label. We should do something about
# that. Maybe change the empty event label to say "None". We'll do that in the
# next section.

# Also, if the idea of splitting data into groups and applying a function to
# each group sounds confusing or inefficient, you're not alone. We will explore
# other functions and packages that make this easier. However I believe it's
# good to know how to use base R functions to manipulate data, especially when
# you write your own functions.


# Updating Data -----------------------------------------------------------

# Sometimes we need to update data, such as replacing a missing value code of 
# "99" with NA. In fact this needs to be done with our arrests data. In this
# data set, 99 means missing. Since most of the numbers in this data are codes,
# this is a reasonable code to have. But there are two variables that represent
# actual numbers: Age and Children. We don't want 99 counted as a number in
# those columns. Here's why:
par(mfrow=c(1,2))
hist(arrests$Age)
hist(arrests$Children)
par(mfrow=c(1,1))
# A non-trivial number of people (about 500) have an age of 99 according to R. 
# And most people have 99 children! So if I summarize age and children, I get
# skewed numbers:
summary(arrests$Age)
summary(arrests$Children)

# Here's how we can replace 99 with NA:
arrests$Age[arrests$Age==99] <- NA
arrests$Children[arrests$Children==99] <- NA

# arrests$Age==99 produces a logical TRUE/FALSE vector the same length (ie, same
# number of indices as the arrests$Age vector). The values in the indexed
# positioms that correspond with TRUE are replaced with NA.

# Now our numerical data looks better and makes sense
par(mfrow=c(1,2))
hist(arrests$Age)
hist(arrests$Children)
par(mfrow=c(1,1))
summary(arrests$Age)
summary(arrests$Children)

# What we did above can also be used to impute missing data with, say, a mean.
# For example, if so inclined, we could now replace the NA's in the
# arrests$Children column with the mean, like so:

# arrests$Children[is.na(arrests$Children)] <- round(mean(arrests$Children, na.rm=TRUE))


# Factors -----------------------------------------------------------------

# Let's look again at the structure of our weather data:
str(weather)

# Notice that EST, PrecipitationIn and Events are stored as a "Factor". What 
# does that mean? Simply put, it means they're being treated as categorical 
# variables. Technically speaking, factors in R are stored as a vector of 
# integers with a corresponding set of character values to display when the 
# factor is printed to the screen. "One of the most important uses of factors is
# in statistical modeling; since categotical variables enter into statistical 
# models differently then continuous variables, storing data as factors insures
# that the modeling functions will treat such data correctly." (Spector, p. 67)

# Look again at the weather Events:
str(weather$Events)
# Notice the integers that are displayed. That's actually how Events are stored 
# in R. But we see the character labels when we print Events to the screen. Here
# we print the first ten:
weather$Events[1:10]
# Nine of the ten have no character label because the source data had no label. 
# Also, notice that the "levels" are automatically displayed below the output.
# This tells us we're looking at a factor instead of a vector of character strings.

# To see the integer codes, we can use the unclass() function:
unclass(weather$Events)

# To see just the factor levels, we can use the levels() function:
levels(weather$Events)

# Why are the Events formatted as factor? We didn't ask R to do that. It turns 
# out R imports character data as factors by default. This is usually the right 
# thing to do, but sometimes not. We can tell R not to format character data as 
# factors whem importing data by setting the argument "stringsAsFactors = FALSE"
# when reading in data via read.csv (or most other read.x functions).

# For example:
tmp <- read.csv("../data/cville_weather_2013.csv", stringsAsFactor=FALSE)
str(tmp$Events) # not a factor
rm(tmp) # remove tmp

# We can also use the as.character() function to convert a factor to character:
as.character(weather$Events)[1:10]

# NOTE: factors are actually an efficient way to store character values, because
# each unique character value is only stored once and the data itself is stored
# as integers.

# We need to be able to manipulate factors in the following ways:
# - create factors
# - change level names
# - add/remove levels
# - reorder levels

# To create factors use the factor() function; the optional labels argument 
# allows you to define your own labels. Let's make Cloud.Cover.Index a factor.
# Calling summary on it shows us that it is currently a numeric.
summary(weather$Cloud.Cover.Index)
# Now make it a factor:
weather$Cloud.Cover.Index <- factor(weather$Cloud.Cover.Index)
# notice how summary works on factor versus numeric vector:
summary(weather$Cloud.Cover.Index)
levels(weather$Cloud.Cover.Index)
class(weather$Cloud.Cover.Index)
weather$Cloud.Cover.Index[1:10]


# Notice NA does not get its own factor level by default. We can change that.
# Let's keep NA as a category and order it to come first; use levels= argument
# to change order of factor levels
weather$Cloud.Cover.Index <- factor(weather$Cloud.Cover.Index, 
                                    levels=c(NA, 0:8),
                                    exclude=NULL) # do not exclude NA
summary(weather$Cloud.Cover.Index)
levels(weather$Cloud.Cover.Index)
weather$Cloud.Cover.Index[1:10]


# To change the names of factor levels, use the levels() function with the 
# assignment operator ( <- ). Notice it requires a list object. Wikipedia tells
# us cloud coverage ranges is value from 0 - 9:
# http://en.wikipedia.org/wiki/Okta. Let's include a level for 9 and change the
# level names:
levels(weather$Cloud.Cover.Index) <- c("missing", "skc","few1", "few2",
                                          "sct3", "sct4", "bkn5", "bkn6", "bkn7",
                                          "ovc", "obstructed")
# notice there is no level 9, "obstructed"
summary(weather$Cloud.Cover.Index)

# We can drop unused factor levels using the droplevels() function:
weather$Cloud.Cover.Index <- droplevels(weather$Cloud.Cover.Index)
summary(weather$Cloud.Cover.Index)
levels(weather$Cloud.Cover.Index)
weather$Cloud.Cover.Index[1:10]

# droplevels() sometimes comes in handy after you have subsetted a data frame 
# and you want to drop unused factor levels that were dropped due to the
# subsetting.

# Let's go ahead and add a level that says "None" for days with no weather Events
levels(weather$Events)
levels(weather$Events)[1] <- "None"
levels(weather$Events)

# To reorder factor levels we can use the relevel() function. The most common 
# reason to reorder factors is to create a new "baseline" (ie, the first level).
# The syntax for relevel is relevel(x, f) where x is an unordered factor and ref
# is the reference level. Let's illustrate with dummy data:
gender <- factor(c("M","F","F","M","M","F"))
gender
# F is the baseline level. We change to M as follows:
gender <- relevel(gender, ref="M")
gender

# This has implications for statistical modelling. 

# Converting factor to numeric --------------------------------------------

# Remember the election data?
str(electionData)
# notice many columns of numbers are formatted as factors. For example, look at
# the "Obama Democratic" column.
summary(electionData$"Obama Democratic")
# Notice we had to put the column name in quotes since it contains a space.

# Note "Democratic" in the summary output. That's due to variable names being on
# two rows in the source Excel document. It has 0 because we removed the 
# offending row in the previous lecture notes, but it's still stored as a level:
levels(electionData$"Obama Democratic")

# Since "Deomocratic" group has no members, we can simply convert "Obama
# Democratic" to character, then numeric.
as.numeric(as.character(electionData$"Obama Democratic"))

# what happens if we just use as.numeric by itself?
as.numeric(electionData$"Obama Democratic")
# We just get the integer codes. Remember, factors are stored as integers with 
# corresponding character labels! Hence the need to first convert to character, 
# which replaces the integer codes with the character labels. Then we can
# convert to numeric.

electionData$"Obama Democratic" <- as.numeric(as.character(electionData$"Obama Democratic"))
summary(electionData$"Obama Democratic") 

# How can we do all of them?
names(electionData) 
# note the index numbers of the columns that are titled "Candidate Party".

# We can write a for loop to do to all of these columns what we did above:
for(i in seq(12,70,by=2)){
  if(is.factor(electionData[,i])){
    electionData[,i] <- as.numeric(as.character(electionData[,i]))
    print(i) # see which columns were changed
  }
}

# Notes:
# seq(12,70,by=2) creates a sequence of numbers from 12 to 70 in steps of 2.
# is.factor(electionData[,i]) returns TRUE if column i is a factor.


# Look again at Precipitation in the weather data:
str(weather$PrecipitationIn)
# What's going on here? Why was Precipitation imported as a factor?
summary(weather$PrecipitationIn)
# Look at the "T" at the end. The "T" means "trace amounts" of precipitation. R 
# saw the character "T" and automatically treated the entire column as 
# character, and hence a factor, when we imported weather. One option is to
# leave as is and treat precipitation as a categorical variable. But what if we
# wanted to calculate the mean precipitation? 

# As we noted above, the correct way to proceed is to first convert the factor
# to character:
weather$PrecipitationIn <- as.character(weather$PrecipitationIn)

# Now let's change the "T" (trace amounts) to 0.001. Notice we're dealing with a
# character vector so we need to use quotes around values:
weather$PrecipitationIn <- ifelse(weather$PrecipitationIn=="T","0.001",
                                  weather$PrecipitationIn)
# NOW we can use as.numeric: 
weather$PrecipitationIn <- as.numeric(weather$PrecipitationIn)
summary(weather$PrecipitationIn)


# Converting numeric to factor --------------------------------------------

# It's easy to convert a numeric variable to a factor. Just use the factor() 
# function. For example, say data frame DF has a variable YEAR that is numeric.
# We can convert YEAR to a factor as follows:

# DF$YEAR <- factor(DF$YEAR)

# We can also use the cut() function to convert a numeric variable into a 
# categories, and hence a factor. For example, splitting ages into age
# categories. The basic syntax is cut(x, breaks) where x is numeric vector and
# breaks is either a numeric vector of two or more unique cut points, or a
# single number (greater than or equal to 2) giving the number of intervals into
# which x is to be cut.

# Let's make four equally spaced levels for Mean.Humidity:
meanHumGr1 <- cut(weather$Mean.Humidity,4) 
table(meanHumGr1)
# By default, labels are constructed using "(a,b]" interval notation

# We can also make four groups with roughly equal numbers in each using the 
# quantile() function. The quantile() function returns quartiles (0%, 25%, 50%, 
# 75%, 100%) by default.
quantile(weather$Mean.Humidity,na.rm=T)
meanHumGr2 <- cut(weather$Mean.Humidity,
                  quantile(weather$Mean.Humidity,
                           na.rm=T))
# can use the table() function to tally membership in categories:
table(meanHumGr2)

# but notice:
sum(table(meanHumGr2))
# I guess we have two records with missing Mean.Humidity...
sum(is.na(weather$Mean.Humidity))
# Apparently not. But what about our new factor variable:
sum(is.na(meanHumGr2))
# Two?! what's going on?
which(is.na(meanHumGr2))
weather[c(48,79),"Mean.Humidity"]
# Why was the record with Mean.Humidity = 26 not classified? help(cut) tells us 
# that include.lowest = FALSE by default. This indicates if a value equal to the
# lowest ‘breaks’ value should be included. Let's try again with include.lowest
# = TRUE
meanHumGr2 <- cut(weather$Mean.Humidity,
                  quantile(weather$Mean.Humidity,
                           na.rm=T),
                  include.lowest = TRUE)
table(meanHumGr2)
# Notice the lowest category is inclusive on the lower bound.
sum(table(meanHumGr2))

# Since letting R determine cut points can lead to confusion as we just saw, 
# sometimes it's better to manually specify groups. Let's create our own four 
# groups:
meanHumGr3 <- cut(weather$Mean.Humidity,
                  breaks=c(0,30,50,70,100))
table(meanHumGr3)
# Notice we specified 5 cut points for 4 groups. You have to specifiy the lowest
# bound and the highest bound. Here we specify (0, 30], (30,50], (50,70], and
# (70,100].

# We can specify labels we prefer using the labels argument.
meanHumGr3 <- cut(weather$Mean.Humidity,
                  breaks=c(0,30,50,70,100),
                  labels=c("bone dry","dry","normal","humid"))
table(meanHumGr3)


# We can also use ifelse() to cut numeric variables into groups. This is handy 
# for creating indicator variables. For example, let's create a snow indicator
# for the weather data:

# if event one of three events, output 1, else output 0
weather$snow <- ifelse(weather$Events %in% c("Fog-Rain-Snow","Snow","Rain-Snow"), 1,0)

# %in% allows you to make multiple comparisons. 

# how many days did it snow in 2013?
sum(weather$snow) 
# sanity check; did we capture all "snow" events?
weather[weather$snow==1,c("Events","snow")]
table(weather$Events, weather$snow)


# save data for next set of lecture notes
save(list=c("electionData", "weather", "arrests", "allStocks"), file="../data/datasets_L04.Rda")

