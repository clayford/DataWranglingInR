#' ---
#' title: "Data Wrangling in R: Data Manipulation Part 1"
#' author: "Clay Ford"
#' date: "Spring 2015"
#' output: pdf_document
#' ---



# Intro -------------------------------------------------------------------

# "Data Manipulation" means many things. It could actually be the title of this 
# course! In this lecture it refers to adding, deleting and changing data in the
# R environment.

# Let's work with data from lecture 2.
load("../data/datasets_L02.Rda")


# The apply family of functions -------------------------------------------

# R is famous (or perhaps infamous) for its apply functions. Put simply the 
# apply functions allow you to "apply" functions to groups of data. The four
# we'll focus on are:

# - apply()
# - lapply()
# - sapply()
# - tapply()

# It's crucial to understand apply functions for effective data manipulation.

# apply() applies a function over matrix;
# lapply() applies a function over a list or vector; it returns a list;
# sapply() is like lapply() except it attempts to "simplify" output;
# tapply() applies a function to groups within a vector;

# The syntax for apply is apply(X, MARGIN, FUN, ...) where X is a matrix, MARGIN
# is the row or column indicator (1 or 2), FUN is the function to apply, and ...
# are additional arguments for FUN.

# Example:
x <- cbind(x1 = 3, x2 = c(4:1, 2:5,10))
x
class(x)
# apply the mean function to the columns
apply(x, 2, mean)
# apply the mean function to the columns with the trim argument set to 0.2. Trim
# is the fraction (0 to 0.5) of observations to be trimmed from each end of x 
# before the mean is computed.
apply(x, 2, mean, trim=.2)
# apply the standard deviation function
apply(x, 2, sd)
# apply a function of our own creation
apply(x, 2, function(x)sd(x)/sqrt(length(x)))
# apply a function across rows
apply(x, 1, sum)

# apply will work on data frames as well, but internally R converts it to a
# matrix first.
x <- data.frame(x)
class(x)
apply(x, 2, mean)

# The syntax for lapply is lapply(X, FUN, ...) where X is a vector or list, FUN 
# is the function to apply, and ... are additional arguments for FUN.

# Example:
y <- list(a=runif(10), b=rnorm(12), c=sample(1:5, 20, replace = TRUE))
y
class(y)
lapply(y,median)
lapply(y,range)
lapply(y, function(x) mean(x + 10))

# The syntax for sapply is pretty much identical to lapply, except the output is
# simplified.

# Example:
sapply(y,median)
sapply(y,range)
sapply(y, function(x) mean(x + 10))

# The syntax for tapply() is tapply(X, INDEX, FUN, ...), where X is usually a 
# vector, INDEX is a list of one or more factors, each of same length as X, FUN
# is the function to apply, and ... are additional arguments for FUN.

# Example:
# generate random data from Normal distributions (10 each)
z <- c(rnorm(10,120,10), rnorm(10,180,10))
z
# generate a factor with 2 levels of 10 each using the gl() function.
g <- gl(n = 2, k = 10)
g

# now apply mean() to z based on levels of g
tapply(z, g, mean)



# Deleting columns from data frame ----------------------------------------

# Sometimes we want to drop unwanted columns from a data frame. We can do that
# as follows:

# Let's drop the WindDirDegrees column from weather
head(weather)
weather$WindDirDegrees <- NULL
head(weather)


# remember the election data? It's a mess:
str(electionData)

# It appears to have "junk" columns that consist of nothing but missing values.
# View the data in RStudio by clicking on it in the Environment panel.

# We would like to identify which columns consist of all missing data and then 
# drop those columns from the data frame. We can use a combination of is.na(),
# all(), which() and sapply()

# apply the function all(is.na(x)) to each column of electionData. Notice that
# electionData is a data frame, and that a data frame is a type of list. Thus we
# can use lapply and sapply on data frames.
sapply(electionData, function(x)all(is.na(x)))

# But we only want the columns that evaluate to TRUE:
which(sapply(electionData, function(x)all(is.na(x))))

# so we want to drop columns 72 and 84
drop <- which(sapply(electionData, function(x)all(is.na(x))))
electionData <- electionData[,-drop]


# Deleting rows from data frame -------------------------------------------

# look again at electionData. Several empty rows read in with all NA.

# drop specific rows with column summaries
electionData <- electionData[-c(53:56),]

# Now identify row numbers for rows of all missing data and drop from data 
# frame. Notice that electionData is a data frame but we use apply(). Can we do 
# that? Yes, just know that apply() first converts data frames to matrices 
# before it applies a function. In this case it's OK because NA is the same no 
# matter the class.
drop <- which(apply(electionData, 1, function(x)all(is.na(x))))
electionData <- electionData[-drop,]



# Adding columns to data frames -------------------------------------------

# One way to add a column to a data frame is to type a new column name following
# the "data frame" plus "$" notation and assign it some value. For example, we
# can add a column and assign it a value of NA as a place holder column for
# future data. NA is automatically copied (or recycled) to every row

weather$freezing <- NA

# We can add a column based on other column(s) i.e., derive a new variable. 
# Let's create a new variable called Temp.Range that is equal to the maximum
# temperature minus the minimum temperature. 
weather$Temp.Range <- weather$Max.TemperatureF - weather$Min.TemperatureF

# Whenever you derive a new variable it's good to look it over and see if it
# makes sense.
summary(weather$Temp.Range) # summary stats; min = -33?
weather[weather$Temp.Range == -33,]
weather[weather$Temp.Range < 0,]
plot(weather$Temp.Range) # plot against index value

# Looks like Min.TemperatureF = 99 may be a missing value code.

# Let's go back to our freezing variable and make that an indicator for days
# that never got above 32:
weather$freezing <- ifelse(weather$Max.TemperatureF<=32, 1, 0)
sum(weather$freezing==1) # number of days that never got above freezing
which(weather$freezing==1) # which rows?
weather[weather$freezing==1,"EST", drop=TRUE] # which days?

# We can also use transform() to derive new variables and add to a data frame.
# The syntax is transform(data frame, new variable = )
# Let's add a variable for humidity range:
weather <- transform(weather, humidity.range = Max.Humidity - Min.Humidity)


# Rename columns in a data frame ------------------------------------------

# First we can see existing names with the names() function
names(weather)

# But not only can we see names with the names() function, we can change names
# with the names() function.
names(weather)[21] <- "Cloud.Cover.Index"

# This is just a nightmare....
names(electionData) 

# Let's clean up the electionData names. First we notice that the column names
# were split across two rows in the original Excel file.

# extract names into vector
top <- names(electionData) 
bot <- electionData[1,] # extract first row
# look at bot; it's a data frame with "names"
bot
class(bot)
names(bot)
# remove names from bot
names(bot) <- NULL 
names(bot)

# Now convert bot to a character vector; first we convert to matrix which
# converts all data to character, and then we convert to a vector.
bot <- as.vector(as.matrix(bot)) 
bot
# now we can paste the two vectors together using the paste() function.
paste(top,bot)
# use the pasted together vector to replace the names of the electionData
names(electionData) <- paste(top, bot)
electionData <- electionData[-1,] # drop the first row
# a little better now, but still needs work. We'll get to it....


# Reorder columns in a data frame -----------------------------------------

# One way is to simply specify the column numbers in the order you want. For 
# example, move freezing and temp.range next to the temperature columns and
# humidity.range next to the humidity columns.
names(weather)
weather <- weather[,c(1:4,23,24,5:10,25,11:22)]
names(weather)

# save data for next set of lecture notes
save(list=c("electionData", "weather", "arrests", "allStocks"), file="../data/datasets_L03.Rda")
