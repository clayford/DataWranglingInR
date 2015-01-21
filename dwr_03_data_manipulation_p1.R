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
# 
# R is famous (or perhaps infamous) for its apply functions. Put simply the 
# apply functions allow you to "apply" functions to groups of data. The four
# we'll focus on are:

# - apply()
# - lapply()
# - sapply()
# - tapply()

# apply() applies a function over columns or rows of a matrix (or array);
# lapply() applies a function over a list or vector; it returns a list;
# sapply() is like lapply() except it attempts to "simplify" output;
# tapply() applies a function to groups within a vector;

# apply 

# The syntax for apply is apply(X, MARGIN, FUN, ...) where X is a matrix, MARGIN
# is the row or column indicator (1 or 2), FUN is the function to apply, and ...
# are additional arguments for FUN.

# Example: Make a matrix called "x"
x <- cbind(x1 = rep(1:3,3), x2 = c(4:1, 2:5,10))
x
class(x)
# apply a function across rows
apply(x, 1, sum)
# apply a function down columns
apply(x, 2, mean)
# apply the mean function to the columns with the trim argument set to 0.2. Trim
# is the fraction (0 to 0.5) of observations to be trimmed from each end of x 
# before the mean is computed.
apply(x, 2, mean, trim=.2)
# apply the range function; this returns a vector
apply(x, 2, range)

# apply will work on data frames as well, but internally R converts it to a
# matrix first.
x <- as.data.frame(x)
class(x)
apply(x, 2, mean)

# By the way, R has built-in functions for calculating row and column sums and
# means:
colSums(x)
rowSums(x)
colMeans(x)
rowMeans(x)

# From the help manual: "These functions are equivalent to use of apply
# with FUN = mean or FUN = sum with appropriate margins, but are a lot faster."

# lapply

# The syntax for lapply is lapply(X, FUN, ...) where X is a vector or list, FUN 
# is the function to apply, and ... are additional arguments for FUN.

# Example: make a list called "y". runif() generates random numbers from a 
# uniform distribution. rnorm() generates random numbers from a normal dist'n.
y <- list(a=runif(10), b=rnorm(12), c=rnorm(15,mean=10,sd=2))
y
# since these are random numbers, your list object will differ from mine.
class(y)
lapply(y,mean)
lapply(y,sd)

# apply a function of our own creation, one that calculates the standard error:
lapply(y, function(x) sd(x)/sqrt(length(x)))

# What we just did was create an "anonymous" function on the fly. We wrote a
# temporary program to calculate the standard error of the 3 list elements in y.

# A digression on writing Functions in R:

# R comes with many functions, such as mean, median, sd, cor, and so on. But R 
# allows you to easily write your own functions. You basically use the
# function() function, with arguments of your own creation. 

# Here's a simple (and useless) function that takes a number and adds 1 to it:

add1 <- function(num) num + 1
add1(num = 4)

# In the function argument we define an argument called "num". After that we 
# create an expression that takes the value of "num". Sort of like f(x) = x + 1.

# We can write functions with more than one argument. Here's a function that
# calculates body mass index (BMI) in pounds:

bmi <- function(weight, height) (weight/(height^2))*703
bmi(weight=215, height=69)

# We can add yet another argument (with a default) to specify metric or English
# units:
bmi <- function(weight, height, metric=TRUE){
  if(metric==TRUE) weight/(height^2)
  else (weight/(height^2))*703
}
bmi(215,69,metric=FALSE)

# The if() function evaluates a condition. If TRUE, it executes whatever 
# follows. The else statement that follows the if() executes in the event the 
# condition in the if() function is FALSE.

# We can also add a check to return a specific error message if someone tries
# to use bmi() with something other than numbers:
bmi <- function(weight, height, metric=TRUE){
  if(!is.numeric(weight) || !is.numeric(height)) stop("please enter numbers")
  if(metric==TRUE) weight/(height^2)
  else (weight/(height^2))*703
}

# is.numeric returns TRUE if a vector is numeric. !is.numeric returns TRUE if a 
# vector is not numeric. The stop() function stops a function and returns an
# error message.

# And down the rabbit hole we go! R programming is quite powerful. However as 
# I've said before, you can be an effective data wrangler and analyst without 
# being a prolific R developer/programmer. On the other hand, I encourage you to
# learn more. The Art of R Programming by Normal Matloff is a great book to get 
# you started. But for this class, this is enough programming. Our goal is to 
# quickly and efficiently get data into R and prepare it for analysis. We can 
# almost always do that with existing functions or simple functions of our own
# creation.

# Once we create a function we can use it by itself or use it with an apply 
# function. Below we create a function called "spread" that calculates the 
# difference between the maximum and minimum values in a vector. The diff()
# function is a base R function that calculates lagged differences. 

spread <- function(x) diff(range(x)) 
# use on a vector
spread(y$a)
# use with lapply on a list
lapply(y, spread)

# Recall that data frames are actually lists, so we can use lapply on data 
# frames. Let's use spread on the weather data frame. Now, obviously spread 
# won't work on non-numeric functions, so we need to select only numeric 
# columns. Let's modify our spread function to only work for numeric vectors or
# otherwise give a message. To do this, we'll need to use {} braces so our
# function can have multiple lines of code. We also add na.rm=TRUE to the range
# function to make it drop NA values before calculating the range.

spread <- function(x) {
  if(!is.numeric(x)) "Not a number"
  else diff(range(x, na.rm = TRUE)) 
}

lapply(weather, spread)


# sapply

# The syntax for sapply is pretty much identical to lapply, except the output is
# simplified.

# Example:
sapply(y,mean)
sapply(y,sd)
# apply a function of our own creation, one that calculates the standard error:
sapply(y, function(x) sd(x)/sqrt(length(x)))

# Watch what happens when we use sapply() with our spread function:
sapply(weather, spread)

# Notice "simplification" has resulted in everything being converted to character.

# tapply

# The syntax for tapply() is tapply(X, INDEX, FUN, ...), where X is usually a 
# vector, INDEX is a list of one or more factors, each of same length as X, FUN
# is the function to apply, and ... are additional arguments for FUN.

# A good way to think of tapply is...
# 1. take the 1st vector
# 2. split into groups according to 2nd vector
# 3. apply the given function to each group.

# Example:
# 1. take weather$Mean.TemperatureF
# 2. split into groups according weather$Events
# 3. apply the given function to each group.

tapply(weather$Mean.TemperatureF, weather$Events, mean)

# So we see that tapply is a sort of short cut for lapply or sapply when you 
# have uneven or "ragged" groups, that is groups of unequal size, that are not
# in a list.

# In later lectures we'll see that there are other friendlier, easier-to-use
# functions for aggregation.

# Deleting columns from data frame ----------------------------------------

# To drop unwanted columns from a data frame, assign the NULL value to the
# column. Let's drop the WindDirDegrees column from weather
weather$WindDirDegrees <- NULL
any(names(weather)=="WindDirDegrees")

# remember the election data? It's a mess:
str(electionData)

# It appears to have "junk" columns that consist of nothing but missing values.
# View the data in RStudio by clicking on it in the Environment panel.

# We would like to identify which columns consist of all missing data and then 
# drop those columns from the data frame. We can use a combination of is.na(),
# all(), which() and sapply()

# Apply the function all(is.na(x)) to each column of electionData. Notice that
# electionData is a data frame, and that a data frame is a type of list. Thus we
# can use lapply and sapply on data frames.
sapply(electionData, function(x)all(is.na(x)))

# But we only want the columns that evaluate to TRUE:
which(sapply(electionData, function(x)all(is.na(x))))

# so we want to drop columns 72 and 84. Save those values into "drop":
drop <- which(sapply(electionData, function(x)all(is.na(x))))
drop
electionData <- electionData[,-drop]


# Deleting rows from data frame -------------------------------------------

# Look again at electionData. Several empty rows read in with all NA.
tail(electionData)

# Row 53 contains column summaries which I don't want. Plus rows 54 and 56 have 
# stray figures not relevant to the main records of the data set. We can 
# specifically remove those columns using a '-' sign in front of a vector
# containing the row numbers:
electionData <- electionData[-c(53:56),]

# Now we want to identify row numbers for rows of all missing data and drop from
# data frame. Notice that electionData is a data frame but we use apply(). Can
# we do that? Yes, just know that apply() first converts data frames to matrices
# before it applies a function. In this case it's OK because NA is the same no 
# matter the class.
drop <- which(apply(electionData, 1, function(x)all(is.na(x))))
drop
electionData <- electionData[-drop,]



# Adding columns/variables to data frames ---------------------------------

# We often want to add columns to a data frame based on calculations using 
# existing columns. This is very easy to do in R. Simply call the data frame 
# with a $ operator followed by the name of a new variable. Then assign to it 
# the expression that derives your new variables. Let's create a new variable
# called Temp.Range that is equal to the maximum temperature minus the minimum
# temperature for a given day.
weather$Temp.Range <- weather$Max.TemperatureF - weather$Min.TemperatureF

# Notice you have to include the name of the data frame each time you refer to a
# variable. In a moment, we'll present another way where we don't have to do
# this.

# Whenever you derive a new variable it's good to look it over and see if it
# makes sense!
summary(weather$Temp.Range) # summary stats; min = -33?
# which record is this?
weather[weather$Temp.Range == -33,]
# Any other records less than 0?
weather[weather$Temp.Range < 0,]
# a plot against index value is also useful:
plot(weather$Temp.Range) 

# Looks like Min.TemperatureF = 99 may be a missing value code.

# Let's make an indicator called "freezing" for days that never got above 32:
weather$freezing <- ifelse(weather$Max.TemperatureF <= 32, 1, 0)
head(weather$freezing)
sum(weather$freezing) # number of days that never got above freezing
which(weather$freezing==1) # which rows?
weather[weather$freezing==1,"EST", drop=FALSE] # which days?

# We can also use the within() function to derive new variables and add to a 
# data frame. The syntax is within(data, expr), where data is a data frame and
# expr is some expression. This allows us to not have to reference the data
# frame each time we refer to a value.

# Let's add a variable for humidity range:
weather <- within(weather, humidity.range <- Max.Humidity - Min.Humidity)

# within() allows you to change multiple variables and even use variables you 
# just created. Just make sure you use curly brackets: {}. Here we create a mean
# temperature variable in celsius units and then standardize the new variable. 
# Standardizing a variable means converting to number of standard deviations
# from mean.
weather <- within(weather, {
  Mean.TemperatureC <- (Mean.TemperatureF - 32)/1.8
  Mean.TemperatureCZ <- (Mean.TemperatureC - mean(Mean.TemperatureC))/sd(Mean.TemperatureC)
})

# NOTE: While using within() results in cleaner code, it means we can't take
# advantage of RStudio's Tab completion since we're not explicitly referencing
# the data frame.


# Rename columns in a data frame ------------------------------------------

# First we can see existing names with the names() function
names(weather)

# But not only can we see names with the names() function, we can change names
# with the names() function.
names(weather)[21] <- "Cloud.Cover.Index"

# This is just a nightmare....
names(electionData) 

# Let's clean up the electionData names. First we notice that the column names 
# were split across two rows in the original Excel file. Look at electionData in
# the viewer. Or enter View(electionData) in the console.

# extract names into vector
top <- names(electionData) 
top
bot <- electionData[1,] # extract first row
# look at bot; it's a data frame with "names"
bot # ugly!
class(bot)
names(bot) # has the same column names 
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
# example, move temp.range next to the temperature columns:
names(weather)
weather <- weather[,c(1:4,24,5:23,25:27)]


# Document a data frame ---------------------------------------------------

# The comment() function allows you to add comments to a data frame. This is
# good for documentation purposes.
comment(arrests) <- "Analysis of Arrests in Paris, June 1848"

# To see comments on a data frame:
comment(arrests)

# We can set our own object attributes using the attr() function. The syntax is 
# attr(x, which), where x is the object and which is the name of your attribute.
# For example, I could create an attribute called URL that contains the link to
# where I downloaded tese data:
attr(arrests, "URL") <- "http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/00049"

# Notice the comment and URL attributes are printed at the bottom of str()
# output as an "attr", or Object Attribute.
str(arrests)

# To view all object attributes, use attributes(). Probably not wise to use on a
# large data frame since one of the attributes is "row.names", which usually
# contains row numbers. To view specific attributes, use the attr() function:
attr(arrests, "URL")

# save data for next set of lecture notes
save(list=c("electionData", "weather", "arrests", "allStocks"), file="../data/datasets_L03.Rda")
