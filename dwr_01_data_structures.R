#' ---
#' title: "Data Wrangling in R: Data Structures"
#' author: "Clay Ford"
#' date: "Spring 2015"
#' output: pdf_document
#' ---


# Vectors -----------------------------------------------------------------

# data of same type (ie, number, integer, character, logical) in one dimension

x1 <- 1:12
x1 # integer
e <- rnorm(12)
e # number
y <- 5 + 3*x1 + e
y # number
test <- y < 20
test # logical
ch <- ifelse(y < 20, "Less Than","Greater")
ch # character

# vectors can be combined
xy <- c(x1,y)
xy
# notice all elements in vector converted to numeric
class(xy)
# what happens when everything combined?
all <- c(x1, e, y, test, ch)
all
class(all)
# notice that R didn't issue a warning.

# accessing elements of a vector with bracket notation
e
e[1:3] # first three
e[-(1:3)] # all but first three
e[c(1,3,6)] # first, third and sixth
e[e > 0] # "e such that e is greater than 0"
e[e > 0 & e < 1] # "e such that e is greater than 0 and less than 1"


# Matrices ----------------------------------------------------------------

# data of same type (ie, number, integer, character, logical) in two dimensions.
# Probably good to think of a matrix as a vector with instructions on how to lay
# out the data on screen.

x2 <- matrix(1:12, ncol=2)
x2 # notice data entered "by column"

e2 <- matrix(rnorm(12), nrow=3)
e2

test2 <- e2 < 0 
test2

# a matrix can be created by "binding" vectors
# using vectors e and y created above...
eyCol <- cbind(e, y) # cbind = column bind
eyCol
eyRow <- rbind(e, y) # rbind = row bind
eyRow

# getting dimensions of matrix
dim(eyCol)
# notice the dimensions are output as a vector

# accessing elements of a matrix with bracket notation
e2
e2[1,1] # element in row 1 column 1
e2[1:2,1:2] # first two rows, first two columns
e2[c(1,3),c(1,3)] # row 1 and 3, column 1 and 3
e2[3,] # row 3
e2[,2] # column 2; output as vector
e2[,2, drop=F] # column 2; output as 1 column matrix
e2[cbind(c(1,2,3),c(1,2,3))] # row 1 col 1, row 2 col 2, and row 3 col 3

# Arrays ------------------------------------------------------------------

# data of same type (ie, number, integer, character, logical) in n dimensions.

x3 <- array(1:12, dim = c(2,2,3))
# dim = c(2,2,3) means 2 rows, 2 columns, 3 layers
x3

x4 <- array(rnorm(24), dim=c(2,2,3,2))
# dim = c(2,2,3) means 2 rows, 2 columns, 3*2 = 6 layers
x4

# we won't deal with arrays in this class


# Data Frames -------------------------------------------------------------

# data of different types in rectangular arrangement, ie, the way you're 
# probably used to seeing data. Like a spreadsheet. When doing data analysis 
# with R you usually want your data in a data frame. It's important to
# understand distinction between matrices and data frames.

# combine vectors from above into data frame
dat <- data.frame(x1, e, y, test, ch)
dat
# can provide descriptive column names
dat <- data.frame(id=x1, error=e, response=y, condition=test, result=ch)
dat

# use the str function to see the structure of an R object
str(dat)

# What happened to our character vector? converted to Factor. R does this by
# default when character vectors are added to a data frame.
class(dat$result)

# Quick side note: Notice we can access the vectors of a data frame directly
# using the "$" notation: "dataframe name" + "$" + "column name"
dat$result

# What is a factor? A factor is a vector of "integer codes" with a "levels"
# attribute.
str(dat$result)
# 2 = "Greater", 1 = "Less Than"

# We will study factors in greater detail in a later lecture; for now it is
# enough to think of Factors as the class for categorical variables.

# accessing elements of a data frame with bracket notation (similar to matrix)
dat[1:2, 1:3] # first two rows, first three columns
dat[,4] # column 4 as a vector
dat[,4, drop=F] # column 4 as a column of a data frame
dat[,"condition"] # using column name to access column
dat$condition # another way
dat[1,] # row 1

# Lists -------------------------------------------------------------------

# data of different types. There is no restriction on shape. A list can contain 
# all sorts of objects of all sorts of size. It is the most general data 
# structure. It can contain vectors, data frames and even other lists. Output of
# statistical analyses are often stored in a list. A data frame is actually a
# list comprised of vectors of the same length.

# store 2 vectors, a matrix, an array and data frame in a list
exList <- list(e, y, x2, x3, dat)
exList

# Notice the list elements are numbered with two brackets

# view the structure of a list
str(exList)

# We can also name the list elements
exList <- list(error=e, response=y, myMatrix=x2, anArray=x3, DataFrame=dat)
exList

# now we can access list elements by name
exList$response
exList$DataFrame

# lists can contain other lists
exList2 <- list(error=e, response=y, myMatrix=x2, anArray=x3, DataFrame=dat, 
                myList=exList)
# look at the structure
str(exList2)

# accessing elements of a list

# we can use the "$" notation repeatedly to access elements
exList2$DataFrame$error
exList2$myList$DataFrame$error

# can also use brackets
# one set accesses list element and returns list
exList2[5]
class(exList2[5])
# two sets access list element and returns the element
exList2[[5]]
class(exList2[[5]])


# Missing Values ----------------------------------------------------------

# Missing values happen. Sometimes your source data has missing values, other
# times missing values occur due to computation or a data transformation.

# In R, the value NA without quotes represents a missing value.

# Let's create a missing value in row 1, column 2 of dat
dat[1,2] <- NA
# now look at row 1
dat[1,]

# we can use the is.na() function to test for missing values
is.na(dat$error)

# You can think of this function as asking each element in the vector the
# question "are you missing?"

# We can reverse it and ask "are you not missing?"
!is.na(dat$error)

# The all(), any() and which() functions are useful for identifying and working
# with missing values:

# all() - tells you if all elements are true
# any() - tells you if any elements are true
# which() - identifies which elements are true

# are all elements in vector missing?
all(is.na(dat$error))
# are any elemens in vector missing?
any(is.na(dat$error)) 
# which elements are missing?
which(is.na(dat$error))

# again we can ask the opposite question:

# are all elements not missing?
all(!is.na(dat$error))
# are any elements not missing?
any(!is.na(dat$error))
# which elements are not missing?
which(!is.na(dat$error))

# another useful function is complete.cases(). It returns a logical vector
# indicating which rows have no missing values.
complete.cases(dat)

# The first row is not a "complete case".

# and again we can ask the opposite: which rows have missing values?
!complete.cases(dat)


# We can combine the above functions to help us quickly identify missing data in
# large data frames. Let's use the airquaility data set that comes with R to
# illustrate.

head(airquality)

# Notice the missing values in the first and second columns. Other columns may 
# have missing data as well. We'd prefer not to rely on our eyes to spot missing
# data.

# Let's use complete.cases
complete.cases(airquality)

# Lots of FALSE elements. How many? Recall that TRUE and FALSE can be treated as
# 1 and 0. Therefore we can use the the sum function to count the number of
# TRUE/FALSE elements.

# How many complete cases?
sum(complete.cases(airquality))

# How many incomplete cases?
sum(!complete.cases(airquality))

# which rows have incomplete cases?
which(!complete.cases(airquality))

# we can save the output of the above line of code and use it to view the subset
# of records with missing data.
miss <- which(!complete.cases(airquality))
airquality[miss,]

# The summary() function when called on a data frame will also give you
# information about missing values.
summary(airquality)

# what if you want to drop all records with missing values? This can be done
# with the na.omit() function:
aq2 <- na.omit(airquality)
summary(aq2)

# Notice there are no missing values in the data frame summary.
