#' ---
#' title: "Data Wrangling in R: Data Structures"
#' author: "Clay Ford"
#' date: "Spring 2016"
#' output: pdf_document
#' ---


# In many traditional statistical programs (SAS, Stata, SPSS) data is structured
# in a rectangular format, with observations in rows and variables in columns. 
# The data look and feel like a spreadsheet. That's good most of the time. 
# However it's nice to have different data structures that allow us to perform 
# our own operations with the data. This is what R provides. 

# The basic data structures in R include vectors, matrices, arrays, lists and 
# data.frames. Mastering the subtleties of this structures can make learning R 
# hard at first. But once mastered, they allow you to efficiently manipulate and
# investigate your data.


# Vectors -----------------------------------------------------------------

# data of same type (ie, number, integer, character, logical) in one dimension

# create an integer vector
x1 <- 1:12
x1 
is.vector(x1)  # Is it a vector? returns T/F
mode(x1)  # The (Storage) Mode of an Object
typeof(x1) # The Type of Object
length(x1) # how many elements in the vector?

# What about this?
z <- 1
length(z)
is.vector(z) # Yes it is! (Not a scalar.)

# numeric vector
e <- rnorm(12) # 12 random values drawn from a standard normal dist'n N(0,1)
e 
is.vector(e)
mode(e)
typeof(e) # double-precision

# I can work directly with these vectors:
plot(x = x1, y = e, type = "b") # type = "b" means dots and lines
mean(e)
sd(e)
e + x1

# Notice in e + x1 we added the first element of e to the first element of x1, 
# the second element of e to the second element of x1, and so forth. That's an 
# example of a "vectorized" operation. We didn't have to create a loop to
# accomplish this. We could just add the vectors. More examples:

x1 * e
x1 / 10
sqrt(x1)
abs(e) + 100
sum(x1)

# A little algebra example: Graph the line y = 2x^3 + 3x + 2
x <- 1:10
y <- 2*x^3 + 3*x + 2
plot(x,y,type="l")

# Another vector
y <- 5 + 3*x1 + e
y 

# Notice we just overwrote the previous y without warning.


# I can also name the elements in a vector using the names() function. First 
# let's make a new vector using the c() function. The c() function "combines" or
# "concatenates" values into a vector:
x2 <- c(3,7,1) 

# Now name the elements
names(x2) <- c("Yes","No","Undecided")   # add names
x2
names(x2) # see the names for x2



# logical vectors
# A logical vector contains TRUE and FALSE values
y < 20 
test <- y < 20
test 
mode(test)
typeof(test)

# Comparison operators: <, <=, >, >=, ==, !=
xx <- 1:4; yy <- c(1,2,10,2)
xx
yy
xx > yy
xx >= yy
xx < yy
xx <= yy
xx == yy
xx != yy
rm(xx, yy) # remove xx and yy from the memory

# TRUE and FALSE correspond to 0 and 1, so we can easily summarize their values.
sum(y < 20)
sum(y < 20)/length(y < 20)
mean(y < 20)  # same as previous

# character vectors
# A vector can have character values
ch <- c("Pancakes","Nachos","Bacon","Cookies")
ch 
mode(ch)
typeof(ch)

# In later lectures we'll talk more about working with character data.

# vectors can be combined using the c() function
xy <- c(x1,y)
xy

# what happens when different modes are combined?
all <- c(x1, e, y, test, ch)
all
mode(all)
# everything converted to character. Notice that R didn't issue a warning.
# Remember: vectors store data of one type.

# accessing elements of a vector with bracket notation
e
e[1:3] # first three
e[-(1:3)] # all but first three (notice the parantheses!)
e[-1:-3] # same as previous
e[c(1,3,6)] # first, third and sixth
e[c(-1,-3,-5)] # all but 1st, 3rd, 5th
e[c(3,3,3)] # repeatedly call elements of a vector
e[length(e)] # the last element

# We can use logical vectors to subset vectors:
e[e > 0] # "e such that e is greater than 0"
e[e > 0 & e < 0.2] # "e such that e is greater than 0 AND less than 0.2"
e[e < 0 | e > 0.2] # "e such that e is less than 0 OR greater than 0.2"

# Conditional operators: &, |, &&, ||
x <- c(TRUE, FALSE, TRUE, FALSE)
y <- c(FALSE, TRUE, TRUE, FALSE)

x & y # AND: check if pairs both TRUE; returns vector
x | y # OR: check if either is TRUE; returns vector

# && and || are trickier. && and || evaluates from left to right examining only
# the first element of each vector. Evaluation proceeds only until the result is
# determined. In other words, if the answer is known with the first comparison,
# there is no need to proceed to the second. && and || typically go with if()
# statements when programming. They return a length-one logical vector.

x && y  # stops after first comparison
x || y  # also stops after first comparison

# accessing elements of a vector with functions
e
max(e)
which.max(e) # location of (first) max in vector
min(e)
which.min(e) # location of (first) min in vector

# Let's say we have 5 judge panel scoring a diving competition and they score a
# dive as follows:
(score <- c(6,6.5,5.5,6,5))

# The lowest and high scores are dropped.
nscore <- score[c(-which.min(score),-which.max(score))]
nscore

# We then sum these scores and multiple by degree of difficulty, say 2.0.
sum(nscore) * 2

# Another way to extract largest and smallest values
range(e) # smallest and largest values

# storage information about a vector (or any object)
object.size(e)
print(object.size(e),units = "Kb")

# Matrices ----------------------------------------------------------------

# A matrix is a vector with instructions on how to lay out the data on screen in
# two dimensions.

# Use the matrix() function to create a matrix. The basic syntax is matrix(data,
# nrow, ncol), where data is typically a vector, nrow is number of rows, and 
# ncol is the number of columns. By default, a matrix is "filled" by starting at
# the top of the first column and going down, then going down the second column,
# etc.
x2 <- matrix(1:12, ncol=2)
x2 # notice data filled by column

# to fill by row, set the byrow argument to TRUE
matrix(1:12, ncol=2, byrow = TRUE)

# let's create more matrices:
e2 <- matrix(rnorm(12), nrow=3)
e2

# matrices are vectors laid out in 2D, so we can do something like this:
test2 <- e2 < 0 
test2
e2[test2] # pull out elements that are greater than 0; returns a vector

# a matrix can be created by "binding" vectors;
# using vectors e and y created above...
eyCol <- cbind(e, y) # cbind = column bind
eyCol
eyRow <- rbind(e, y) # rbind = row bind
eyRow

# getting dimensions of matrix
dim(eyCol)
# notice the dimensions are output as a vector

# accessing elements of a matrix with bracket notation.
# [row number(s), column number(s)]
e2
e2[1,1] # element in row 1 column 1
e2[1:2,1:2] # first two rows, first two columns
e2[c(1,3),c(1,3)] # row 1 and 3, column 1 and 3
e2[3,] # row 3
e2[,2] # column 2; output as vector
e2[,2, drop=F] # column 2; output as 1 column matrix (instead of vector)

# An advanced concept: using a matrix in brackets.
z <- cbind(c(1,2,3),c(1,3,4))
z
# what does the following return?
e2
e2[z] 


# Again a matrix is a vector with instructions on how to lay it out. By simply
# defining the dimensions, we can turn a vector into a matrix:
e
is.vector(e) # is this a vector?
is.matrix(e) # is this a matrix?
dim(e) # NULL

# define the dimensions for e as 3 x 4:
dim(e) <- c(3,4)
e
is.vector(e)
is.matrix(e)
dim(e)

# Turn e back into a vector by removing the dimensions:
dim(e) <- NULL
e
is.vector(e)
is.matrix(e)

# we can also turn a matrix into a vector using as.vector()
is.matrix(e2)
as.vector(e2)

# is.vector(x) returns TRUE if x is a vector, FALSE otherwise.
# as.vector(x) attempts to coerce x into a vector. 

# Sometimes we want to transpose a matrix. That means make the rows the columns,
# and vice versa. The t() function does this for us.
x2
t(x2)

# Tranposing a matrix is something we sometimes do when performing matrix 
# algebra. R is great for matrix algebra. Other basic operators and functions 
# include %*%, diag, crossprod, det and solve. We won't go further here because
# I'm not sure of everyone's comfort with the math, but google "matrix algebra
# with R" if you want to learn more.



# Arrays ------------------------------------------------------------------

# Arrays are data of the same type (ie, number, integer, character, logical) in 
# more than 2 dimensions. Again, pretty much a vector with instructions on how
# to lay out on screen.

x3 <- array(1:12, dim = c(2,2,3))
# dim = c(2,2,3) means 2 rows, 2 columns, 3 layers
x3
dim(x3)
is.vector(x3)
is.matrix(x3)
is.array(x3)

as.vector(x3)
as.matrix(x3)

# 4 dimentions
x4 <- array(rnorm(24), dim=c(2,2,3,2))
# dim = c(2,2,3,2) means 2 rows, 2 columns, 3*2 = 6 layers
x4

# We can subset using brackets.
x4[1,1,1,1] # element (1,1) in layer , , 1, 1
x4[1,,1,1] # row 1 in layer , , 1, 1
x4[,1,1,1] # col 1 in layer , , 1, 1
x4[,1,1,1, drop = F] # col 1 in layer , , 1, 1, (as a matrix)
x4[,,1,1] # layer , , 1, 1

# Using brackets with arrays can get confusing!
# We won't deal with arrays too much in this class


# Data Frames -------------------------------------------------------------

# A data frame contains data of different types in a rectangular arrangement,
# ie, the way you're probably used to seeing data. Like a spreadsheet. When
# doing data analysis with R you usually want your data in a data frame. It's 
# important to understand distinction between matrices and data frames.

# combine vectors from above into a data frame
dat <- data.frame(x1, y, e, test, ch)
dat
# Notice the ch vector got "recycled".

# A data frame is not a matrix
is.data.frame(dat) == is.matrix(dat)

# can provide descriptive column names
dat <- data.frame(id=x1, response=y, error=e, condition=test, snack=ch)
dat


# we can access columns (vectors) of a data frame using the $ operator:
dat$response
dat$condition
dat$response + dat$error
sum(dat$condition)

is.vector(dat$response)

# try typing dat$ and hitting tab in either the R script or console. What
# happens?


# The str function displays the structure of an R object. This is useful for
# data frames:
str(dat)

# What happened to our character vector? converted to Factor. R does this by
# default when character vectors are added to a data frame.

# What is a factor? Technically, a factor is a vector of "integer codes" with a
# "levels" attribute. Conceptually, it's simply a categorical variable.

# We will study factors in greater detail in a later lecture; for now it is
# enough to think of Factors as the class for categorical variables.

# accessing elements of a data frame with bracket notation (similar to matrix)
dat[1:2, 1:3] # first two rows, first three columns
dat[,4] # column 4 as a vector
dat[,4, drop=F] # column 4 as a column of a data frame
is.data.frame(dat[,4, drop=F]) # Trust me, it's a data frame
dat[,"condition"] # using column name to access column
dat$condition # and of course with $ operator
dat[1,] # row 1
dat[c(3,2,1,1),] # rows 3, 2, 1, and then 1 again, in that order

# using conditions to select rows of data frame; notice we have to preface
# column names with name data frame.
dat[dat$error > 0,]
dat[dat$error > 0, c("id","error")]
dat[dat$error > 0 & dat$response==TRUE,]
dat[dat$snack=="Nachos",]
dat[dat$response==TRUE | dat$condition==TRUE, c("response","condition")]


# R also has functions for returning information about data frames:
head(dat)
tail(dat)
nrow(dat)
ncol(dat)
dim(dat) # both number of rows and columns
summary(dat)

# What happens when we convert to a matrix?
as.matrix(dat)

# Lists -------------------------------------------------------------------

# Data of different types. There is no restriction on shape. A list can contain 
# all sorts of objects of all sorts of size. It is the most general data 
# structure. It can contain vectors, data frames and even other lists. Output of
# statistical analyses are often stored in a list. 

# store 2 vectors, a matrix, an array and data frame in a list using the list()
# function:
exList <- list(e, y, x2, x3, dat)
exList

# Notice the list elements are numbered with two brackets

# view the structure of a list
str(exList)

# We can also name the list elements
exList <- list(error=e, response=y, myMatrix=x2, anArray=x3, DataFrame=dat)
exList

# Now we can access list elements by name using the $ operator just as we can
# with data frames.
exList$response
exList$DataFrame
# we can also use the $ notation repeatedly to access elements
exList$DataFrame$response

# lists can contain other lists
exList2 <- list(error=e, response=y, myMatrix=x2, anArray=x3, DataFrame=dat, 
                myList=exList)
# look at the structure
str(exList2)

# Again, accessing elements of a list using the $ operator
exList2$DataFrame$error
exList2$myList$DataFrame$error

# can also use brackets
# one set of brackets accesses list element and returns list
exList2[5]
class(exList2[5])
# two sets of brackets access list element and returns the element
exList2[[5]]
class(exList2[[5]])

# Want to remove a list element? Assign it NULL.
exList2$myList <- NULL
exList2[["anArray"]]  <- NULL
str(exList2)

# By the way, a data frame is actually a list comprised of vectors of the same length:
is.data.frame(dat)
is.list(dat)
typeof(dat)
mode(dat)

# Finally, if you want to turn a list into a flat vector, use unlist(). 
exList3 <- list(e2, x2)
exList3
unlist(exList3)


# Missing Values ----------------------------------------------------------

# Missing values happen. Sometimes your source data has missing values, other
# times missing values occur due to computation or a data transformation.

# In R, the value NA without quotes represents a missing value.

# Let's create a missing value in row 1, column 2 of dat
dat[1,2] <- NA
# now look at row 1
dat[1,]

# we can use the is.na() function to test for missing values
is.na(dat$response)

# You can think of this function as asking each element in the vector the
# question "are you missing?"

# We can reverse it and ask "are you not missing?" with ! 
!is.na(dat$response)

# The all(), any() and which() functions are useful for identifying and working
# with missing values:

# all() - tells you if all elements are true
# any() - tells you if any elements are true
# which() - identifies which elements are true

# are all elements in vector missing?
all(is.na(dat$response))
# are any elemens in vector missing?
any(is.na(dat$response)) 
# which elements are missing?
which(is.na(dat$response))

# again we can ask the opposite question:

# are all elements not missing?
all(!is.na(dat$response))
# are any elements not missing?
any(!is.na(dat$response))
# which elements are not missing?
which(!is.na(dat$response))

# Another useful function is complete.cases(). It returns a logical vector 
# indicating which rows (or "cases") have no missing values. Very good for data
# frames.
complete.cases(dat)

# The first row is not a "complete case".

# and again we can ask the opposite: which rows have missing values?
!complete.cases(dat)


# We can combine the above functions to help us quickly identify missing data in
# large data frames. Let's use the airquaility data set that comes with R to 
# illustrate. Enter data() at the console to see data that come with R. Or look
# at the datasets package.

head(airquality)

# Notice the missing values in the first and second columns. Other columns may 
# have missing data as well. We'd prefer not to rely on our eyes to spot missing
# data.

# complete.cases tells which *rows* of the data frame have any missing data:
complete.cases(airquality)

# Lots of FALSE elements. How many? Recall that TRUE and FALSE can be treated as
# 1 and 0. Therefore we can use the the sum function to count the number of
# TRUE/FALSE elements.

# How many complete cases?
sum(complete.cases(airquality))

# How many incomplete cases?
sum(!complete.cases(airquality))

# What proportion of airquality rows contain complete cases?
mean(complete.cases(airquality))

# which rows have incomplete cases?
which(!complete.cases(airquality))

# We can save the output of the above line of code and use it to view the subset
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

# we can also use is.na() on a data frame to get a matrix of TRUE/FALSE
# indicating missing data:
head(is.na(airquality))
class(is.na(airquality))

# We can use the colSum or colMean function on matrix such as this to get a
# summary of missing data by column:
colSums(is.na(airquality))
colMeans(is.na(airquality))

