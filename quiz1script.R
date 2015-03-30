# loading csv data
data1 <- read.csv('/home/abagri/Dropbox/ashish/R_programming/hw1_data.csv')
# make a row as vector
y <- as.vector(a[1])
# check the type of variable
class(y)
# omit values which are NA and getting the length
length(na.omit(a[[1]]))
# select from a column Ozone, values more than 31. Second (,) means select all data
z <- a[a$Ozone > 31, ]
# more complex selection criteria and selection of only Solar.R column
w <- a[with(a, Ozone > 31 && Temp > 90), Solar.R ]
# advance selection, omit NA values and get mean of values from column 2
mean(na.omit(a[with(a, Ozone > 31 & Temp > 90), 2]))
# create a numeric vector
v <- c(2,3,5,7)
# c converts all the elements of same type. elements will be of char type
j <- c(2,3,4,6,"xx")
# as.<<>> to cast data types