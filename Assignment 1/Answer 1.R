###################################################################All the code written here answers for Question 1
#Used to convert all the upper-case letters to lower-case for apple text
lapple <- tolower(apple)
lapple

#Used to convert all the upper-case letters to lower-case for samsung text
lsamsung <- tolower(samsung)
lsamsung

#counts the total numbe of character in the apple text
nchar(apple)

#counts the total number of character in the samsung text
nchar(samsung)

####################################################################All the code written here answers for Question 2
#StrSplit is used to split the lower-case words in Q1 in word unit
apple.a <- strsplit(lapple, split=' |\\n')
apple.a

#StrSplit is used to split the lower-case words in Q1 in word unit
samsung.a <- strsplit(lsamsung, split=' |\\n')
samsung.a

#Reads the number of characters once the puntuation is removed from the apple text
rapple <- gsub("[[:punct:]]", "", apple.a[[1]])
length(apple.a[[1]])

#Reads the number of characters once the puntuation is removed from the samsung text
rsamsung <- gsub("[[:punct:]]", "", samsung.a[[1]])
length(samsung.a[[1]])

#####################################################################All the code written here answers for Question 3
#Returns the number of characters of the words used in lower-case for apple text
apple.b <- strsplit(lapple, split=' |\\n')
apple_pun <- gsub("[[:punct:]]", "", apple.b[[1]])
x <- c()
for (a in 1:543){
  x[a] = nchar(apple_pun[a])
}
x

#Returns mean of the character for each word used in lower-case apple text
mean(x)

#Returns the number of characters of words used in the lower-case for samsung text
samsung.b <- strsplit(lsamsung, split=' |\\n')
samsung_pun <- gsub("[[:punct:]]", "", samsung.b[[1]])
y <- c()
for (d in 1:279){
  y[d] = nchar(apple_pun[d])
}
y

#Returns mean of the character for each word used in lower-case samsung text
mean(y)

#######################################################################All the code written here answers for Question 4
#Count all the space used in apple text
library(stringr)
str_count(apple, ' ')

#Count all the space used in the samsung text
str_count(samsung, ' ')

#######################################################################All the code written here answers for Question 5
#This reports the frequency of words that start with upper-case in Apple text
library("readr")
fapple <- gregexpr("\\b[A-Z]\\w+",apple)
fapple.upper <- regmatches(apple, fapple)
table(fapple.upper)

#This reports the frequency of words that start with upper-case in Samsung text
fsamsung <- gregexpr("\\b[A-Z]\\w+", samsung)
fsamsung.upper <- regmatches(samsung, fsamsung)
table(fsamsung.upper)

#######################################################################All the code written here answers for Question 6
#The code here extracts the years in the Apple text
dapple <- gregexpr ("\\d{4}", apple)
dapple
regmatches(apple, dapple)

#The code here extracts the years in the Samsung text
dsamsung <- gregexpr ("\\d{4}", samsung)
dsamsung
regmatches(samsung, dsamsung)

#This code here reports the total number of years in the Apple text
dapple <- gregexpr ("\\d{4}", apple)
dapple
dapple.d <- regmatches(apple, dapple)
length(dapple.d[[1]])

#This code here reports the total number of years in the Samsung text
dsamsung <- gregexpr ("\\d{4}", samsung)
dsamsung
dsamsung.d <- regmatches(samsung, dsamsung)
length(dsamsung.d[[1]])
