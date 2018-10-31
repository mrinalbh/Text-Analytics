################################################################################################Question 1
#Reads the files from the directiory its stored in (a)
library('tm')
my.text.location <- "C:/Users/mrina/OneDrive/Documents/Text/Chap 4/EnglishAbstract/"
apapers <- VCorpus(DirSource(my.text.location))
class(apapers)
summary(apapers)
papertitles <- unlist(meta(apapers,tag='id'))
papertitles

#Creates an data frame with four object 'Author', 'Volume', 'Issues', 'Id' seperateing the the text file names from the voluem and issue number (b,c)
paper.df <- read.table(text= substring(papertitles, 1, nchar(papertitles) -3), sep='_', fill=TRUE, col.names=c('Author', 'Volume', 'Issue'))
cbind(Id = 1:nrow(paper.df),paper.df)

#################################################################################################Question 2
#Shows the number of total issues per each volume
table(paper.df$Volume)
#################################################################################################Question 3
#Shows the most prolific author 
papermax <- table(paper.df$Author)
sort(papermax, decreasing = TRUE)[1]

#################################################################################################Question 4
####################################Aggregates all the characters without pre processing (a)
#Mean
my_count <- data.frame(t(sapply(apapers, nchar)))
mean(my_count$content)

#Min
min(my_count$content)

#Max
max(my_count$content)

#Standard Deviation
sd(my_count$content)

####################################Aggregates all the words without pre processing (b)
#Mean
library(stringr)
g <- function(x){(str_count(x, "\\w{1,}"))}
q <- lapply(apapers, g)
table(unlist(q))
mean(as.numeric(q), na.rm = T)

#Min
min(as.numeric(q))

#Max
max(as.numeric(q))

#Standard Deviation
sd(as.numeric(q))


##################################################################################################Question 5
######################################Text Cleaning
library("SnowballC") 
library("stopwords")
apapers <- tm_map(apapers, removeNumbers)
apapers <- tm_map(apapers, removePunctuation)
apapers <- tm_map(apapers, removeWords, words = stopwords(source = "smart"))
apapers <- tm_map(apapers, stemDocument, language = "en")

####################################Aggregates all charactes after pre processing (a)
#Mean
my_count <- data.frame(t(sapply(apapers, nchar)))
mean(my_count$content)

#Min
min(my_count$content)

#Max
max(my_count$content)

#Standard Deviation
sd(my_count$content)

####################################Aggregates all words after pre processing (b)
#Mean
g <- function(x){(str_count(x, "\\w{1,}"))}
q <- lapply(apapers, g)
table(unlist(q))
mean(as.numeric(q), na.rm = T)

#Min
min(as.numeric(q))

#Max
max(as.numeric(q))

#Standard Deviation
sd(as.numeric(q))
