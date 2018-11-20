########################################################################################Question 1
#Creates a corpus
library("tm")
my.text.location <- "C:/Users/mrina/OneDrive/Documents/Text/Assignments/A6/Data-Papers-En/"

newpapers <- VCorpus(DirSource(my.text.location))

########################################################################################Question 2
#Finds the total number files inthe folder
length(newpapers)

#########################################################################################Question 3
#Shows the name of the files in the folder
unlist(meta(newpapers,tag='id'))

#########################################################################################Question 4
#Shows what the file 1 & 2 contain
newpapers[[1]]$content
newpapers[[2]]$content

#########################################################################################Question 5
#Gives the terms that have punctuation in them and their frequency
library("stringr")
punctterms <- function(x){str_extract_all(x, "[[:alnum:]]{1,}[[:punct:]]{1,}?[[:alnum:]]{1,}")}
terms <- lapply(newpapers, punctterms)
m <- as.data.frame(table(unlist(terms)))
names(m) <- c("Terms", "Frequency")
m

#########################################################################################Question 6
#Gives all the numbers in the file and their frequency
numterms <- function(x){str_extract_all(x, "[[:digit:]]{1,}")}
nums <- lapply(newpapers, numterms)
n <- as.data.frame(table(unlist(nums)))
names(n) <- c("Numbers", "Frequency")
n

#########################################################################################Question 7
#Removes all the punctuations and then checks if any exist
newpapers1 <- tm_map(newpapers, content_transformer(function(x){gsub('[[:punct:]]+','',x)}))

my.check.func <- function(x){str_extract_all(x, "[[:punct:]]")}
my.check1 <- lapply(newpapers1, my.check.func)
p <- as.data.frame(table(unlist(my.check1)))
p

##########################################################################################Question 8
#Removes all the numbers in the file and then checks if it exist
newpapers1 <- tm_map(newpapers1, removeNumbers)

num.check.func <- function(x){str_extract_all(x, "[[:digit:]]{1,}]]")}
my.check2 <- lapply(newpapers1, num.check.func)
d <- as.data.frame(table(unlist(my.check2)))
d

#########################################################################################Question 9
#Cleans the corpus
newpapers1 <- tm_map(newpapers1, stripWhitespace)
newpapers1 <- tm_map(newpapers1, content_transformer(tolower))
newpapers1 <- tm_map(newpapers1, removeWords, words=stopwords("SMART"))
newpapers1 <- tm_map(newpapers1, stemDocument, language="en")

#########################################################################################Question 10
#Shows what the file 1 & 24 contain after cleaning the corpus
newpapers1[[1]]$content
newpapers1[[24]]$content

#########################################################################################Question 11
#Creates a DTM out of the clean corpus and rounds it with 2 digits
ptm.tfidf <- DocumentTermMatrix(newpapers1, control=list(weighting=function(x){weightTfIdf(x, normalize=FALSE)}))
r <- round(ptm.tfidf, 2)
inspect(r)

#########################################################################################Question 12
#Removes sparse and then plots a Dendrogram 
r <- removeSparseTerms(r, sparse = 0.80)
title <-  str_extract_all(meta(newpapers, tag = "id"), "[[:digit:]]+[[:alpha:]]")
rownames(r) <- title
distance <- dist(r, method="euclidian")
hc <- hclust(distance, method="complete")
plot(hc, main='Data Papers Dendrogram')

#########################################################################################Question 13
#Clusters the Dendrogram in 6 and colors the culters
library("dendextend")
hcd = as.dendrogram(hc)
labelColors = c("red", "green", "blue", "yellow", "grey", "pink")
clusMember <- cutree(hc, 6)
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}
clusDendro = dendrapply(hcd, colLab)
plot(clusDendro, main='Data Papers Dendrogram')