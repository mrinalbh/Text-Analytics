###################################
##################################################################################################Question 1
#Preprocessing the corpus to clean it
library('tm')
library("SnowballC") 
my.text.location <- "C:/Users/mrina/OneDrive/Documents/Text/Assignments/A4/EnglishAbstract/"
apapers <- VCorpus(DirSource(my.text.location))
class(apapers)
mpapers <- tm_map(apapers, removeNumbers)
mpapers <- tm_map(mpapers, removePunctuation)
mpapers <- tm_map(mpapers, stemDocument, language = "en")

###################################################################################################Question 2
#Building a Document Term Matrix and then fiding the most frequent word between all the documents
ptm.tf <- DocumentTermMatrix(mpapers) 
n <- 1
top <- findMostFreqTerms(ptm.tf, n = n)
topunlist <- unlist(top)
sort(topunlist, decreasing = T)[1]

###################################################################################################Question 3
#Building a Document Term Matrix and then finding the most frequent word between all the documents
ptm.tfidf <- DocumentTermMatrix(mpapers, 
                                control=list(weighting=function(x){weightTfIdf(x, normalize=FALSE)}))
y <- 1
top1 <- findMostFreqTerms(ptm.tfidf, n = y)
top1unlist <- unlist(top1)
sort(top1unlist, decreasing = T)[1]

###################################################################################################Question 4
#Creating a data frame consisting of tf and tfidf values witht he document names and terms
ptm.tf.terms <- rep(colnames(ptm.tf[,]), each=dim(ptm.tf[,])[1])
ptm.tf.value <- as.vector(as.matrix(ptm.tf[,]))
ptm.tfidf.value <- as.vector(as.matrix(ptm.tfidf[,]))
ptm.tf.docnames <- rownames(ptm.tf[,])

paperdata <- data.frame(ptm.tf.terms, ptm.tf.docnames, ptm.tf.value, ptm.tfidf.value)
colnames(paperdata) <- c("Terms", "Documents", "tf", "tfidf")
head((paperdata), n = 10)
tail((paperdata), n = 10)

###################################################################################################Question 5
#Aggregating to find sum of tf and tfidf values with the addition of new columns like 'Author', 'Volume' and 'Issue'
summation <- aggregate (cbind(tf,tfidf) ~ docnames, paperdata, sum)
summation$authors <- str_replace (str_extract (summation$docnames, "[[:alpha:]]{0,}[[:punct:]]{0,1}[[:alpha:]]{0,}[[:space:]]{0,1}[[:alpha:]]{1,}_"),"_","")
summation$vols <- str_replace_all(str_extract(summation$docnames, "_[[:digit:]]{2}_"),"_","")
summation$issues <- str_extract(str_extract(summation$docnames,"_[[:digit:]]{1}\\.txt"),"[[:digit:]]{1}")
head(summation, n=10) 
tail(summation, n=10)

####################################################################################################Question 6
#Aggregating to find mean for tf and tfidf alues using the aggregation created in the question 5
meannation <- aggregate (cbind(tf, tfidf)~Author, summation, mean)
head((meannation), n = 10)
tail((meannation), n = 10)

#####################################################################################################Question 7
#Using the question 5 & 6 as the base to merge and create data frame showing only the mean tf and tfidf values with Author name and the number of publication and categorizing them 
meannation1 <- aggregate (Documents~Author, summation, length)
q7 <- merge(meannation,meannation1)
q7$Publication_Productivity <- ifelse(q7$Documents <= 1, '1', ifelse(q7$Documents <= 3, '2', '3'))
names(q7) <- c("Authors", "tf-Mean", "tfidf-Mean", "Total Number of Publications", "Publication Productivity")
head(q7, 10)
tail(q7, 10)
  
