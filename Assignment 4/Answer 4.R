########################################################################################################Question 1
#Reading the csv file and getting detailed information about the file
library("dplyr")
my.path <- "C:/Users/mrina/OneDrive/Documents/Text/Assignments/A5/5000-movie-dataset/"
my.file <- "movie_metadata.csv"
filedetas <- read.csv(paste(my.path, my.file, sep = ''))
glimpse(filedetas)
colnames(filedetas)
dim(filedetas)
class(filedetas)

##########################################################################################################Question 2
#Creating ggplot() to show number of movies across the years
library("ggplot2")
filedetas %>% count(title_year, movie_title) %>% ggplot(aes(x = title_year, y = n)) + 
  geom_bar(stat = 'identity', width = 0.8) + xlab("Years") + ylab("Number of Movies") + ggtitle("Number of Movies per Year")

##########################################################################################################Question 3
#Creating ggplot() to show changes across average review score acorss the years
review_movies <- filedetas %>% group_by(title_year) %>% summarise(change_years = mean(imdb_score)) 
ggplot(review_movies, aes(x = title_year, y = change_years)) + geom_freqpoly(stat = "identity") + xlab("Years") + 
  ylab("Average IMDB Score") + ggtitle("Average Rating Change over the Years")

##########################################################################################################Question 4
#Creating ggplot() to show frequency ditribution for all categories of movie content rating and removing the empty values
ratings <- filedetas[filedetas$content_rating!= "", ]
ggplot(data = ratings, aes(content_rating, fill = content_rating)) + geom_bar() + xlab("Content Rating") + 
  ylab("Number Count") + ggtitle("Content Rating Number Count")

##########################################################################################################Question 5
#Creating ggplot() for question 5B to show the top 4 cateogry of content rating as boxplot
ggplot(subset(filedetas, content_rating %in% c("PG", "PG-13", "R", "G"))) + geom_boxplot(aes(content_rating, imdb_score)) + 
  xlab("Top Content Rating") + ylab("IMDB Scores") + ggtitle("Top Content Rating in Box Plot")

##########################################################################################################Question 6
#Creating ggplot() to represent Q5 as density plot placed on top of each other
ggplot(subset(filedetas, content_rating %in% c("PG", "PG-13", "R", "G"))) + 
  geom_density(aes(x = imdb_score, color = content_rating, fill = content_rating, alpha = 0.3)) + 
  xlab("IMDB Score") + ylab("Density") + ggtitle("Top Content Rating in Density")

##########################################################################################################Question 7
#Creating ggplot() to show facebook like frequency
filedetas %>% ggplot(aes(x=movie_facebook_likes)) + geom_freqpoly() + scale_x_log10() + xlab("Movie Facebook Likes in Log") + 
  ylab("Amount of Likes") + ggtitle("Movie Facebook Likes Frequency Distribution")

###########################################################################################################Question 8
#Creating ggplot() to show relation between squared facebook likes and movie review score 
filedetas %>% ggplot(aes(movie_facebook_likes^2, imdb_score, color = imdb_score)) + geom_point() + geom_smooth() + 
  scale_color_gradient(low = "#0091ff", high = "#f0650e") + xlab("Squared Movie Facebook Likes") + 
  ylab("IMDB Scores") + ggtitle("Squared Movie Facebook Likes Scatter Plot")

############################################################################################################Question 9
#Creating ggplot() to show change in squared facebook likes over the years
ggplot(data = filedetas, aes(x = title_year, y = movie_facebook_likes^2, group = title_year)) + geom_boxplot() + 
  xlab("Years") + ylab("Squared Movie Facebook Likes") + ggtitle("Change in  Squared Movie Facebook Likes over the Years")

############################################################################################################Question 10
#Creating ggplot() to show relation between squared facebook likes and movie review sCOre being above 2010 and in U.S.A
filedetas %>% filter(title_year > 2010 & country == "USA") %>% ggplot(aes(movie_facebook_likes^2, imdb_score, color = imdb_score)) + 
  geom_point() + geom_smooth() + scale_color_gradient(low = "#0091ff", high = "#f0650e") + xlab("Squared Movie Facebook Likes") + 
  ylab("IMDB Scores") + ggtitle("Squared Movie Fcaebook Likes Scatter Plot from Year 2010> in USA")

############################################################################################################Question 11
#Reporting correlation value between squared fabeook likes and movie review score being above 2010 and in U.S.A
filedetas1 <- filedetas %>% filter(title_year > 2010 & country == "USA") %>% as.data.frame()
cor(filedetas1$movie_facebook_likes^2, filedetas1$imdb_score)
