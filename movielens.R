# Data Science Professional Certificate
# Course: HarvardX - PH125.9x
# Capstone : MovieLens

# !!! 20 minutes to execute the whole code
rm(list = ls())
gc()
# to permit execution in 8 Go RAM machine


#Loading data####

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse",
                                         repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret",
                                     repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table",
                                          repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate")
if(!require(knitr)) install.packages("knitr")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(knitr)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", 
                             readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), 
                          "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
# movies <- as.data.frame(movies) %>% 
#  mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
#                                           genres = as.character(genres))

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") 
# if using R 3.5 or earlier, use `set.seed(1)`

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, 
                                  list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# end of provided code in the course #
gc()


####################################################################
# Initial exploration and visualization ####
## dimension ####
dim(edx)

## head ####
head(edx, 12)

## structure ####
str(edx)

## missing values ####
sapply(edx, function(x) sum(is.na(x))) %>% 
  kable(col.names = c("Missing Values"))

## Number of movies and users : ####
summarize(edx, users = n_distinct(edx$userId), 
          movies = n_distinct(edx$movieId)) %>%
  kable(caption = "Number of Users and Movies", 
        col.names = c("Unique Users", "Unique Movies"))

# cleaning data ####
## cleaning edx data set ####
# timestamp to a human readable object
edx$timestamp <- edx$timestamp %>%
  as.POSIXct(origin = "1970-01-01") %>% year()

# extract the year of release
edx <- edx %>%
  mutate(title = str_trim(title)) %>%
  extract(title,
          c("titleTemp", "release"),
          regex = "^(.*) \\(([0-9 \\-]*)\\)$",
          remove = F) %>%
  mutate(release = if_else(str_length(release) > 4,
                           as.integer(str_split(release, "-",
                                                simplify = T)[1]),
                           as.integer(release))
  )  %>%
  select(-titleTemp)

gc()

# separate genre. Takes 3 minutes !!
edx <- edx %>% separate_rows(genres, sep = "\\|")

gc()

## cleaning validation data set ####
# timestamp to a human readable object
validation$timestamp <- validation$timestamp %>%
  as.POSIXct(origin = "1970-01-01") %>% year()

# extract the year of release on validation data set
validation <- validation %>%
  mutate(title = str_trim(title)) %>%
  extract(title,
          c("titleTemp", "release"),
          regex = "^(.*) \\(([0-9 \\-]*)\\)$",
          remove = F) %>%
  mutate(release = if_else(str_length(release) > 4,
                           as.integer(str_split(release, "-",
                                                simplify = T)[1]),
                           as.integer(release))
  )  %>%
  select(-titleTemp)

gc()

# separate genre in validation data set
validation <- validation %>% separate_rows(genres, sep = "\\|")

gc()





# Exploratory Data Analysis ####
## distribution of movies ####
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies distribution") + theme_minimal() +
  xlab("number of ratings")

gc()

# First twelve most rated movies
edx %>% group_by(movieId, title) %>% summarise(n = n()) %>%
  arrange(desc(n)) %>% head(10)

gc()

## Ten most active users ####
edx %>% dplyr::count(userId) %>% arrange(desc(n)) %>% head(10)

# user distribution 
edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("User distribution") +
  xlab("number of ratings") + theme_minimal()

## there are 10 possible choices rating between 0.5 and 5 ####
length(edx$rating)
data.frame(table(edx$rating)) %>% kable()

# Most users give rounded rating :
edx %>% ggplot(aes(rating)) + geom_bar() + theme_minimal() +
  ggtitle("Distribution of rating")

## The rating starts in 1995 and ends in 2009 ####
range(edx$timestamp)

# Number of rating per year
edx %>% group_by(timestamp) %>% summarise(n=n()) %>%
  arrange(desc(n))

# Number of rating per year
edx %>% ggplot(aes(timestamp)) + geom_bar() + theme_minimal() +
  ggtitle("Distribution of rating") +
  xlab("year") +
  ylab("number of ratings")

## year of release starts in 1915 ####
range(edx$release)

# year of release distribution
edx %>% group_by(release) %>% summarise(n = n()) %>%
  ggplot(aes(release, n)) + geom_line(color = "blue") + 
  theme_minimal() + ggtitle("Distribution of release") + 
  ylab("number of ratings")

gc()

## Number per genre ####
edx %>% count(genres) %>% arrange(desc(n))

# Distribution of rating per genre
edx %>% mutate(genres = factor(genres,
                               levels = names(sort(table(edx$genres))))) %>%
  ggplot(aes(genres)) +  geom_bar(fill = "green") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("number of movie per genre")







# Data processing ####
## create a function of Residual Mean Square Error ####
RMSE <- function(actual , prediction){
  sqrt(mean((actual - prediction)^2))
}

## Create train_set and test_set ####
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating, 
                                  times = 1, p = 0.25, list = FALSE)

train_set <- edx[-test_index, ]
temp <- edx[test_index, ]

# select movies and users who aren't in the train_set and remove them from
# the test_set
test_set <- temp %>% semi_join(train_set, by = "movieId") %>% 
  semi_join(train_set, by = "userId")
removed <- anti_join(temp, test_set)

# add them to the train_set
train_set <- rbind(train_set, removed)

rm(temp, test_index, removed, edx)
gc()


# Results ####

## step_by_step model 
## just the mean : predict rating as the average ####
avg_rating <- mean(train_set$rating)

# RMSE (just the mean)
just_avg <- RMSE(test_set$rating, avg_rating)

results <- data.frame("method" = "just the avg", "rmse" = just_avg)
results %>% kable()
rm(just_avg)

## movie effect ####
movie_avg <- train_set %>%
  group_by(movieId) %>%
  summarize(movie_eff = mean(rating - avg_rating))

movie_eff <- test_set %>% left_join(movie_avg, by = "movieId") %>% 
  mutate(pred = avg_rating + movie_eff) %>% pull(pred)

# RMSE (movie effect)
movie_rmse <- RMSE(test_set$rating, movie_eff)

results <- results %>% add_row(method = "movie_eff", 
                               rmse = movie_rmse)
results %>% kable()
rm(movie_rmse)
gc()

## user effect ####
user_avg <- train_set %>%
  left_join(movie_avg, by='movieId') %>%
  group_by(userId) %>%
  summarize(user_eff = mean(rating - avg_rating - movie_eff))

user_eff <- test_set %>% 
  left_join(movie_avg, by = 'movieId') %>%
  left_join(user_avg , by = 'userId') %>% 
  mutate(pred = avg_rating + movie_eff + user_eff) %>% pull(pred)

gc()

# RMSE (user effect)
user_rmse <- RMSE(test_set$rating , user_eff)
results <- results %>% add_row(method = "user_eff", 
                               rmse = user_rmse)
results %>% kable()
rm(user_rmse)
gc()

## genre effect ####
genre_avg <- train_set %>% 
  left_join(movie_avg, by = 'movieId') %>%
  left_join(user_avg, by = "userId") %>%
  group_by(genres) %>%
  summarize(genre_eff = mean(rating - avg_rating - movie_eff - user_eff))

genre_eff <- test_set %>% 
  left_join(movie_avg, by = 'movieId') %>%
  left_join(user_avg , by = 'userId') %>%
  left_join(genre_avg, by = "genres") %>%
  mutate(pred = avg_rating + movie_eff + user_eff + genre_eff) %>% pull(pred)

# RMSE (genre effect)
genre_rmse <- RMSE(test_set$rating ,  genre_eff)
results <- results %>% add_row(method = "genre_eff", 
                               rmse = genre_rmse)
results %>% kable()
rm(genre_rmse)
gc()

## timestamp effect ####
timestamp_avg <- train_set %>%
  left_join(movie_avg, by = 'movieId') %>% 
  left_join(user_avg, by = "userId") %>%
  left_join(genre_avg, by = "genres") %>% 
  group_by(timestamp) %>%
  summarize(timestamp_eff = 
              mean(rating - avg_rating - movie_eff - user_eff - genre_eff))

timestamp_eff <- test_set %>% 
  left_join(movie_avg, by = 'movieId') %>%
  left_join(user_avg , by = 'userId') %>%
  left_join(genre_avg, by = "genres")%>% 
  left_join(timestamp_avg, by = "timestamp") %>%
  mutate(pred = avg_rating + movie_eff + user_eff + genre_eff + timestamp_eff) %>%
  pull(pred)

# RMSE (timestamp effect)
timestamp_rmse <- RMSE(test_set$rating ,  timestamp_eff)
results <- results %>% add_row(method = "timestamp_eff", 
                               rmse = timestamp_rmse)
results %>% kable()
rm(timestamp_rmse)
rm(avg_rating, movie_eff, movie_avg, user_eff, user_avg, genre_eff,
   genre_avg, timestamp_eff, timestamp_avg)
gc()





## regularization ####
# with user_score, movie_score, and genre_score
# calculate the best lambda
lambdas <- seq(1, 8, 0.5)

# 3 minutes to execute
rmses <- sapply(lambdas, function(l)
{
  avg_rating <- mean(train_set$rating)
  
  movie_score <- train_set %>%
    group_by(movieId) %>%
    summarize(b_movie = sum(rating - avg_rating) / (n()+l))
  
  user_score <- train_set %>% 
    left_join(movie_score, by = "movieId") %>%
    mutate(b_movie = ifelse(is.na(b_movie), 0, b_movie)) %>%
    group_by(userId) %>%
    summarize(b_user = sum(rating - avg_rating - b_movie) / (n()+l))
  
  genre_score <- train_set %>%
    left_join(movie_score, by = "movieId") %>%
    left_join(user_score, by = "userId") %>%
    mutate(b_movie = ifelse(is.na(b_movie), 0, b_movie)) %>%
    mutate(b_user = ifelse(is.na(b_user), 0, b_user)) %>%
    group_by(genres) %>%
    summarise(b_genre = sum(rating - avg_rating - b_movie - b_user) / (n()+l))
  
  predicted_ratings <- test_set %>% 
    left_join(movie_score, by = "movieId") %>%
    left_join(user_score, by = "userId") %>%
    left_join(genre_score, by = "genres") %>%
    mutate(b_movie = ifelse(is.na(b_movie), 0, b_movie)) %>%
    mutate(b_user = ifelse(is.na(b_user), 0, b_user)) %>%
    mutate(b_genre = ifelse(is.na(b_genre), 0, b_genre)) %>%
    mutate(pred = avg_rating + b_movie + b_user + b_genre) %>%
    .$pred
  
  return(RMSE(predicted_ratings, test_set$rating))
})

lambda <- lambdas[which.min(rmses)]
lambda
qplot(lambdas, rmses)
rm(lambdas, rmses)

# lambda which minimises RMSE is 4.5
lambda <- 4.5

avg_rating <- mean(train_set$rating)

movie_score <- train_set %>%
  group_by(movieId) %>%
  summarize(b_movie = sum(rating - avg_rating) / (n()+lambda))

user_score <- train_set %>%
  left_join(movie_score, by = "movieId") %>%
  mutate(b_movie = ifelse(is.na(b_movie), 0, b_movie)) %>%
  group_by(userId) %>%
  summarize(b_user = sum(rating - avg_rating - b_movie) / (n()+lambda))

genre_score <- train_set %>%
  left_join(movie_score, by = "movieId") %>%
  left_join(user_score, by = "userId") %>%
  mutate(b_movie = ifelse(is.na(b_movie), 0, b_movie)) %>%
  mutate(b_user = ifelse(is.na(b_user), 0, b_user)) %>%
  group_by(genres) %>%
  summarise(b_genre = sum(rating - avg_rating - b_movie - b_user) / (n()+lambda))

# predicting 
predicted_ratings <- test_set %>% 
  left_join(movie_score, by = "movieId") %>%
  left_join(user_score, by = "userId") %>%
  left_join(genre_score, by = "genres") %>%
  mutate(b_movie = ifelse(is.na(b_movie), 0, b_movie)) %>%
  mutate(b_user = ifelse(is.na(b_user), 0, b_user)) %>%
  mutate(b_genre = ifelse(is.na(b_genre), 0, b_genre)) %>%
  mutate(pred = avg_rating + b_movie + b_user + b_genre) %>%
  .$pred

regularization_rmse <- RMSE(test_set$rating, predicted_ratings)

results <- results %>% add_row(method = "regularization", 
                               rmse = regularization_rmse)
results %>% kable()
rm(train_set, test_set, lambda, regularization_rmse, predicted_ratings)






# apply the best model on the validation set ####
predicted_ratings <- validation %>% 
  left_join(movie_score, by = "movieId") %>%
  left_join(user_score, by = "userId") %>%
  left_join(genre_score, by = "genres") %>%
  mutate(b_movie = ifelse(is.na(b_movie), 0, b_movie)) %>%
  mutate(b_user = ifelse(is.na(b_user), 0, b_user)) %>%
  mutate(b_genre = ifelse(is.na(b_genre), 0, b_genre)) %>%
  mutate(pred = avg_rating + b_movie + b_user + b_genre) %>%
  .$pred

validation_rmse <- RMSE(validation$rating, predicted_ratings)
results <- results %>% add_row(method = "validation", 
                               rmse = validation_rmse)
results %>% kable()
rm(genre_score, movie_score, user_score, avg_rating, predicted_ratings, 
   validation_rmse)
gc()


