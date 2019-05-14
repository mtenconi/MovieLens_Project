# MovieLens Final Project

## 1. Introduction
# This report is about the construction of a movie recommender system: I custom taste profile and make specific recommendations to users, using already rated movies.
# The original data can be obtained from the GroupLens webpage: https://grouplens.org/datasets/movielens/.  

# The following libraries and files are used for the project:  
# - tydiverse package  
# - caret package  
# - MovieLens 10M dataset (ml-10m)  

## The datasets:
# The MovieLens 10M dataset (ml-10m) has 10000054 million ratings and 95,580 tag applications applied to 10,681 movies by 71,567 users. Users were selected at random. All users had rated at least 20 movies. Original data are contained in the following files: movies.dat and ratings.dat. (https://grouplens.org/datasets/movielens/)  


## Aim of the project:
# To build a movie recommender system, using data available on the GroupLens webpage: https://grouplens.org/datasets/movielens/.

## Key steps:
# First step is to download the data sets and to do data wrangling.  
# After it, I can start with the analysis: I construct three models (A, B, C) for movie recommendation.  
# For each model I predict movie ratings.  
# Then, I evaluate the models by calculating the error loss, using the residual mean squared error (RMSE).  
# At the end I compare the RMSE obtained for the three models. The model that provides the minor error (minor RMSE) is the best recommendation system.  



## 2. Analysis:
# For the following analysis consider y(u,i) = rating for movie i by user u; and y_hat = predicted rating.

## a) Data sets download and data wrangling.
# Datasets and libraries download:
# The following libraries and files are used for the project:  
# - tydiverse package
# - caret package
# - MovieLens 10M dataset (ml-10m)

# Datasets generation and wrangling:
# The downloaded files (rating.dat and movies.dat) are grouped in one data frame, movielens.

# From the file "movielens" I create test and validation sets:  edx = test set, validation = validation set.  
# Validation set is 10% of movielens data.  
# I make sure userId and movieId in validation set are also in edx set.  
# I add rows removed from validation set back into edx set.

# Test and validation sets details:
# Both edx and validation are object of class data.frame composed by 6 variables:  
# - UserID: unique ID for the user.  
# - MovieID: unique ID for the movie.  
# - Rating: a rating between 0 and 5 for the movie.  
# - Timestamp: date and time the rating was given, expressed as seconds since midnight UTC of January 1, 1970.  
# - Title: movie title.  
# - Genres: genres associated with the movie (Action, Adventure, Animation, Children's, Comedy, Crime, Documentary, Drama, Fantasy, Film-Noir, Horror, Musical, Mystery, Romance, Sci-Fi, Thriller, War, Western)
# Each row represents a rating given by one user to one movie.


## b) Construction of three models (A, B, C) for movie recommendation
# Model A: 
# I predict the same rating for all movies regardless of user: 
# y_hat_A = mu + error_u,i	   where the estimate that minimizes the RMSE is the least squares estimate of mu, corresponding to the average of all ratings:  
# y_hat_A = mean(edx$rating)

# Model B: 
# In model B I take in consideration that different movies are rated differently than others (some movies are generally rated higher). Therefore, I add a movie-specific effect (b_i) to the equation of Model A:  
# y_hat_B = mu + b_i  + error_u,i  
# The least square estimate b_i = the average of (y_u,i - mu_hat)    where:  
# y_u,i = rating;  
# mu = y_avg = average of all ratings.

# Model C: 
# In model C I take in consideration that some users are more active in rating than others. Therefore, I add a user effect (b_u) to the equation of Model B:  
# y_hat_C = (mu + b_u + b_i + error_u,i)  
# b_u = the average of (y_u,i - mu_hat - b_i)


## c) Residual mean squared error (RMSE):
# I evaluate the models by calculating the mean squared error (RMSE):  



## 3. Results:
# For the following analysis consider y(u,i) = rating for movie i by user u; and y_hat = predicted rating.

## a) Data sets download and data wrangling.
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

# Datasets generation and wrangling:
# The downloaded files are grouped in one data frame
ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")

# From movielens I create test and validation sets:  edx = test set, validation = validation set.  
# Validation set is 10% of MovieLens data.  
# I make sure userId and movieId in validation set are also in edx set.  
# I add rows removed from validation set back into edx set.
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Test and validation sets details:
str(edx)
str(validation)

# Edx and validation sets in tidy format.  
# Each row represents a rating given by one user to one movie.
head(edx)
head(validation)

## b) Construction of three models (A, B, C) for movie recommendation
# Model A: 
# I predict the same rating for all movies regardless of user: 
# y_hat_A = mu + error_u,i    # y_hat_A = mean(edx$rating)
y_hat_A <- mean(edx$rating)

# Model B: 
# In model B I add a movie-specific effect (b_i) to the equation of Model A (different movies are rated differently than others):
# y_hat_B = mu + b_i  + error_u,i  
# b_i = the average of (y_u,i - mu_hat)
# y_u,i = rating;  
# mu = y_avg = average of all ratings.
y_avg <- mean(edx$rating)

movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - y_avg))

y_hat_B <- y_avg + validation %>%
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

# Model C: 
# In model C I add a user-specific effect (b_u) to the equation of Model B (some users are more active in rating than others):  
# y_hat_C = (mu + b_u + b_i + error_u,i)  
# b_u = the average of (y_u,i - mu_hat - b_i)
user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>% 
  group_by(userId) %>%
  summarize(b_u = mean(rating - y_avg - b_i))

y_hat_C <- validation %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = y_avg + b_i + b_u) %>%
  .$pred

## c) Residual mean squared error (RMSE):
# I evaluate the models by calculating the mean squared error (RMSE):  
rmse_A <- RMSE(y_hat_A, validation$rating)
rmse_B <- RMSE(y_hat_B, validation$rating)
rmse_C <- RMSE(y_hat_C, validation$rating)
rmse_A
rmse_B
rmse_C



## 4. Conclusion
# With the three approaches I get RMSE_A = 1.06, RMSE_B = 0.94, RMSE_C = 0.86.  
# The RMSEs decrease from approach A to C, and approach C, that applies both a user-specific and a movie-specific effect, has the lowest RMSE = 0.86.  
# Therefore, MODEL C is the final movie reccomnder model.
rmse_C