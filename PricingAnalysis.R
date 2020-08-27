## Packages

install.packages("magrittr")
install.packages("dplyr")    # alternative installation of the %>%
install.packages("tidyr")
install.packages("corrplot")

library(plotly)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)

## Read the data
ab.data <- read.csv("D://SJSU//Fall 2019 - SEM_3//BUS 235C//Project//data//AB_NYC_2019.csv",header=TRUE,na.strings=c("","NA"))

## Creating a dataframe for ab.data
df <- data.frame(ab.data)

## View the dataset
dim(df)
View(df)
colnames(df)


#######--------------------Data Cleansing-------------------------------------------------------------------------I

# We can drop the columns name, host_name, host_id as they are not relevant for analysis.
# Keeping id as it is a unique key.
# Drop name, host_name as they are not relevant
df$name <- NULL
df$host_name <- NULL
df$host_id <- NULL
# Check the new data frame after dropping values.
View(df)

# Handling Missing data
install.packages("kableExtra")
library(kableExtra)

## List out all missing values with NA in the dataframe
list_na <- colnames(df)[ apply(df, 2, anyNA) ]
list_na

# Count the number of missing values for each column of the df
numMissingVal <-sapply(df, function(x) sum(length(which(is.na(x)))))  
kable(as.data.frame(numMissingVal)) %>% kable_styling(bootstrap_options = c("striped", "hover", "responsive")) %>% scroll_box(width = "100%", height = "250px")
## last_review and review_per_month have 10052 missing values

# Impute mean for the missing values in reviews_per_month 
df$reviews_per_month <- ifelse(is.na(df$reviews_per_month), mean(df$reviews_per_month, na.rm=TRUE), df$reviews_per_month)

# Check missing values count again by running the below commands again
numMissingVal <-sapply(df, function(x) sum(length(which(is.na(x)))))  
kable(as.data.frame(numMissingVal)) %>% kable_styling(bootstrap_options = c("striped", "hover", "responsive")) %>% scroll_box(width = "100%", height = "250px")

# We still have missing values in last_review col, but in order to fill it, we first need to
# make sure the format of the col is of type Date

# Check the class of each feature and transform as needed
str(df)

# Convert last_review from factor to date.
class(df$last_review)
df$last_review<-as.Date(df$last_review, format = "%m/%d/%y")

# Convert categorical variables into factors
df$neighbourhood_group <- as.factor(df$neighbourhood_group)
df$neighbourhood <- as.factor(df$neighbourhood)
df$room_type <- as.factor(df$room_type)

# Check class of the features after transforming 
str(df)

# We can drop last_review as it is not relevant for analysis.
df$last_review <- NULL
View(df)

# Check missing values count again by running the below commands again
numMissingVal <-sapply(df, function(x) sum(length(which(is.na(x)))))  
kable(as.data.frame(numMissingVal)) %>% kable_styling(bootstrap_options = c("striped", "hover", "responsive")) %>% scroll_box(width = "100%", height = "250px")


########------------ Exploratory Data Analysis---------------------------------------------------------------I
## Now We have a clean dataset to start an exploratory data analysis (EDA) 

# Let us take a look at the statistical summary of each column in the dataset
summary(df)

# Lets check the unique values present in each column of the df

rapply(df,function(x)length(unique(x))) # gives the count of unique elements in each column

# Checking Unique elements present in categorical columns can be helpful for further analysis
unique(df$neighbourhood_group, incomparables = FALSE) # 5 unique neighborhood_groups

unique(df$neighbourhood, incomparables = FALSE) # 221 unique neighborhoods

unique(df$room_type, incomparables = FALSE) # 3 unique types of rooms



#####################----------------------Bi-variate-analysis-------------------------########

## Visualize the dataset to draw  general conclusions
## Analysis and visualization

plot(df$latitude, df$longitude, xlab="latitude",ylab="longitude")
# Central New York can be seen as highly populated.

# Which neighborhood has maximum number of properties?
ggplot(df, aes(neighbourhood_group)) + geom_bar() + labs(x = "Neighbourhood group", y = "Number of Properties") + geom_text(stat='count', aes(label=..count..), vjust= -0.3)  + theme_classic()

## Manhattan and Brooklyn with highest no. of properties

# Which room_types are more in number?

counts <- table(df$room_type)
barplot(counts,xlab="Room Types", ylab="Count")
# It can be seen that Entire home/apt types are most popular in NYC, followed by Private rooms,
# shared rooms are least popular. We may conclude that popular room types would be more expensive
# as well. We will verify that when we perform a bivariate analysis.


# How is the room_type count spread across various neighborhood_groups?

tbl <- with(df, table(room_type, neighbourhood_group))
tbl

ggplot(as.data.frame(tbl), aes(neighbourhood_group, Freq, fill = room_type)) +     
  geom_col(position = 'dodge')

## It can be seen from the above plot that Manhattan has highest entire home/apt among other neighborhood groups
# Private rooms are relatively more popular in Brooklyn
# shared room has the lowest numbers among all neighborhood groups


## How does the price of room_types vary across different neighborhood_groups?

roomTypePrice <- ggplot(df, aes(x=room_type, y=price, fill = neighbourhood_group)) + scale_y_continuous(labels = scales::comma) + geom_point(aes(col = neighbourhood_group, size=price)) + 
  geom_smooth(method="loess", se=F) + labs( x="room type", y="price") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplotly(roomTypePrice)

## Entire home/apt room-types are costliest in Manhattan and cheapest in Staten Island 
## Private rooms are costlier in Queens and Brooklyn compared to Manhattan
## Cheap shared rooms can be found majorly in Queens.

# Lets check the average prices across neighborhoods

df%>%
  group_by(neighbourhood_group)%>%
  summarise(average_price = mean(price))%>%
  ggplot(aes(x = neighbourhood_group, y = average_price, fill = neighbourhood_group)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "neighborhood_group",
    y = "average_price",
    title = paste("NYC airbnb prices")
  )

# Manhattan charges highest average price among other neighborhood groups making it the most
# Manhattan neighborhood. this can be attributed to the fact that Manhattan has the highest number
# of entire home/apt room_types among other neighborhood groups which are again more expensive
# compared to private and shared rooms.

# Let us check which neighborhood properties have a maximum number of reviews 
# with maximum number of reviews  .

df%>%
  group_by(neighbourhood_group)%>%
  summarise(reviews = sum(number_of_reviews))%>%
  ggplot(aes(x = neighbourhood_group, y = reviews, fill = neighbourhood_group)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "neighborhood_group",
    y = "reviews")

# Brooklyn has the highest number of total reviews among other neighbourhood groups
# but we cannot tell from here whether those reviews are good or bad.

##--Pairwise-plots---###

# availability vs price
availPrice <- df %>% group_by(neighbourhood_group) %>% summarise_all(funs(mean)) %>% ggplot(aes(x=availability_365, y=price)) + scale_colour_brewer(palette = "Set1") + geom_point(aes(col=neighbourhood_group, size=price)) +
  labs(x="Availability", y="Price", shape="Price", colour= "Neighbourhood_group")

ggplotly(availPrice) ## run this even if you get warning for the above line of code

## Lower the availability in Manhattan, higher are the prices.

# No. of reviews vs Price
reviewPrice <- ggplot(data = df, aes(x=number_of_reviews, y=price, colour = neighbourhood_group)) + geom_point(aes( size=price)) + scale_y_continuous(limits = quantile(df$price, c(0, 0.99))) + labs(x="No. of reviews", y="Price")
ggplotly(reviewPrice)
## A good or bad review i.e the type of sentiment matters more than the the mere numbers.


## Creating a wordcloud for the name column
install.packages("tm")
install.packages("wordcloud")
install.packages("RcolorBrewer")

library(tm)
library(wordcloud)
library(RcolorBrewer)

get_name = readLines(df$name)
name<-Corpus(VectorSource(df$name))
revs <- tm_map(name, function(x) iconv(x, to='UTF-8', sub='byte'))
revs <- tm_map(revs, PlainTextDocument)


# Remove whitespace, convert to lower case, remove numbers, remove punctuation from the name column
revs <- tm_map(revs, removeWords, stopwords("english")) 
revs <- tm_map(revs, removePunctuation) 
revs <- tm_map(revs, removeNumbers) 
revs <- tm_map(revs, stripWhitespace)

library(textmining)
dtm <- TermDocumentMatrix(revs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = revs, freq = count(revs), min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

df$name <- as.character(df$name)
toString(df$name,width=NULL)
class(df$name)
cd2 <- count(df, df$name)

wordcloud(words = cd2$Project, freq = cd2$n, min.freq = 1)

########## --------Univariate analysis: Explore variables on-by-one----------- ###########

####-----------Normality check-------------######

# Visualize continuous numerical data columns using histogram to check for normality

# Target/Dependent Variable Price
hist(df$price)        # We do not see a very good graph here to draw meaningful conclusions,
hist(log(df$price))   # so we plot the log(price)

# With log(price), we get a much clear visualization of the histogram. We can see that 
# Price is right-skewed.

# Features: 

hist(df$latitude) # Normally distributed

hist(df$longitude) # Normally distributed

hist(df$minimum_nights) 
hist(log(df$minimum_nights)) # Right skewed distribution

hist(df$number_of_reviews)        
hist(log(df$number_of_reviews))   # Right- skewed distribution

hist(df$reviews_per_month)
hist(log(df$reviews_per_month))  # left-skewed distribution

hist(df$calculated_host_listings_count)
hist(log(df$calculated_host_listings_count)) # Right skewed distribution

hist(df$availability_365)
hist(log(df$availability_365)) # Left-skewed distribution

## We can see that except for latitude and longitude, all our features 
# come from non-normal distributions.We may confirm this with the help of normaility plots also
library(ggpubr)

# A q-q plot to check normality
par(mfrow = c(2,2))
ggqqplot(df$price)
ggqqplot(df$minimum_nights)
ggqqplot(df$number_of_reviews)  
ggqqplot(df$reviews_per_month)
ggqqplot(df$calculated_host_listings_count)
ggqqplot(df$availability_365)

# The above plots confirm that the data is not normal

##### Check for outliers in the dataset using boxplots #####

boxplot(df$id, xlab="id")
boxplot(df$latitude, xlab="latitude")
boxplot(df$longitude, xlab="longitude")
boxplot(df$minimum_nights, xlab="minimum_nights")
boxplot(df$number_of_reviews, xlab="number_of_reviews")
boxplot(df$reviews_per_month, xlab="reviews_per_month")
boxplot(df$calculated_host_listings_count, xlab="calculated_host_listings_count")
boxplot(df$availability_365, xlab="availability_365")

## Outliers are present since we do not know the true source of these outliers,
# we will not remove them as these outliers may have an impact.

##########----Correlation matrix----------

# Generating a correlation matrix for numeric data 
head(df)
str(df)

## The default pearson correlation coefficient measures the linear dependence between 
#  two variables.
data.cor = cor(df[,c(1,4,5,7,8,9,10,11,12)], method ="pearson") 
corrplot(data.cor)


## Correlation coefficient values closer to 1 show high correlation between 2 variables,
# and are statistically insignificant
# In such cases, we may drop any of one of the 2 variables as we want the features to be independent 
# Here we can see noumber_of_reviews and reviews_per_month have a high correlation value of 0.6.
# We may drop one of these in future.

round(data.cor, 2)   

##  We can see number of reviews, reviews per month and longitude have -ve coefficients,
# indication a negative correlation to price.


# Correlation matrix with p-values
res2 <- rcorr(as.matrix(df[,c(1,4,5,7,8,9,10,11,12)]))
res2
## The output of the function rcorr() is a list containing the following elements
# - r : the correlation matrix 
# - n : the matrix of the number of observations used in analyzing each pair of variables 
# - P : the p-values corresponding to the significance levels of correlations.

# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P

########----------------Data Transformation------------------------------########################

## Z-score standardization of numeric features ##
# When there are outliers in the data which are important and 
# we don't want to lose their impact, we go with Z score normalization.

head(df)
df.norm <- df[,-7] %>%mutate_if(is.numeric,scale)
head(df.norm)
df.final <- cbind(df.norm,df[,7, drop =FALSE]) # Merging the normalized features with price column
head(df.final)

boxplot(df.final$price, xlab="price")
boxplot(df.final$id, xlab="id")
boxplot(df.final$latitude, xlab="latitude")
boxplot(df.final$longitude, xlab="longitude")
boxplot(df.final$minimum_nights, xlab="minimum_nights")
boxplot(df.final$number_of_reviews, xlab="number_of_reviews")
boxplot(df.final$reviews_per_month, xlab="reviews_per_month")
boxplot(df.final$calculated_host_listings_count, xlab="calculated_host_listings_count")
boxplot(df.final$availability_365, xlab="availability_365")

## Checking for skewness of the data

## If skewness value lies above +1 or below -1, data is highly skewed. 
## If it lies between +0.5 to -0.5, it is moderately skewed. 
## If the value is 0, then the data is symmetric

library(e1071)

skewness(df.final$price) # Highly skewed  
skewness(df.final$id)       # -ve skewed
skewness(df.final$latitude) # +ve skewed
skewness(df.final$longitude) # highly skewed
skewness(df.final$minimum_nights) # highly skwewd
skewness(df.final$number_of_reviews)  # highly skewed
skewness(df.final$reviews_per_month)  # highly skewed
skewness(df.final$calculated_host_listings_count) # highly skewed
skewness(df.final$availability_365)               # highly skewed

## Resolve  positive skewness:
# Common transformations of this data include square root, cube root, and log.

## Resolve negative skewness:
# Common transformations include square, cube root and logarithmic.

# Transformation can be applied directly to the model while building it.


#####---------Build Model--------------#############

# Splitting the dataset into train, validate and test sets

total.rows <- nrow(df.final)
train.size <- floor(0.7*total.rows)
train.rows <- sample(1:total.rows, train.size)
train <- df.final[train.rows, ]
validation.size <- floor(0.2*total.rows)
train.rows <- sample(1:total.rows, train.size)
train <- df.final[train.rows, ]
remaining <- df.final[-train.rows, ]
remaining.rows <- nrow(remaining)
validate.rows <- sample(1:remaining.rows, validation.size)
validate <- remaining[validate.rows, ]
test <- remaining[-validate.rows, ]
dim(train)
dim(validate)
dim(test)
head(test)

# Model 1: Linear Regression model 
## Test out a simple Linear model

train.model <- lm(formula = price ~ neighbourhood_group + latitude + longitude + minimum_nights
                  + room_type + number_of_reviews + calculated_host_listings_count + availability_365, data = train)
summary(train.model) # R2 is just 10.58 % which is extremely low  with a very high error of 215
plot(train.model)

## The residuals are not randomly fitted, indicating the model is not a very good fit.


## To evaluate multicollinearity of multiple regression model, calculating the
# variance inflation factor (VIF) from the result of lm(). 
# If VIF is more than 10, multicollinearity is strongly suggested.

library(fmsb)
VIF(train.model) # VIF is 1.10 < 10

# MAE
mean(abs(train$price-predict(train.model, newdata=train))) # $73


## Model 2: log transformed linear model : The target variable price is right-skewed, so we apply
# log transformation to Price.
lm_log.model = lm(formula = log1p(price) ~ neighbourhood_group + latitude + longitude + minimum_nights
                  + room_type + number_of_reviews + calculated_host_listings_count + availability_365, data = train)

summary(lm_log.model)  ## R2 has improved to 48.8 % which means around 49% of the variability
## in price can be explained by the  features selected in the model.
## error has reduced greatly to 0.49

VIF(lm_log.model) # VIF is 1.9 <10, indicating low multicollinearity

plot(lm_log.model)

## Residuals vs Fitted values has a funnel shape.
## Hence, the model is not a very good fit, but it is much better than the simple linear model

# Lets predict prices using this model

log.predict <- predict(lm_log.model, train)
head(log.predict)


# Compare predicted prices with actual prices
compare <- cbind (actual=train$price, predicted=expm1(log.predict))
head(compare)

# Check Accuracy 
mean (apply(compare, 1, min)/apply(compare, 1, max)) # 72.8%


## Validation set
val.set <- as.data.frame(validate[,c("neighbourhood_group","latitude","longitude","minimum_nights",
                                     "room_type","number_of_reviews", "calculated_host_listings_count","availability_365","price")])
log.predict.val <- predict(lm_log.model, newdata=val.set)

head(expm1(log.predict.val))

# Compare predicted prices with actual prices for the validation set
compare.val <- cbind (actual=val.set$price, predicted=expm1(log.predict.val))
head(compare.val)

# Check validation Accuracy 
mean (apply(compare.val, 1, min)/apply(compare.val, 1, max))

## Predict for Testing set
test.set <- as.data.frame(test[,c("neighbourhood_group","latitude","longitude","minimum_nights",
                                  "room_type","number_of_reviews", "calculated_host_listings_count","availability_365","price")])
log.predict.test <- predict(lm_log.model, newdata=as.data.frame(test.set))

head(expm1(log.predict.test))

# Compare predicted prices with actual prices for the testing set
compare.test <- cbind (actual=test.set$price, predicted=expm1(log.predict.test))
head(compare.test)

# Check Testing Accuracy 
mean (apply(compare.test, 1, min)/apply(compare.test, 1, max))

# MAE for Model
mean(abs(train$price-expm1(predict(lm_log.model, newdata=train))))  # train MAE 
mean(abs(val.set$price-expm1(log.predict.val)))                     # validation MAE
mean(abs(test.set$price-expm1(log.predict.test)))                   # test MAE


## Model 3 : Bagging

library(randomForest)
library(ipred)
bag.model <- bagging(log(price) ~ neighbourhood_group + latitude + longitude + minimum_nights
                     + room_type + number_of_reviews + calculated_host_listings_count + availability_365, data = train, nbagg = 5)
actual <- train$price
predicted <- predict(bag.model, newdata = train)

# Training Accuracy 
sum(actual == predicted)/nrow(train) 


## Model 4: Polynomial Regression


poly.model <- lm(formula = price ~ neighbourhood_group + poly(latitude,3) + poly(longitude,3) + poly(minimum_nights,3)
                 + room_type + poly(number_of_reviews,3) + poly(calculated_host_listings_count,3) + poly(availability_365,3), data = train)
summary(poly.model) # R2 is just 10.44 % which is extremely low  with a very high error of 233
plot(train.model)




## Model 5: Ridge regression model
install.packages("ridge")
library(glmnet)
library(Matrix)
library(ridge)

View(df.final)

x_train <- model.matrix(price ~ neighbourhood_group
                        + room_type + number_of_reviews
                        + calculated_host_listings_count + availability_365, train)[,-12]
y_train = train %>%
  select(price) %>%
  unlist() %>%
  as.numeric()



x_test = model.matrix(price ~ neighbourhood_group
                      + room_type + number_of_reviews
                      + calculated_host_listings_count + availability_365, test)[,-12]

dim(x_test)
y_test = test %>%
  select(price) %>%
  unlist() %>%
  as.numeric()

grid <- 10^seq(10, -2, length = 100)

# Fit ridge regression model on training data
ridge.fit <- glmnet(x_train, y_train, alpha = 0, lambda = grid, thresh = 1e-12) 
plot(ridge.fit) 

# Select the best value of lambda that minimizes training MSE using cross validation
set.seed(1)
cv.out = cv.glmnet(x_train, y_train, alpha = 0) 
bestlam = cv.out$lambda.min 
bestlam
plot(cv.out)

# To extract all of the fitted models 
fit <- cv.out$glmnet.fit
summary(fit)

# Predict on test data
ridge.pred = predict(ridge.fit, s = bestlam, newx = x_test)

# Compare predicted prices with actual prices
compare <- cbind (actual=y_test, predicted=ridge.pred)  # combine
head(compare)

# Check Accuracy
mean (apply(compare, 1, min)/apply(compare, 1, max)) # 


mean((ridge.pred - y_test)^2)
mean (apply(compare, 1, min)/apply(compare, 1, max))  # 67%

rss <- sum((ridge.pred - y_test) ^ 2)  ## residual sum of squares
tss <- sum((y_test - mean(y_test)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
rsq    # R2 is just 9% 


# Model 6: Logistic regression
logistic.model <- glm(formula = log1p(price) ~ neighbourhood_group + latitude + longitude + minimum_nights
                      + room_type + number_of_reviews + calculated_host_listings_count + availability_365, data = df.final)
summary(logistic.model)

###########---------------------Future Scope---------------#############################

# Model 7: Converting target variable Price into categorical ranges


## Categorize price into Low, Medium, High based on the quartiles 

head(df$price)
summary(df$price)
min(df$price)
max(df$price)

## 0 < price <= 106 : low
## 107 < price <= 175 : medium
## price > 300  : high

res <- df %>% mutate(category=cut(price, breaks=c(0, 106, 175, Inf), labels=c("low","medium","high")))

head(res)

as.data.frame(table(res$category))
