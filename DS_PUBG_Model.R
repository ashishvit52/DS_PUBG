library(dplyr)
database <- read.csv('../input/train_V2.csv')
summary(database)

# Check out missing values
sapply(database, function(x) sum(is.na(x))) # only 1 missing value in winPlacePerc
database[is.na(database$winPlacePerc),] # player id f70c74418bb064
train <- database[-2744605,] # remove the one record with missing winPlacePerc

# Correlation matrix for numeric variables
numeric.var <- sapply(train, is.numeric)
corr.matrix <- cor(train[, numeric.var])
corr.matrix
# write.csv(corr.matrix, 'corrmatrix.csv')

# Remove killStreaks, numGroups, rankPoints, winPoints because of correlations; remove id variables
remove.var <- c("killStreaks", "numGroups", "killPoints", "rankPoints", "winPoints",
                "Id", "groupId", "matchId")
trainset <- train[ , !(names(train) %in% remove.var)]
glimpse(trainset)

# General linear regression
general.lm <- lm(winPlacePerc ~ ., data=trainset)
summary(general.lm)

#Test Data
# test <- read.csv('../input/test_V2.csv')
# # Apply chosen model to the test set
# test.pred <- predict(general.lm, test)
# # Generate submission file
# test.output <- data.frame(test$Id, test.pred)
# names(test.output) <- c("Id", "winPlacePerc")
# glimpse(test.output)