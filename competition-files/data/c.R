df<-read.csv("~/STA 631/Activities/activity08-mini-competition/competition-files/data/inventory.csv")
str(df)
# Change sales column to 1 if value > 0, else 0
df$sold <- ifelse(df$sold > 0, 1, 0)
df$week <-factor(df$week)
df$sold<-factor(df$sold)
head(df)


# Remove rows with null values
df <- na.omit(df)

# Install and load the caTools package
install.packages("caTools")
# Install and load the caTools package
install.packages("caTools")
library(caTools)

# Set the seed for reproducibility
set.seed(123)

# Split the data into training and testing sets
split <- sample.split(df$sold, SplitRatio = 0.8)
trainData <- subset(df, split == TRUE)
testData <- subset(df, split == FALSE)

# Fit a logistic regression model on the training data
model <- glm(sold ~ ., data = trainData, family = binomial(link = "logit"))

# Make predictions on the testing data
predictions <- predict(model, newdata = testData, type = "response")

# Evaluate the model performance
# Assuming 'sold' is a binary response variable (0 or 1)
# You can use appropriate evaluation metrics based on your specific problem
# Here, we calculate the accuracy of the predictions
accuracy <- mean(ifelse(predictions > 0.5, 1, 0) == testData$sold)
print(paste("Accuracy:", accuracy))



