# ALY6015 Module1 - Final Project
# Loan Default (Binary Classification)

library(ISLR) 
library(caret) 
library(ggplot2) 
library(gridExtra) 
library(pROC)
library(corrplot) 
library(RColorBrewer)

library(ISLR) 
library(caret) 
library(ggplot2) 
library(gridExtra) 
library(pROC)
library(corrplot) 
library(RColorBrewer)
library(glmnet)
library(Metrics)
library(MASS)
library(car)

# Load tha dataset
df <- read.csv("Loan_default.csv",header = TRUE)
head(df)
str(df)
df <- df[, -1]  # Removes the first column


#df[, 10:17] <- lapply(df[, 10:17], factor)  # Converts columns 10 to 17 to factor
str(df)  
summary(df)
colnames(df)

################################################################################
# Exploratory Data Analysis (EDA)

# Distribution of categorical variables
table(df$Education)
table(df$EmploymentType)
table(df$LoanPurpose)

# Bar plot for Default
ggplot(df, aes(x = factor(Default))) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + # Add text labels
  labs(title = "Distribution of Default (0: No, 1: Yes)", x = "Default")

# Box plots for each numeric variable against the "Default" variable
df_subset1 <- df[, c(1:9,17)]
plots <- lapply(names(df_subset1)[-ncol(df_subset1)], function(var) {
  ggplot(df_subset1, aes_string(x = "Default", y = var, fill = "Default")) +
    geom_boxplot() +
    labs(title = paste("Box Plot of", var, "vs. Default")) +
    guides(fill = FALSE)
})
# Arrange the plots in a grid
grid.arrange(grobs = plots, ncol = 3) 

# Correlation matrix for numeric variables
df_subset2 <- df[, c(1:9)]
cor_matrix <- cor(df_subset2)  
cors<- cor(df_subset2, use="pairwise")
corrplot(cors, type="upper", col=brewer.pal(n=8, name="RdYlBu"))

################################################################################
# Undersampling to balance the dataset
df <- df %>%
  group_by(Default) %>%
  slice_sample(n = 29653, replace = FALSE)

################################################################################

# Numeric Encoding
char_cols <- sapply(df, is.character)
for (col in names(df)[char_cols]) {
  df[[paste0(col, "_factor")]] <- factor(df[[col]], levels = unique(df[[col]]))
  df[[paste0(col, "_integer")]] <- as.integer(df[[paste0(col, "_factor")]])
}
df <- df[sapply(df, is.integer) | sapply(df, is.numeric)]

################################################################################

# Split the data into training and testing sets
set.seed(123)
trainIndex <- sample(x = nrow(df), size = nrow(df) * 0.7)
train_data <- df[trainIndex, ]
test_data <- df[-trainIndex, ]

train_x <- model.matrix(Default ~ ., train_data) [, -1]
test_x <- model.matrix(Default ~ ., test_data) [, -1]

train_y <- train_data$Default
test_y <- test_data$Default

################################################################################

# Fit logistic regression model
ols_model <- glm(Default ~ ., family = binomial(link = "logit"), data = train_data)
summary(ols_model)

# Train set predictions
probabilities.train1 <- predict(ols_model,newdata = train_data,type ='response')
predicted.classes.train1 <- as.factor(ifelse(probabilities.train1 >=0.5,"1","0"))

# Model accuracy
train_data$Default <- as.factor(train_data$Default)
confusionMatrix(data = predicted.classes.train1, reference = train_data$Default, positive = "1")

# Test set predictions
probabilities.test1 <- predict(ols_model,newdata = test_data,type ='response')
predicted.classes.test1 <- as.factor(ifelse(probabilities.test1 >=0.5,"1","0"))

# Model accuracy
test_data$Default <- as.factor(test_data$Default)
confusionMatrix(data = predicted.classes.test1, reference = test_data$Default, positive = "1")

################################################################################
# Feature Selection

# Perform stepwise selection
step_model <- step(ols_model, direction = "both")
summary(step_model)

# Train set predictions
probabilities.train2 <- predict(step_model,newdata = train_data,type ='response')
predicted.classes.train2 <- as.factor(ifelse(probabilities.train2 >=0.5,"1","0"))

# Model accuracy
confusionMatrix(data = predicted.classes.train2, reference = train_data$Default, positive = "1")

# Test set predictions
probabilities.test2 <- predict(step_model,newdata = test_data,type ='response')
predicted.classes.test2 <- as.factor(ifelse(probabilities.test2 >=0.5,"1","0"))

# Model accuracy
confusionMatrix(data = predicted.classes.test2, reference = test_data$Default, positive = "1")


################################################################################
# Lasso Regression

# Perform cross-validated Lasso regression
set.seed(123)
cv_lasso <- cv.glmnet(train_x,  train_y, family = "binomial", alpha = 1,nfolds = 10)

# Estimate lambda.min and lambda.1se
lambda_min_lasso <- cv_lasso$lambda.min
lambda_1se_lasso <- cv_lasso$lambda.1se
# Compare lambda values
lambda_min_lasso
lambda_1se_lasso
# Plot the results from cv.glmnet for Lasso
plot(cv_lasso)

# Fit a Lasso regression model using lambda.min
lasso_model_min <- glmnet(train_x, train_y, family = "binomial", alpha = 1, lambda = lambda_min_lasso) # alpha = 1 for Lasso
# Fit a Lasso regression model using lambda.1se
lasso_model_1se <- glmnet(train_x, train_y, family = "binomial", alpha = 1, lambda = lambda_1se_lasso) # alpha = 1 for Lasso

# Get coefficients
coef(lasso_model_min)
coef(lasso_model_1se)

probabilities_train_min <- predict(lasso_model_min, newx = train_x, type = "response")
predicted_classes_train.min <- ifelse(probabilities_train_min >= 0.5, 1, 0)

probabilities_train_1se <- predict(lasso_model_1se, newx = train_x, type = "response")
predicted_classes_train.1se <- ifelse(probabilities_train_1se >= 0.5, 1, 0)

# Convert variables to factors for confusionMatrix
train_y <- as.factor(train_y)
predicted_classes_train.min <- as.factor(predicted_classes_train.min)
predicted_classes_train.1se <- as.factor(predicted_classes_train.1se)

# Model accuracy using confusionMatrix
confusionMatrix(data = predicted_classes_train.min, reference = train_y, positive = "1")
confusionMatrix(data = predicted_classes_train.1se, reference = train_y, positive = "1")

# Test set predictions
probabilities_test_min <- predict(lasso_model_min, newx = test_x, type = "response")
predicted_classes_test.min <- ifelse(probabilities_test_min >= 0.5, 1, 0)

probabilities_test_1se <- predict(lasso_model_1se, newx = test_x, type = "response")
predicted_classes_test.1se <- ifelse(probabilities_test_1se >= 0.5, 1, 0)

# Convert variables to factors for confusionMatrix
test_y <- as.factor(test_y)
predicted_classes_test.min <- as.factor(predicted_classes_test.min)
predicted_classes_test.1se <- as.factor(predicted_classes_test.1se)

# Model accuracy using confusionMatrix
confusionMatrix(data = predicted_classes_test.min, reference = test_y, positive = "1")
confusionMatrix(data = predicted_classes_test.1se, reference = test_y, positive = "1")

################################################################################
# Calculate ROC curve using predicted probabilities
ROC1 <- roc(as.numeric(test_y), as.numeric(probabilities_test_1se))

# Plot the ROC curve
plot(ROC1, col = "blue", ylab = "TPR", xlab = 'FPR', main = "ROC Curve")

# Calculate the area under the ROC curve
auc_value <- ROC1$auc
auc_value