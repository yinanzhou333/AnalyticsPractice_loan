# WENZHUO ZHANG & YINAN ZHOU, Final Assignment


library(glmnet)
library(Metrics)
library(psych)
library(ggplot2)
library(GGally)
library(dplyr)
library(caret)
library(corrplot)
library(gridExtra)
library(rlang)
library(car)
library(Hmisc)

# Load data
Loan <- read.csv("Loan_default.csv")
attach(Loan)

#########################
# EDA
#########################
# quick view about the data
headTail(Loan)

# Get a summary of the data
summary(Loan)

# View the structure of the data
str(Loan)
  
# List of categorical variables
cat_vars <- c('Education', 'EmploymentType', 'MaritalStatus', 
                                   'HasMortgage', 'HasDependents', 'LoanPurpose',
                                   'NumCreditLines', 'HasCoSigner', 'Default')

# List of numerical variables
num_vars <- c('Age', 'Income', 'LoanAmount', 'CreditScore', 
              'MonthsEmployed', 'InterestRate', 'LoanTerm', 'DTIRatio')

# Convert string columns to factors
Loan$Education <- as.factor(Loan$Education)
Loan$EmploymentType <- as.factor(Loan$EmploymentType)
Loan$MaritalStatus <- as.factor(Loan$MaritalStatus)
Loan$HasMortgage <- as.factor(Loan$HasMortgage)
Loan$HasDependents <- as.factor(Loan$HasDependents)
Loan$LoanPurpose <- as.factor(Loan$LoanPurpose)
Loan$HasCoSigner <- as.factor(Loan$HasCoSigner)
Loan$NumCreditLines <- as.factor(Loan$NumCreditLines)
Loan$Default <- as.factor(Loan$Default)
str(Loan)

# bar charts
Edu <- Loan %>% group_by(Education) %>% count()
Emp <- Loan %>% group_by(EmploymentType) %>% count()
Mar <- Loan %>% group_by(MaritalStatus) %>% count()
HM <- Loan %>% group_by(HasMortgage) %>% count()
HD <- Loan %>% group_by(HasDependents) %>% count()
LP <- Loan %>% group_by(LoanPurpose) %>% count()
NCL <- Loan %>% group_by(NumCreditLines) %>% count()
HC <- Loan %>% group_by(HasCoSigner) %>% count()
D <- Loan %>% group_by(Default) %>% count()

Edu1 <- ggplot(Edu, aes(x = Education, y = n, fill = Education)) +
  geom_bar(stat = 'identity') +
  labs(title = "Distribution of Education", x = "Education", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

Emp1 <- ggplot(Emp, aes(x = EmploymentType, y = n, fill = EmploymentType)) +
  geom_bar(stat = 'identity') +
  labs(title = "Distribution of EmploymentType", x = "Employment Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

Mar1 <- ggplot(Mar, aes(x = MaritalStatus, y = n, fill = MaritalStatus)) +
  geom_bar(stat = 'identity') +
  labs(title = "Distribution of MaritalStatus", x = "MaritalStatus", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

HM1 <- ggplot(HM, aes(x = HasMortgage, y = n, fill = HasMortgage)) +
  geom_bar(stat = 'identity') +
  labs(title = "Distribution of HasMortgage", x = "Mortgage", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

HD1 <- ggplot(HD, aes(x = HasDependents, y = n, fill = HasDependents)) +
  geom_bar(stat = 'identity') +
  labs(title = "Distribution of HasDependents", x = "Dependents", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

LP1 <- ggplot(LP, aes(x = LoanPurpose, y = n, fill = LoanPurpose)) +
  geom_bar(stat = 'identity') +
  labs(title = "Distribution of LoanPurpose", x = "Loan Purpose", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

NCL1 <- ggplot(NCL, aes(x = NumCreditLines, y = n, fill = NumCreditLines)) +
  geom_bar(stat = 'identity') +
  labs(title = "Distribution of NumCreditLines", x = "NumCreditLines", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

HC1 <- ggplot(HC, aes(x = HasCoSigner, y = n, fill = HasCoSigner)) +
  geom_bar(stat = 'identity') +
  labs(title = "Distribution of HasCoSigner", x = "Cosigner", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

D1 <- ggplot(D, aes(x = Default, y = n, fill = Default)) +
  geom_bar(stat = 'identity') +
  labs(title = "Distribution of Default", x = "Default", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

grid.arrange(Edu1, Emp1, Mar1, HM1, HD1, LP1, NCL1, HC1, D1, ncol = 3)

# bar charts separate by default and non-default
Edu2 <- ggplot(Loan, aes(x = Education, fill = as.factor(Default))) +
  geom_bar(position = "dodge", color = "black") + 
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), labels = c("Non-default", "Default")) +  
  labs(title = "Counts of Education by Default Status",
       x = "Education",
       y = "Count",
       fill = "Default Status") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

Emp2 <- ggplot(Loan, aes(x = EmploymentType, fill = as.factor(Default))) +
  geom_bar(position = "dodge", color = "black") + 
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), labels = c("Non-default", "Default")) +  
  labs(title = "Counts of EmploymentType by Default Status",
       x = "Employment Type",
       y = "Count",
       fill = "Default Status") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

Mar2 <- ggplot(Loan, aes(x = MaritalStatus, fill = as.factor(Default))) +
  geom_bar(position = "dodge", color = "black") + 
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), labels = c("Non-default", "Default")) +  
  labs(title = "Counts of MaritalStatus by Default Status",
       x = "Marital Status",
       y = "Count",
       fill = "Default Status") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

HM2 <- ggplot(Loan, aes(x = HasMortgage, fill = as.factor(Default))) +
  geom_bar(position = "dodge", color = "black") + 
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), labels = c("Non-default", "Default")) +  
  labs(title = "Counts of HasMortgage by Default Status",
       x = "Has Mortgage",
       y = "Count",
       fill = "Default Status") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

HD2 <- ggplot(Loan, aes(x = HasDependents, fill = as.factor(Default))) +
  geom_bar(position = "dodge", color = "black") + 
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), labels = c("Non-default", "Default")) +  
  labs(title = "Counts of HasDependents by Default Status",
       x = "Has Dependents",
       y = "Count",
       fill = "Default Status") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

LP2 <- ggplot(Loan, aes(x = LoanPurpose, fill = as.factor(Default))) +
  geom_bar(position = "dodge", color = "black") + 
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), labels = c("Non-default", "Default")) +  
  labs(title = "Counts of LoanPurpose by Default Status",
       x = "Loan Purpose",
       y = "Count",
       fill = "Default Status") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

NCL2 <- ggplot(Loan, aes(x = NumCreditLines, fill = as.factor(Default))) +
  geom_bar(position = "dodge", color = "black") + 
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), labels = c("Non-default", "Default")) +  
  labs(title = "Counts of NumCreditLines by Default Status",
       x = "NumCreditLines",
       y = "Count",
       fill = "Default Status") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

HC2 <- ggplot(Loan, aes(x = HasCoSigner, fill = as.factor(Default))) +
  geom_bar(position = "dodge", color = "black") + 
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), labels = c("Non-default", "Default")) +  
  labs(title = "Counts of HasCoSigner by Default Status",
       x = "Has Co-Signer",
       y = "Count",
       fill = "Default Status") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

D2 <- ggplot(Loan, aes(x = Default, fill = as.factor(Default))) +
  geom_bar(position = "dodge", color = "black") + 
  scale_fill_manual(values = c("#56B4E9", "#E69F00"), labels = c("Non-default", "Default")) +  
  labs(title = "Counts of Default by Default Status",
       x = "Default",
       y = "Count",
       fill = "Default Status") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# put all the bar charts together
grid.arrange(Edu2, Emp2, Mar2, HM2, HD2, LP2, NCL2, HC2, D2, ncol = 3)

#####################################
# Chi-Square Testing: are Defaults related to the Education level?
#####################################
Loan %>% group_by(Education, Default) %>% summarise(count = n())

# State the Hypotheses
# H0 : Default (Yes or No) is independent of the Education level
# H1 : Default (Yes or No) is dependent of the Education level

# set alpha
alpha <- 0.05

# create matrix of observed frequencies
r1 <- c(56577, 7789)
r2 <- c(55673, 8230)
r3 <- c(56633, 6908)
r4 <- c(56811, 6726)

observed <- matrix(c(r1, r2, r3, r4), nrow = 4, byrow = TRUE)
observed

# named the rows and columns
rownames(observed) = c("Bachelor's", "High School", "Master's", "PhD")
colnames(observed) = c("Non-Default", "Default")

# Performing Chi-Square test
chi_square_test <- chisq.test(observed)
chi_square_test

# Compare the p value to alpha and make conclusion
ifelse(chi_square_test$p.value > alpha, "Fail to reject the Null Hypothesis",
       "Reject the Null Hypothesis")

#######################################
# Anova Testing
#######################################
# list of all numerical variables
numerical_variables <- Loan[c('Age', 'Income', 'LoanAmount', 'CreditScore', 
                      'MonthsEmployed', 'InterestRate', 'LoanTerm', 'DTIRatio')]

# histogram of all numerical variables to see the distribution
hist_plots <- lapply(num_vars, function(var) {
  ggplot(numerical_variables, aes_string(x = var)) +
    geom_histogram(bins = 30, fill = "red", color = "black") +
    labs(title = paste("Histogram of", var)) +
    guides(fill = "none")
})

grid.arrange(grobs = hist_plots, ncol = 3)

# box plot of all numerical variables to see if there exists outliers or data skewness
box_plots <- lapply(num_vars, function(var) {
  ggplot(numerical_variables, aes_string(x = var)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", var)) +
    guides(fill = "none")
})

grid.arrange(grobs = box_plots, ncol = 3)

# want to find is there any difference in income in the four education levels
# H0: There is no difference in income in the four education levels
# H1: There is a difference in income in the four education levels

# Box plot to visualize income across education levels
boxplot(Income ~ Education, data = Loan, 
        main = "Income across Education Levels", xlab = "Education", ylab = "Income")

# Histograms for each education level
ggplot(Loan, aes(x = Income)) +
  geom_histogram(bins = 30, fill = "gray", color = "black") +
  facet_wrap(~ Education, scales = "free") +
  ggtitle("Income Distribution across Education Levels")

# alpha = 0.05
# Kruskal-Wallis Test
kruskal_test <- kruskal.test(Income ~ Education, data = Loan)
print(kruskal_test)

# Correlation matrix 
Loan1 <- Loan[c('Age', 'Income', 'LoanAmount', 'CreditScore', 
                     'MonthsEmployed', 'NumCreditLines', 'InterestRate', 
                     'LoanTerm', 'DTIRatio', 'Default')]
str(Loan1)
Loan1$NumCreditLines = as.numeric(Loan1$NumCreditLines)
Loan1$Default = as.numeric(Loan1$Default)
Loan1$Age = as.numeric(Loan1$Age)
Loan1$Income = as.numeric(Loan1$Income)
Loan1$LoanAmount = as.numeric(Loan1$LoanAmount)
Loan1$CreditScore = as.numeric(Loan1$CreditScore)
Loan1$MonthsEmployed = as.numeric(Loan1$MonthsEmployed)
Loan1$LoanTerm = as.numeric(Loan1$LoanTerm)

sapply(Loan1, class)

matrix_data <- as.matrix(Loan1)
class(matrix_data)

corrplot(cor(matrix_data), type = 'upper')
cor(matrix_data)

# scatter plot matrix
ggpairs(matrix_data)

######################################################################################################

# Loan Default (Binary Classification)

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


# df[, 10:17] <- lapply(df[, 10:17], factor)  # Converts columns 10 to 17 to factor
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
set.seed(123)
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

# Train set predictions
probabilities_train_1se <- predict(lasso_model_1se, newx = train_x, type = "response")
predicted_classes_train.1se <- ifelse(probabilities_train_1se >= 0.5, 1, 0)

# Convert variables to factors for confusionMatrix
train_y <- as.factor(train_y)
predicted_classes_train.1se <- as.factor(predicted_classes_train.1se)

# Model accuracy using confusionMatrix
confusionMatrix(data = predicted_classes_train.1se, reference = train_y, positive = "1")

# Test set predictions
probabilities_test_1se <- predict(lasso_model_1se, newx = test_x, type = "response")
predicted_classes_test.1se <- ifelse(probabilities_test_1se >= 0.5, 1, 0)

# Convert variables to factors for confusionMatrix
test_y <- as.factor(test_y)
predicted_classes_test.1se <- as.factor(predicted_classes_test.1se)

# Model accuracy using confusionMatrix
confusionMatrix(data = predicted_classes_test.1se, reference = test_y, positive = "1")

################################################################################
# Calculate ROC curve using predicted probabilities
ROC1 <- roc(as.numeric(test_y), as.numeric(probabilities_test_1se))

# Plot the ROC curve
plot(ROC1, col = "blue", ylab = "TPR", xlab = 'FPR', main = "ROC Curve")

# Calculate the area under the ROC curve
auc_value <- ROC1$auc
auc_value