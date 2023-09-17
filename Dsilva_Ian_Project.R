#Loading Libraries
library(dplyr)
library(pROC)
library(tidyverse)
library(ROCR)
library(ggplot2)
library(caret)
library(Hmisc)
library(reshape2)
library(viridis)
library(glmnet)
library(pROC)
library(randomForest)
  
setwd("D:/Boston/CS 555/Project/Dsilva_Ian_Project")

#Loading Dataset
df_diab <- read.csv('diabetes.csv')

####################################### Data Cleaning #######################################
# Check for missing values
any(missing(df_diab))
any(is.na(df_diab))
sum(is.na(df_diab))


####################################### Exploratory Data Analysis #######################################
# Describing Dataset
head(df_diab)
summary(df_diab)
describe(df_diab)

# histogram for attribute
ggplot(gather(df_diab[, c(1,2,3,4,5,6,8)], variable, value), aes(x = value)) +
  geom_histogram(binwidth = 5, fill = "aquamarine4", color = "darkgreen") +
  facet_wrap(~variable, scales = "free") +
  labs(x = "Value", y = "Count", title = "Distrbution of values for each attribute")

# pie chart of the distribution of 0's and 1's in the Outcome variable
ggplot(df_diab, aes(x = "", fill = factor(df_diab$Outcome))) +
  geom_bar(width = 1, stat = "count") +
  scale_fill_manual(values = c("#576CBC", "#E21818"), name = "Outcome", labels = c("No Diabetes", "Diabetes")) +
  coord_polar(theta = "y") +
  labs(fill = "Outcome", title = "Outcome Distribution")

df_corr <- df_diab

df_corr$Outcome <- NULL

# Computing correlation matrix
cor_matrix <- cor(df_corr)

# Converting correlation matrix to long format
cor_df <- melt(cor_matrix)

# Plotting correlation matrix
ggplot(cor_df, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 4, color = "white") +
  scale_fill_viridis_c(option = "viridis", na.value = "white") +
  labs(title = "Correlation Matrix", x = "", y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, color = 'black'),
        axis.text.y = element_text(size = 10, color = 'black'))

####################################### Box Plot #######################################
# Reshaping the data into long format
df_long <- tidyr::gather(df_diab, key = "Feature", value = "Value", -Pregnancies)

# Creating a boxplot of all features
ggplot(df_long, aes(x = Feature, y = Value, fill = Feature)) +
  geom_boxplot(color = "black") +
  labs(title = "Boxplot of All Features",
       x = "Feature",
       y = "Value") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(name = "Feature")

######################################### One Sample T test #########################################
# Research Question: Is there sufficient evidence to suggest that the mean Blood Pressure of the population from which the given data set is sampled is significantly higher than a known value of 70 mm Hg, using a one-sample t-test?

# Null Hypothesis (H0): The mean Blood Pressure of the population is equal to 70 mm Hg.
# Alternate Hypothesis (H1): The mean Blood Pressure of the population is greater than 70 mm Hg.

#Alpha = 0.05
abs(qt(p = 0.05, df = 767, lower.tail = FALSE))

t.test(df_diab$BloodPressure, mu = 70, alternative = 'two.sided', conf.level = 0.90)

######################################### One Sample T test #########################################
# Research Question: Is there a significant difference in the average blood glucose levels of pregnant women with diabetes, compared to the general population average of 110 mg/dL?

# Null Hypothesis (H0): There is no significant difference in the average blood glucose levels of pregnant women with diabetes compared to the general population average.
# Alternate Hypothesis (H1): There is a significant difference in the average blood glucose levels of pregnant women with diabetes compared to the general population average.

#Alpha = 0.05
abs(qt(p = 0.05, df = 767, lower.tail = FALSE))

t.test(df_diab$Glucose, mu = 110, alternative = 'two.sided', conf.level = 0.90)

######################################### One Way Anova Hypothesis Testing #########################################
# Perform a one-way ANOVA in R to test the null and alternative hypotheses for the relationship between 
# the presence of diabetes (Outcome) and the diabetes pedigree function (DiabetesPedigreeFunction) 
# Use a significance level of α=0.05. Summarize the results using the 5-step procedure. 
# If the results of the overall model are significant,perform the appropriate pairwise comparisons using 

# Step 1: Set-up the hypothesis and select the alpha level.
# Null Hypothesis (H0): There is no significant difference in the mean diabetes pedigree function among different outcome groups.
# Alternate Hypothesis (H1): There is a significant difference in the mean diabetes pedigree function among different outcome groups.

# Step 2: Select the appropriate test statistic.
#  We will select F-test (n-1-1) = 768-1-1 = 766

# Step 3: State the decision rule.
# If p value is less than the significance level; we will reject null hypothesis else we will fail to
# reject null hypothesis.
qf(0.95,1,766)

# Step4: Perform One-Way ANOVA
preg_res <- aov(DiabetesPedigreeFunction ~  Outcome, data = df_diab)
preg_res


summary(preg_res)

# Step 5: Interpret the result.
# So, as the model is significant; we will reject the null hypothesis as p value is less than significance level (0.05)

######################################### Multivariable Logistic Regression Hypothesis Testing #########################################
# Perform multiple logistic regression predicting the relationship between several independent variables (Pregnancies, Glucose, 
# BloodPressure, SkinThickness, Insulin, BMI, DiabetesPedigreeFunction, Age) and the binary outcome of diabetes (Outcome). 
# Formally test (at the alpha=0.05 level).Summarize the results using the 5-step procedure.Lastly, what is the c-statistic for this model?

# Step 1: Formulate Hypotheses
# Null Hypothesis (H0): There is no significant relationship between the independent variables (Glucose, BloodPressure, SkinThickness, Insulin, BMI, DiabetesPedigreeFunction, Age) and the binary outcome (Outcome) of diabetes.
# Alternative Hypothesis (H1): There is a significant relationship between the independent variables and the binary outcome of diabetes.

# Step 2: Assumption of ANOVA
# If the p value is less than the significance level; we will fail to reject null hypothesis or else, 
# we will reject null hypothesis. We will select F-test (n-1-1) = 768-1-1 = 766

# Step 3: Perform multivariable logistic regression
# multivariable logistic regression
multi_log <- glm(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age,data = df_diab, family = binomial(link = "logit"))
multi_log 
summary(multi_log)
anova(multi_log)

# Step 4: Interpret the result.
# So, as the model is significant; we fail to reject the null hypothesis as p value is less than
# significance level (0.05)

# Step 5:
exp(cbind(coef(multi_log), confint.default(multi_log)))

# Calculate the c-statistic (area under ROC curve)
multi_predictions <- predict(multi_log, type = 'response')
multi_roc <- roc(df_diab$Outcome, multi_predictions)

plot(multi_roc , main = "ROC Curve", print.thres = TRUE, legacy.axes = TRUE)
cat("The area under the curve is:", multi_roc$auc, "\n")

######################################### Logistic Regression #########################################
# Converting the columns to categorical or factor
df_diab$Outcome <- as.factor(df_diab$Outcome)

# Splitting the dataset into training and testing sets (80:20 split)
# Setting seed
set.seed(123)  
train_indices <- createDataPartition(df_diab$Outcome, p = 0.8, list = FALSE)
train_data <- df_diab[train_indices, ]
test_data <- df_diab[-train_indices, ]

# Fitting logistic regression model
log_reg <- glm(Outcome ~ ., data = train_data, family = "binomial")
log_reg

# Making predictions on test data
predictions <- predict(log_reg, newdata = test_data, type = "response")

# Convert predicted probabilities to binary outcomes (0 or 1)
predicted_outcomes <- ifelse(predictions > 0.5, 1, 0)

# Setting levels of predicted outcomes to match levels of test data outcomes
predicted_outcomes <- factor(predicted_outcomes, levels = levels(test_data$Outcome))

# Calculating the confusion matrix
confusion <- confusionMatrix(predicted_outcomes, test_data$Outcome)

# Extracting the confusion matrix values
confusion$table

# Extracting TP, FP, FN, TN from confusion matrix
tp <- confusion$table[2,2]
fp <- confusion$table[1,2]
fn <- confusion$table[2,1]
tn <- confusion$table[1,1]

# Calculating accuracy
accuracy <- (tp + tn) / sum(confusion$table)

# Calculating ROC (Receiver Operating Characteristic) curve
roc <- roc(test_data$Outcome, predictions)

# beta value for F-score
beta <- 1

# Calculate precision and recall
precision <- tp / (tp + fp)
recall <- tp / (tp + fn)

# Calculate F-score
f_score <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)


# Print results
cat("Confusion Matrix:\n", confusion$table, "\n")
cat("TP:", tp, "\n")
cat("FP:", fp, "\n")
cat("FN:", fn, "\n")
cat("TN:", tn, "\n")
cat("Accuracy:", accuracy, "\n")
cat("AUC-ROC:", auc(roc), "\n")
cat("F-score:", f_score, "\n")

plot(roc, main = "ROC Curve", print.thres = TRUE, legacy.axes = TRUE)

summary(log_reg)
######################################### Random Forest #########################################
# Convert the Outcome column to factor
df_diab$Outcome <- as.factor(df_diab$Outcome)

# Split the dataset into training and testing sets (80:20 split)
# Set seed for reproducibility
set.seed(123)  
train_indices <- createDataPartition(df_diab$Outcome, p = 0.8, list = FALSE)
train_data <- df_diab[train_indices, ]
test_data <- df_diab[-train_indices, ]

# Prepare predictor variables and target variable
predictors <- train_data[, -which(names(train_data) == "Outcome")]
target <- train_data$Outcome

# Train the random forest model
rf_model <- randomForest(x = predictors, y = target, ntree = 500, mtry = sqrt(ncol(predictors)))
rf_model

# Make predictions on the test data
test_predictors <- test_data[, -which(names(test_data) == "Outcome")]
test_predictions <- predict(rf_model, newdata = test_predictors)

# Convert predicted outcomes to factors
test_predictions <- as.factor(test_predictions)

# Calculate the confusion matrix
confusion <- confusionMatrix(test_predictions, test_data$Outcome)
confusion$table
# Extract TP, FP, FN, TN from confusion matrix
tp <- confusion$table[2,2]
fp <- confusion$table[1,2]
fn <- confusion$table[2,1]
tn <- confusion$table[1,1]

# Calculate accuracy
accuracy <- (tp + tn) / sum(confusion$table)

# Calculate AUC-ROC
roc <- roc(test_data$Outcome, as.numeric(test_predictions))
auc_roc <- auc(roc)

# beta value for F-score
beta <- 1

# Calculate precision and recall
precision <- tp / (tp + fp)
recall <- tp / (tp + fn)

# Calculate F-score
f_score <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)

# Print results
cat("TP:", tp, "\n")
cat("FP:", fp, "\n")
cat("FN:", fn, "\n")
cat("TN:", tn, "\n")
cat("Accuracy:", accuracy, "\n")
cat("AUC-ROC:", auc_roc, "\n")
cat("F-score:", f_score, "\n")

plot(roc, main = "ROC Curve", print.thres = TRUE, legacy.axes = TRUE)





