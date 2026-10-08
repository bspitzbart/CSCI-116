################################################################### 
# H03b
# Brad Spitzbart
# CSCI-E-116 10/07/2026
# modified from:
# AR01: Answer key to Assignment 1
# By William Yu, Harvard Extension
# 2/14/2023
################################################################### 
setwd("C:/Users/brs416/Desktop/CSCI-E-116_bigdata/Week3/Data_and_Script_3")
library(readxl) 
library(leaps)
library(MASS)
library(corrplot)

###########
## Part B
###########

new2 = read.csv("C:/Users/brs416/Desktop/CSCI-E-116_bigdata/Week2/Data and Script 2/Health in America Covid19 Variation/new2.csv") 

corr_new2 = new2 %>% select(where(is.numeric))
head(corr_new2)
cor_matrix <- cor(corr_new2, use = "complete.obs")
corrplot(cor_matrix, 
         method = "color",       # Use colored squares to save space
         type = "upper",         # Show only the upper triangle
         order = "hclust",       # Cluster similar variables together
         tl.pos = "n",           # Hide text labels if they are too crowded (optional)
         # tl.cex = 0.3,         # Use tiny text size if labels are kept
         # tl.col = "black",     # Label color
         mar = c(0, 0, 1, 0))    # Margins

# from Gemini we can find variables with high correlation
# 1. Clear out the lower triangle and diagonal to prevent duplicate pairs (e.g., Var1-Var2 and Var2-Var1)
cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA

# 2. Flatten the matrix into a 3-column table
cor_flat <- as.data.frame(as.table(cor_matrix))

# 3. Filter for absolute correlations strictly greater than 0.9
high_cor <- subset(cor_flat, abs(Freq) > 0.9)

# 4. Clean up column names
colnames(high_cor) <- c("Variable_1", "Variable_2", "Correlation")

# 5. Sort by strongest correlation descending
high_cor <- high_cor[order(-abs(high_cor$Correlation)), ]

# View the ranked predictors
print(high_dv_corr, row.names = FALSE)

# find variables that are highly correlated with deathp520
# 2. Extract correlation values for your specific dependent variable
# Replace "your_y_variable_name" with the actual column name of your DV
dv_corr <- cor_matrix[, "deathp520"]

# 3. Convert to a data frame and remove the self-correlation (DV vs DV)
dv_df <- data.frame(Variable = names(dv_corr), Correlation = dv_corr)
dv_df <- subset(dv_df, Variable != "deathp520")

# 4. Filter for high correlations (e.g., absolute value > 0.5)
# Adjust the 0.5 threshold up or down depending on how strict you want to be
high_dv_corr <- subset(dv_df, abs(Correlation) > 0.5)

# 5. Sort from strongest relationship to weakest
high_dv_corr <- high_dv_corr[order(-abs(high_dv_corr$Correlation)), ]

# View the ranked predictors
print(high_dv_corr, row.names = FALSE)

# random forest
To build a Random Forest model in R with 105 variables, you should use the randomForest package or the faster ranger package (which is highly optimized for large datasets with many variables).
Below is the optimized script using ranger. It automatically splits your data, trains the model, and extracts variable importance so you can see if the highly correlated variables you found earlier are actually useful predictors.
## R Code for Random Forest

# 1. Install and load required packages
install.packages(c("ranger", "caret"))
library(ranger)
library(caret)
new2 <- new2[!is.na(new2$deathp520), ]
# Set seed for reproducibility
set.seed(42)

# 2. Split data into Training (80%) and Testing (20%)
# Replace "your_y_variable_name" with your actual dependent variable column name
train_index <- createDataPartition(new2$deathp520, p = 0.8, list = FALSE)
train_data  <- new2[train_index, ]
test_data   <- new2[-train_index, ]

# 3. Train the Random Forest model
# 'importance = "permutation"' calculates how much each variable drops model accuracy
rf_model <- ranger(deathp520 ~ ., 
                   data = train_data, 
                   num.trees = 500,          # Standard number of trees
                   importance = "permutation", 
                   verbose = TRUE)

# Print model summary (shows OOB error or R-squared)
print(rf_model)

# 4. Make predictions on the test set
predictions <- predict(rf_model, data = test_data)$predictions

# 5. Evaluate Model Performance
if(is.factor(new2$deathp520)) {
  # For Classification (Categorical target)
  confusionMatrix(as.factor(predictions), as.factor(test_data$deathp520))
} else {
  # For Regression (Continuous target)
  postResample(pred = predictions, obs = test_data$deathp520)
}

# 6. Extract and view top Variable Importance
var_importance <- importance(rf_model)
importance_df  <- data.frame(Variable = names(var_importance), Importance = var_importance)
importance_df  <- importance_df[order(-importance_df$Importance), ]

# Show top 10 most important variables
head(importance_df, 10)

# original model fits
model01 = lm(deathp520 ~ a85a + a7584 + a6574 + a5564 + a2034 + pdensity + pop + aindian + black + latino + asian + sparent + 
               mincome + poverty + chci + lcp + ur + disable + hi_pub  + demv + hosp + vcrime +
               commute_p + wfh + computer + p_nursehome + p_liquor + prematured + drinking  + lowbirthw, data=new2)
summary(model01) 

model05 = lm(deathp520 ~ a85a + a7584 + a6574 + a5564 + a2034 + pdensity + pop + aindian + black + latino + asian + sparent + 
               mincome + poverty + chci + lcp + ur + disable + hi_pub  + demv + hosp + vcrime +
               commute_p + wfh + computer + p_nursehome + p_liquor + prematured + drinking  + lowbirthw + statename, data=new2)
summary(model05) 

# next step is to train and test additional models
