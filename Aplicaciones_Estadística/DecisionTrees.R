# Decision trees ----------------------------------------------------------

# DTrees are one of the most fundamentals algorithms in supervised ML.
# DTrees make questions. Based on the answers they either ask more, or classify records.
# DTrees have a root node, i.e., a first question.
# The root node is based on the attribute that best separates training records.
# This attribute is determined by the gini impurity formula, GIF. (simple, but tedious)
# The GIF contrasts the purity of each variable's first attempt in separating the data].
# Finally, the tree branches out to better classify impurities found in the root node.


################## DECISION TREE = MULTIPLE IF-ELSE STATEMENTS #################
# BUT NOT QUITE.

# Decision Trees generate conditional statements automatically
# Simply put, the machine learns the best conditions for your data.




# Libraries ---------------------------------------------------------------

library(caTools) # For splitting data randomly [training and testing]
library(rpart) # For making decision trees
library(rpart.plot) # For plotting decision trees
library(caret) # For modeling and evaluation
library(Boruta) # For variable selection
library(cvms) # For cross validation
library(dplyr) # For data manipulation
head(iris) # Dataset we'll use



# Data splitting ----------------------------------------------------------

# 75% of the data will be used for training the model, 25% for testing it
set.seed(42) # The first ran results will be replicated after running this line
sample_split <- sample.split(Y = iris$Species, SplitRatio = 0.75) # 75% Training, 25% Testing
train_set <- subset(x = iris, sample_split == TRUE) 
test_set <- subset(x = iris, sample_split == FALSE)



# Modeling ----------------------------------------------------------------

# Syntax: y ~ x1 + x2 + ... + xn or y ~ . if all variables are needed
model <- rpart(Species ~ ., data = train_set, method = 'class')  
model # class = classification

# Decision tree's plot
rpart.plot(model) 

# Importance of each variable in the classification
importances <- caret::varImp(model) %>%
  arrange(desc(Overall))


# Variable importances with Boruta
boruta_output <- Boruta(Species ~ ., data = train_set, doTrace = 0)
rough_fix_mod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(rough_fix_mod)
importances <- attStats(rough_fix_mod)
importances <- importances[importances$decision != "Rejected", c("meanImp", "decision")]
importances[order(-importances$meanImp), ]
plot(boruta_output, ces.axis = 0.7, las = 2, xlab = "", main = "Feature importance")
# Green: Relevant (The higher, the more important the variable is)
# Blue: Variables used by Boruta to determine the importance (discard them)
# Red: Non-relevant




# Generating predictions --------------------------------------------------

preds <- predict(model, newdata = test_set, type = "class")
preds #This asigns a predictive class for each index in the test_set


# The confusion matrix is one of the most commonly used metrics to evaluate 
# classification models. It compares the real classification with the prediction
caret::confusionMatrix(test_set$Species, preds)


# Confusion matrix as a heatmap

library(cvms)

cm <- caret::confusionMatrix(test_set$Species, preds)
cfm <- as_tibble(cm$table)
plot_confusion_matrix(cfm, target_col = "Reference", prediction_col = "Prediction", counts_col = "n")
