install.packages("LLM")
install.packages("mlbench")

library(LLM)

# Test
if (requireNamespace("mlbench", quietly = TRUE)) {
  library("mlbench")
}

# ## Split in training and test (2/3 - 1/3)
# idtrain <- c(sample(1:7043,5283))
# ChurnTrain <-data[idtrain,]
# ChurnTest <-data[-idtrain,]

# #number of rows
# nrow(ChurnTest)
# nrow(ChurnTrain)

# ## Create the LLM
# Churn.llm <- llm(X = ChurnTrain[,-c(21)],Y = ChurnTrain$Churn, threshold_pruning = 0.25,nbr_obs_leaf = 100)

# ## Use the model on the test dataset to make a prediction
# PimaPrediction <- predict.llm(object = Pima.llm, X = Pimatest[,-c(9)])
# ## Optionally add the dependent to calculate performance statistics such as AUC
# PimaPrediction <- cbind(PimaPrediction, "diabetes" = Pimatest[,"diabetes"])

# print(PimaPrediction)
