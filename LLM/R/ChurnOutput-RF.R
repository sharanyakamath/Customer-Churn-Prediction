# install.packages("LLM")
# install.packages("mlbench")

library(LLM)

# # Test
# if (requireNamespace("mlbench", quietly = TRUE)) {
#   library("mlbench")
# }

data <- read.csv("preprocessed.csv")

## Split in training and test (3/4 - 1/4)
set.seed(123)
n <- nrow(data)
idtrain <- sample(n, trunc(0.75*n))

ChurnTrain <-data[idtrain,]
ChurnTest <-data[-idtrain,]

#number of rows
nrow(ChurnTest)
nrow(ChurnTrain)

# print(ChurnTrain[,20])
# write.csv(ChurnTrain[,"Churn"],"try.csv")
# Create the LLM
Churn.llm <- llm(X = ChurnTrain[,-c(20)],Y = ChurnTrain$Churn, threshold_pruning = 0.01, nbr_obs_leaf = 100)
# Churn.llm.cv <- llm.cv(X = ChurnTrain[,-c(20)],Y = ChurnTrain$Churn, cv=4, threshold_pruning = 0.25, nbr_obs_leaf = 100)

## Use the model on the test dataset to make a prediction
ChurnPrediction <- predict.llm(object = Churn.llm, X = ChurnTest[,-c(20)])

# ## Optionally add the dependent to calculate performance statistics such as AUC
ChurnPrediction <- cbind(ChurnPrediction, "Churn" = ChurnTest[,"Churn"])

Churn.Viz <- table.llm.html(object = Churn.llm, headertext = "LLM model")
write(Churn.Viz, "Churn.html")
# print(ChurnPrediction)

write.csv(ChurnPrediction,"ChurnPrediction.csv")

write.csv(Churn.llm$foldpred, file="output/foldpred.csv")
write.csv(Churn.llm$pred, file="output/pred.csv")
write.csv(Churn.llm$foldclass, file="output/foldclass.csv")
write.csv(Churn.llm$class, file="output/class.csv")
write.csv(Churn.llm$conf, file="output/conf.csv")

