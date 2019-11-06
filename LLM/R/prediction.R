# install.packages('gridExtra')
# install.packages('party')
# install.packages('pROC')
# install.packages("precrec")
# install.packages('autoplot')
# install.packages("ggfortify")

library(party)
library(pROC)
library(zoo)
library(ggplot2)


AUC <- function (actuals, predictedScores){
    fitted <- data.frame (Actuals=actuals, PredictedScores=predictedScores)
    colnames(fitted) <- c('Actuals','PredictedScores')
    ones <- fitted[fitted$Actuals==1, ] # Subset ones
    zeros <- fitted[fitted$Actuals==0, ] # Subsetzeros
    totalPairs <- nrow (ones) * nrow (zeros) # calculate total number of pairs to check
    conc <- sum (c(vapply(ones$PredictedScores, function(x) {((x > zeros$PredictedScores))}, FUN.VALUE=logical(nrow(zeros)))), na.rm=T)
    disc <- sum(c(vapply(ones$PredictedScores, function(x) {((x < zeros$PredictedScores))}, FUN.VALUE = logical(nrow(zeros)))), na.rm = T)
    concordance <- conc/totalPairs
    discordance <- disc/totalPairs
    tiesPercent <- (1-concordance-discordance)
    AUC = concordance + 0.5*tiesPercent
    Gini = 2*AUC - 1
    return(list("Concordance"=concordance, "Discordance"=discordance,
                "Tied"=tiesPercent, "Gini or Somers D"=Gini))
}




data <- read.csv("ChurnPrediction.csv")


data$predictedChurn 

for(i in 1:nrow(data)){
	churn = data[i,"Churn"]
	prob = data[i,"probability"]

  if(churn %in% c("Yes")) {
    data[i,"ChurnBool"] = 1
  }
  if(churn %in% c("No")) {
    data[i,"ChurnBool"] = 0
  }

	if(as.numeric(prob) < 0.5) {
		data[i,"predictedChurn"] = 0
	}
	else {
		data[i,"predictedChurn"] = 1
	}
}

write.csv(data, file="predictedChurn.csv")
write.csv(data[,c("ChurnBool", "probability")], file="validate.csv")
validate_data <- read.csv("validate.csv")

tp <- 0
fn <- 0
fp <- 0
tn <- 0

for(i in 1:nrow(data)){
	# if(data[i,"predictedChurn"] == "Yes" && data[i,"Churn"] == "Yes") {
	# 	tp <- tp + 1
	# }
	# if(data[i,"predictedChurn"] == "Yes" && data[i,"Churn"] == "No") {
	# 	fn <- fn + 1
	# }
	# if(data[i,"predictedChurn"] == "No" && data[i,"Churn"] == "Yes") {
	# 	fp <- fp + 1
	# }
	# if(data[i,"predictedChurn"] == "No" && data[i,"Churn"] == "No") {
	# 	tn <- tn + 1
	# }

  if(data[i,"predictedChurn"] == 1 && data[i,"ChurnBool"] == 1) {
    tp <- tp + 1
  }
  if(data[i,"predictedChurn"] == 1 && data[i,"ChurnBool"] == 0) {
    fn <- fn + 1
  }
  if(data[i,"predictedChurn"] == 0 && data[i,"ChurnBool"] == 1) {
    fp <- fp + 1
  }
  if(data[i,"predictedChurn"] == 0 && data[i,"ChurnBool"] == 0) {
    tn <- tn + 1
  }
}

accuracy = (tp+tn)/(fp+fn+tp+tn)

cat("\n\nTP : ", tp, "\n")
cat("FN : ", fn, "\n")
cat("FP : ", fp, "\n")
cat("TN : ", tn, "\n")
cat("Accuracy : ", accuracy, "\n\n\n")

newdata <- read.csv("predictedChurn.csv")

AUC(newdata$ChurnBool, newdata$predictedChurn)

# roc.plot(newdata$ChurnBool, newdata$predictedChurn, thresholds = NULL)
# auc.roc.plot (validate_data)

#----------------------------------
pROC_obj <- roc(validate_data$ChurnBool,validate_data$probability,
            smoothed = TRUE,
            # arguments for ci
            ci=TRUE, ci.alpha=0.9, stratified=FALSE,
            # arguments for plot
            plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
            print.auc=TRUE, show.thres=TRUE)

sens.ci <- ci.se(pROC_obj)
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")
#------------------------
# library(precrec)
# precrec_obj2 <- evalmod(scores = validate_data$probability, labels = validate_data$ChurnBool, mode="basic")
# autoplot(precrec_obj2)  

#-----------------------
# install.packages("plotROC")
# library(plotROC)
# rocplot <- ggplot(validate_data, aes(m = validate_data$probability, d = validate_data$ChurnBool))+ geom_roc(n.cuts=20,labels=FALSE)
# rocplot + style_roc(theme = theme_grey) + geom_rocci(fill="pink")