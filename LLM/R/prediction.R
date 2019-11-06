# install.packages('gridExtra')
# install.packages('party')
# install.packages('pROC')
library(party)
library(pROC)
library(zoo)


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
                "Tied"=tiesPercent, "AUC"=AUC, "Gini or Somers D"=Gini))
}

calculate_roc <- function(df, cost_of_fp, cost_of_fn, n=100) {
  tpr <- function(df, threshold) {
    sum(df$predictedChurn >= threshold & df$Churn == "Yes") / sum(df$Churn == "Yes")
  }
  
  fpr <- function(df, threshold) {
    sum(df$predictedChurn >= threshold & df$Churn == "No") / sum(df$Churn == "No")
  }
  
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
    sum(df$predictedChurn >= threshold & df$Churn == "No") * cost_of_fp + 
      sum(df$predictedChurn < threshold & df$Churn == "Yes") * cost_of_fn
  }
  
  roc <- data.frame(threshold = seq(0,1,by=0.1), tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(df, th, cost_of_fp, cost_of_fn))
  
  print(roc)
  return(roc)
}

plot_roc <- function(roc, threshold, cost_of_fp, cost_of_fn) {
  library(gridExtra)
  library(ggplot2)

  norm_vec <- function(v) (v - min(v))/diff(range(v))
  
  idx_threshold = which.min(abs(roc$threshold-threshold))
  
  col_ramp <- colorRampPalette(c("green","orange","red","black"))(100)
  col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99)+1]
  p_roc <- ggplot(roc, aes(fpr,tpr)) + 
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    coord_fixed() +
    geom_line(aes(threshold,threshold), color=rgb(0,0,1,alpha=0.5)) +
    labs(title = sprintf("ROC")) + xlab("FPR") + ylab("TPR") +
    geom_hline(yintercept=roc[idx_threshold,"tpr"], alpha=0.5, linetype="dashed") +
    geom_vline(xintercept=roc[idx_threshold,"fpr"], alpha=0.5, linetype="dashed")
  
  p_cost <- ggplot(roc, aes(threshold, cost)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    labs(title = sprintf("cost function")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
  
  sub_title <- sprintf("threshold at %.2f - cost of FP = %d, cost of FN = %d", threshold, cost_of_fp, cost_of_fn)

  grid.arrange(p_roc, p_cost, ncol=2, sub=textGrob(sub_title, gp=gpar(cex=1), just="bottom"))
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

print(tp)
print(fn)
print(fp)
print(tn)
print(accuracy)

newdata <- read.csv("predictedChurn.csv")

AUC(newdata$ChurnBool, newdata$predictedChurn)