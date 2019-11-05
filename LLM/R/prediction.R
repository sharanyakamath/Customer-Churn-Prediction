# install.packages('gridExtra')
# install.packages('party')
# install.packages('pROC')
library(party)
library(pROC)


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
	if(as.numeric(prob) < 0.5) {
		data[i,"predictedChurn"] = "No"
	}
	else {
		data[i,"predictedChurn"] = "Yes"
	}
	# data[i,] <- row
}

write.csv(data, file="predictedChurn.csv")

tp <- 0
fn <- 0
fp <- 0
tn <- 0

for(i in 1:nrow(data)){
	if(data[i,"predictedChurn"] == "Yes" && data[i,"Churn"] == "Yes") {
		tp <- tp + 1
	}
	if(data[i,"predictedChurn"] == "Yes" && data[i,"Churn"] == "No") {
		fn <- fn + 1
	}
	if(data[i,"predictedChurn"] == "No" && data[i,"Churn"] == "Yes") {
		fp <- fp + 1
	}
	if(data[i,"predictedChurn"] == "No" && data[i,"Churn"] == "No") {
		tn <- tn + 1
	}
}

accuracy = (tp+tn)/(fp+fn+tp+tn)

print(tp)
print(fn)
print(fp)
print(tn)
print(accuracy)

roc <- calculate_roc(data, 1, 2, n = 100)
plot_roc(roc, 0.5, 1, 2)