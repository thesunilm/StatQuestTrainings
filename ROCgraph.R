####Analysis of obese vs. Not obese people 
####by generating random sample of weight
####Applying Random Forest

library(pROC)
library(randomForest)
set.seed(420)
num.samples <- 100
weight <- sort(rnorm(n=num.samples, mean=172, sd=29))
obese <- ifelse(test=(runif(n=num.samples) < (rank(weight)/100)), yes=1, no=0)
obese
weight
plot(x=weight, y=obese)
glm.fit=glm(obese ~ weight, family=binomial)
lines(weight, glm.fit$fitted.values)

#to plot the graph without extra-padding in R-studio
par(pty = "s")
roc(obese, glm.fit$fitted.values, plot=TRUE)

# Specificity is backward, to make it correct use legacy
roc(obese, glm.fit$fitted.values, plot = TRUE, legacy.axes = TRUE)

#Replacing sensitivity & specificity with other labels
roc(obese, glm.fit$fitted.values, plot = TRUE, legacy.axes = TRUE, 
    xlab="False Positive Percentage", ylab="True Posisitve Percentage")

#adding color & chaning thickness
roc(obese, glm.fit$fitted.values, plot = TRUE, legacy.axes = TRUE,
    xlab="False Positive Percentage", ylab="True Positive Percentage", 
    col="#377eb8", lwd=4)

#Access the specific threhould
#use below code to save the calculations that ROC() function does in a variable

roc.info <- roc(obese, glm.fit$fitted.values, legacy.axes=TRUE)

# make dataframe

roc.df <- data.frame(
  tpp=roc.info$sensitivities*100,
  fpp=(1 - roc.info$specificities)*100,
  thresholds=roc.info$thresholds)

# use head method to check 

head(roc.df)

# use tail function to see last 6 rows

tail(roc.df)

# want to check the tpp is between 60 to 80

roc.df[roc.df$tpp > 60 & roc.df$tpp <80,]

#Going back to custimization of ROC graph
# let's print AUC on graph

roc(obese, glm.fit$fitted.values, plot = TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Positive Percentage",
    col="#377eb8", lwd=4, print.auc=TRUE)



# To draw and calculate partial AUC
# There are useful when you want to focus on the part of the ROC curve
# that only allows for a small number of False positivies. 

roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes= TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Positive Percentage",
    col="#377eb8", lwd=4, print.auc=TRUE, print.auc.x=45, partial.auc=c(100,90),
    auc.polygon = TRUE, auc.polygon.col = "#377eb822")


### Overlapping two ROC curves
### Create a model

rf.model <- randomForest(factor(obese) ~ weight)


#curve with logistic regression

roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes= TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Positive Percentage",
    col="#377eb8", lwd=4, print.auc=TRUE)

# plot curve with random forest & compine

plot.roc(obese, rf.model$votes[,1], percent=TRUE, col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)

# Adding legends to the plot
legend("bottomright", legend=c("Logistic Regression", "Random Forest"),
       col=c("#377eb8", "#4daf4a"), lwd=4)

#reset the parameter pty graphical parameter back to its default value m which
# is short of maximum
par(pty ='m')