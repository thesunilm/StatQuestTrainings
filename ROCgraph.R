library(pROC)
library(randomForest)
set.seed(420)
num.samples <- 100
weight <- sort(rnorm(n=num.samples, mean=172, sd=29))
obese <- ifelse(test=(runif(n=num.samples) < (rank(weight)/100)), yes=1, no=0)
obese
plot(x=weight, y=obese)