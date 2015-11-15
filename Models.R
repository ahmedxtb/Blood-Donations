## Libraries


library(caret)


## Logloss metric function


loglossSummary <- function(data, lev = NULL, model = NULL)  
{
  #print(paste(class(data$pred))) # factor  
  data$pred <- as.numeric(data$pred) - 1 # otherwise I get an error as these are factors  
  data$obs <- as.numeric(data$obs) - 1 # otherwise I get an error as these are factors  
  epsilon <- .000000000000001  
  yhat <- pmin(pmax(data$pred, rep(epsilon)), 1 - rep(epsilon))  
  logloss <- -mean(data$obs * log(yhat) + (1 - data$obs) * log(1 - yhat))  
  names(logloss) <- "LOGLOSS"  
  logloss
}

LogLossSummary <- function (data, lev = NULL, model = NULL) 
{
  probs <- pmax(pmin(as.numeric(data$T), 1 - 1e-15), 1e-15)
  logPreds <- log(probs)        
  log1Preds <- log(1 - probs)
  real <- (as.numeric(data$obs) - 1)
  out <- c(mean(real * logPreds + (1 - real) * log1Preds)) * -1
  names(out) <- c("LogLoss")
  out
}
# Will use caret built-in function mnLogLoss


## Model 1 : Logistic regression


Logit.ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = mnLogLoss)

set.seed(1455)
Logit.fit <- train(Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations + Months.since.First.Donation, data = Blood.Train, method = "glm", metric = "logLoss", family = "binomial", trControl = Logit.ctrl)

summary(Logit.fit)
print(Logit.fit)
# CV Logloss = 0.4882664

Logit.preds <- predict(Logit.fit, newdata = Blood.Test, type = "prob")
Submission.Logit1 <- data.frame(X = Blood.Test$X, Made.Donation.in.March.2007 = Logit.preds[, 2])
colnames(Submission.Logit1) <- c("", "Made Donation in March 2007")
write.csv(Submission.Logit1, "Submission1.csv", row.names = FALSE)
# Leaderboard Logloss = 0.4457
