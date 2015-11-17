## Libraries


library(caret)
library(ggplot2)


## Learning curve


LogLoss <- function(obs, pred) {
  obs <- as.numeric(obs) - 1
  eps <- 1e-15
  probs <- pred
  probs[probs > 1 - eps] <- 1 - eps
  probs[probs < eps] <- eps
  -mean(obs * log(probs) + (1 - obs) * log(1 - probs))
}

run_learning_curve <- function(model_formula, data, y, vss = 5000, num_tss = 30, min_tss = 1000){
  library(data.table)
  max_tss <- nrow(data) - vss
  tss_vector <- seq(min_tss, max_tss, length = num_tss)
  data.table::rbindlist( lapply (tss_vector, function(tss){
    vs_idx <- sample(1:nrow(data), vss)
    vs <- data[vs_idx,]
    
    ts_eligible <- setdiff(1:nrow(data), vs_idx)
    ts <- data[sample(ts_eligible, tss), ]

    fit <- glm(model_formula, ts, family = "binomial")
    
    preds.ts <- rep(0, nrow(ts))
    preds.ts[predict(fit, ts, type = "response") > 0.5] <- 1
    preds.vs <- rep(0, nrow(vs))
    preds.vs[predict(fit, vs, type = "response") > 0.5] <- 1
    
    training_error <- LogLoss(ts[, y], preds.ts)
    validation_error <- LogLoss(vs[, y], preds.vs)

    data.frame(tss = tss, 
               error_type = factor(c("training", "validation"), 
                                   levels = c("validation", "training")),
               error = c(training_error, validation_error))
  }) )
}


## Model 1 : Logistic regression


Logit.ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = mnLogLoss)

set.seed(1455)
formula <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations + Months.since.First.Donation
Logit.fit <- train(formula, data = Blood.Train, method = "glm", metric = "logLoss", family = "binomial", trControl = Logit.ctrl)

summary(Logit.fit)
print(Logit.fit)
# CV Logloss = 0.4882664

Logit.preds <- predict(Logit.fit, newdata = Blood.Test, type = "prob")
Submission.Logit1 <- data.frame(X = Blood.Test$X, Made.Donation.in.March.2007 = Logit.preds[, 2])
colnames(Submission.Logit1) <- c("", "Made Donation in March 2007")
write.csv(Submission.Logit1, "Submission1.csv", row.names = FALSE)
# Leaderboard Logloss = 0.4457

learning.curves.model1 <- run_learning_curve(formula, Blood.Train, 6, 114, 30, 10)
ggplot(learning.curves.model1, aes(x = tss, y = error, linetype = error_type)) + geom_line(size = 1, col="blue") + xlab("training set size") + geom_hline(y = 10, linetype = 3)
# The sample size is too small for the learning curves
