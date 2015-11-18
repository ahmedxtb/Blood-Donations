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

set.seed(1455)
formula.2 <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + I(Number.of.Donations^2) + Months.since.First.Donation
Logit.fit.2 <- train(formula.2, data = Blood.Train, method = "glm", metric = "logLoss", family = "binomial", trControl = Logit.ctrl)

summary(Logit.fit.2)
print(Logit.fit.2)
# CV Logloss = 0.5075705

set.seed(1455)
formula.3 <- Made.Donation.in.March.2007 ~ I(Months.since.Last.Donation^2) + Number.of.Donations + Months.since.First.Donation
Logit.fit.3 <- train(formula.3, data = Blood.Train, method = "glm", metric = "logLoss", family = "binomial", trControl = Logit.ctrl)

summary(Logit.fit.3)
print(Logit.fit.3)
# CV Logloss = 0.4915196

set.seed(1455)
formula.4 <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations + I(Months.since.First.Donation^2)
Logit.fit.4 <- train(formula.4, data = Blood.Train, method = "glm", metric = "logLoss", family = "binomial", trControl = Logit.ctrl)

summary(Logit.fit.4)
print(Logit.fit.4)
# CV Logloss = 0.4833443

Logit.preds.4 <- predict(Logit.fit.4, newdata = Blood.Test, type = "prob")
Submission.Logit4 <- data.frame(X = Blood.Test$X, Made.Donation.in.March.2007 = Logit.preds.4[, 2])
colnames(Submission.Logit4) <- c("", "Made Donation in March 2007")
write.csv(Submission.Logit4, "Submission2.csv", row.names = FALSE)
# Leaderboard Logloss = 0.4384 (Best)

set.seed(1455)
formula.5 <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations * Months.since.First.Donation + I(Months.since.First.Donation^3)
Logit.fit.5 <- train(formula.5, data = Blood.Train, method = "glm", metric = "logLoss", family = "binomial", trControl = Logit.ctrl)

summary(Logit.fit.5)
print(Logit.fit.5)
# CV Logloss = 0.4794161

Logit.preds.5 <- predict(Logit.fit.5, newdata = Blood.Test, type = "prob")
Submission.Logit5 <- data.frame(X = Blood.Test$X, Made.Donation.in.March.2007 = Logit.preds.5[, 2])
colnames(Submission.Logit5) <- c("", "Made Donation in March 2007")
write.csv(Submission.Logit5, "Submission3.csv", row.names = FALSE)
# Leaderboard Logloss = 0.4451

set.seed(1455)
formula.6 <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations + Months.since.First.Donation + I(Months.since.First.Donation^3)
Logit.fit.6 <- train(formula.6, data = Blood.Train, method = "glm", metric = "logLoss", family = "binomial", trControl = Logit.ctrl)

summary(Logit.fit.6)
print(Logit.fit.6)
# CV Logloss = 0.4844757

Logit.preds.6 <- predict(Logit.fit.6, newdata = Blood.Test, type = "prob")
Submission.Logit6 <- data.frame(X = Blood.Test$X, Made.Donation.in.March.2007 = Logit.preds.6[, 2])
colnames(Submission.Logit6) <- c("", "Made Donation in March 2007")
write.csv(Submission.Logit6, "Submission4.csv", row.names = FALSE)
# Leaderboard Logloss = 0.4411


## Model 2 : Random forests


Rf.ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = mnLogLoss,
  allowParallel = TRUE)

Rf.grid <- data.frame(.mtry = seq(1, 3, by = 1))

set.seed(1455)
formula <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations + Months.since.First.Donation
Rf.fit <- train(formula, data = Blood.Train, method = "rf", metric = "logLoss", tuneGrid = Rf.grid, trControl = Rf.ctrl)

summary(Rf.fit)
print(Rf.fit)
# CV Logloss = 1.237144 (?)

varImp(Rf.fit)
# Seems that "Months.since.Last.Donation" is not important at all

set.seed(1455)
formula.2 <- Made.Donation.in.March.2007 ~ Number.of.Donations + Months.since.First.Donation
Rf.fit.2 <- train(formula.2, data = Blood.Train, method = "rf", metric = "logLoss", tuneGrid = Rf.grid, trControl = Rf.ctrl)

summary(Rf.fit.2)
print(Rf.fit.2)
# CV Logloss = 1.484172 (?)
