## Libraries


library(caret)
library(ggplot2)
library(caretEnsemble)



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

set.seed(1455)
formula.7 <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations + I(Months.since.First.Donation^2)
Logit.fit.7 <- train(formula.7, data = Blood.Train, method = "glm", metric = "logLoss", family = "binomial", preProc = c("center", "scale"), trControl = Logit.ctrl)

summary(Logit.fit.7)
print(Logit.fit.7)
# CV Logloss = 0.4833443


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
Rf.fit.1 <- train(formula, data = Blood.Train, method = "rf", metric = "logLoss", tuneGrid = Rf.grid, trControl = Rf.ctrl)

summary(Rf.fit.1)
print(Rf.fit.1)
# CV Logloss = 1.237144 (?)

varImp(Rf.fit)
# Seems that "Months.since.Last.Donation" is not important at all

set.seed(1455)
formula.2 <- Made.Donation.in.March.2007 ~ Number.of.Donations + Months.since.First.Donation
Rf.fit.2 <- train(formula.2, data = Blood.Train, method = "rf", metric = "logLoss", tuneGrid = Rf.grid, trControl = Rf.ctrl)

summary(Rf.fit.2)
print(Rf.fit.2)
# CV Logloss = 1.484172 (?)

Rf.preds.1 <- predict(Rf.fit.1, newdata = Blood.Test, type = "prob")
Submission.Rf1 <- data.frame(X = Blood.Test$X, Made.Donation.in.March.2007 = Rf.preds.1[, 2])
colnames(Submission.Rf1) <- c("", "Made Donation in March 2007")
write.csv(Submission.Rf1, "Submission5.csv", row.names = FALSE)
# Leaderboard Logloss = 1.7678 (Very bad !)


## Model 3 : Penalized logistic regression


Plr.ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = mnLogLoss,
  allowParallel = TRUE)

Plr.grid <- expand.grid(lambda = c(0.5), cp = c("bic", "aic"))

set.seed(1455)
formula <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations + Months.since.First.Donation
Plr.fit.1 <- train(formula, data = Blood.Train, method = "plr", metric = "logLoss", tuneGrid = Plr.grid, trControl = Plr.ctrl)

summary(Plr.fit.1)
print(Plr.fit.1)
# CV Logloss = 0.4882591

Plr.preds.1 <- predict(Plr.fit.1, newdata = Blood.Test, type = "prob")
Submission.Plr1 <- data.frame(X = Blood.Test$X, Made.Donation.in.March.2007 = Plr.preds.1[, 2])
colnames(Submission.Plr1) <- c("", "Made Donation in March 2007")
write.csv(Submission.Plr1, "Submission6.csv", row.names = FALSE)
# Leaderboard Logloss = 0.4457

Plr.grid.2 <- expand.grid(lambda = c(0.5), cp = c("bic", "aic"))

set.seed(1455)
formula.2 <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations + I(Months.since.First.Donation^2)
Plr.fit.2 <- train(formula.2, data = Blood.Train, method = "plr", metric = "logLoss", tuneGrid = Plr.grid.2, trControl = Plr.ctrl)

summary(Plr.fit.2)
print(Plr.fit.2)
# CV Logloss = 0.483337

Plr.preds.2 <- predict(Plr.fit.2, newdata = Blood.Test, type = "prob")
Submission.Plr2 <- data.frame(X = Blood.Test$X, Made.Donation.in.March.2007 = Plr.preds.2[, 2])
colnames(Submission.Plr2) <- c("", "Made Donation in March 2007")
write.csv(Submission.Plr2, "Submission7.csv", row.names = FALSE)
# Leaderboard Logloss = 0.4384 (Best)


## Model 4 : Conditional inference forests


Cif.ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = mnLogLoss,
  allowParallel = TRUE)

Cif.grid <- expand.grid(mtry = seq(2, 3, 1))

set.seed(1455)
formula <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations + Months.since.First.Donation
Cif.fit <- train(formula, data = Blood.Train, method = "cforest", metric = "logLoss", preProc = c("center", "scale"), controls = cforest_unbiased(ntree = 500), trControl = Cif.ctrl, tuneGrid = Cif.grid, maximize = FALSE)

summary(Cif.fit)
print(Cif.fit)
# CV Logloss = 0.5001579

set.seed(1455)
formula.2 <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations + I(Months.since.First.Donation^2)
Cif.fit.2 <- train(formula.2, data = Blood.Train, method = "cforest", metric = "logLoss", preProc = c("center", "scale"), controls = cforest_unbiased(ntree = 500), trControl = Cif.ctrl, tuneGrid = Cif.grid, maximize = FALSE)

summary(Cif.fit.2)
print(Cif.fit.2)
# CV Logloss = 0.5000875

Cif.preds.2 <- predict(Cif.fit.2, newdata = Blood.Test, type = "prob")
Submission.Cif2 <- data.frame(X = Blood.Test$X, Made.Donation.in.March.2007 = Cif.preds.2[, 2])
colnames(Submission.Cif2) <- c("", "Made Donation in March 2007")
write.csv(Submission.Cif2, "Submission8.csv", row.names = FALSE)
# Leaderboard Logloss = 0.4568


## Model 4 : LDA


Lda.ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = mnLogLoss,
  allowParallel = TRUE)

set.seed(1455)
formula <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations + Months.since.First.Donation
Lda.fit <- train(formula, data = Blood.Train, method = "lda", metric = "logLoss", trControl = Lda.ctrl, maximize = FALSE)

summary(Lda.fit)
print(Lda.fit)
# CV Logloss = 0.4906083

set.seed(1455)
formula.2 <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations + I(Months.since.First.Donation^2)
Lda.fit.2 <- train(formula.2, data = Blood.Train, method = "lda", metric = "logLoss", trControl = Lda.ctrl, maximize = FALSE)

summary(Lda.fit.2)
print(Lda.fit.2)
# CV Logloss = 0.4858648

Lda.preds.2 <- predict(Lda.fit.2, newdata = Blood.Test, type = "prob")
Submission.Lda2 <- data.frame(X = Blood.Test$X, Made.Donation.in.March.2007 = Lda.preds.2[, 2])
colnames(Submission.Lda2) <- c("", "Made Donation in March 2007")
write.csv(Submission.Lda2, "Submission9.csv", row.names = FALSE)
# Leaderboard Logloss = 0.4525


## Model 5 : GBM


Boost.ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = mnLogLoss,
  allowParallel = TRUE)

Boost.grid <- expand.grid(interaction.depth = c(2, 4), n.trees = (20:60) * 50, shrinkage = c(0.001), n.minobsinnode = 10)

set.seed(1455)
formula <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations
Boost.fit <- train(formula, data = Blood.Train, method = "gbm", metric = "logLoss", trControl = Boost.ctrl, tuneGrid = Boost.grid, maximize = FALSE, verbose = FALSE)

summary(Boost.fit)
print(Boost.fit)
# CV Logloss = 0.4779473

Boost.preds <- predict(Boost.fit, newdata = Blood.Test, type = "prob")
Submission.Boost <- data.frame(X = Blood.Test$X, Made.Donation.in.March.2007 = Boost.preds[, 2])
colnames(Submission.Boost) <- c("", "Made Donation in March 2007")
write.csv(Submission.Boost, "Submission10.csv", row.names = FALSE)
# Leaderboard Logloss = 0.4561

set.seed(1455)
formula.2 <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations + I(Months.since.First.Donation^2)
Boost.fit.2 <- train(formula.2, data = Blood.Train, method = "gbm", metric = "logLoss", trControl = Boost.ctrl, tuneGrid = Boost.grid, maximize = FALSE, verbose = FALSE)

summary(Boost.fit.2)
print(Boost.fit.2)
# CV Logloss = 0.4778955

Boost.preds.2 <- predict(Boost.fit.2, newdata = Blood.Test, type = "prob")
Submission.Boost2 <- data.frame(X = Blood.Test$X, Made.Donation.in.March.2007 = Boost.preds.2[, 2])
colnames(Submission.Boost2) <- c("", "Made Donation in March 2007")
write.csv(Submission.Boost2, "Submission11.csv", row.names = FALSE)
# Leaderboard Logloss = 0.4558


## Model 6 : QDA


Qda.ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = mnLogLoss,
  allowParallel = TRUE)

set.seed(1455)
formula <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations + Months.since.First.Donation
Qda.fit <- train(formula, data = Blood.Train, method = "qda", metric = "logLoss", trControl = Qda.ctrl, maximize = FALSE)

summary(Qda.fit)
print(Qda.fit)
# CV Logloss = 0.6309947

set.seed(1455)
formula.2 <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations + I(Months.since.First.Donation^2)
Qda.fit.2 <- train(formula.2, data = Blood.Train, method = "qda", metric = "logLoss", trControl = Qda.ctrl, maximize = FALSE)

summary(Qda.fit.2)
print(Qda.fit.2)
# CV Logloss = 0.6241451


## Model 7 : SVM


Svm.ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = mnLogLoss,
  allowParallel = TRUE)

Svm.grid <- expand.grid(C = c(0.0001, 0.001, 0.01, 0.1, 1.5, 10, 100))

set.seed(1455)
formula <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations + Months.since.First.Donation
Svm.fit <- train(formula, data = Blood.Train, method = "svmLinear", metric = "logLoss", trControl = Svm.ctrl, tuneGrid = Svm.grid, maximize = FALSE)

summary(Svm.fit)
print(Svm.fit)
# CV Logloss = 0.5218798

set.seed(1455)
formula.2 <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations + I(Months.since.First.Donation^2)
Svm.fit.2 <- train(formula.2, data = Blood.Train, method = "svmLinear", metric = "logLoss", trControl = Svm.ctrl, tuneGrid = Svm.grid, maximize = FALSE, verbose = FALSE)

summary(Svm.fit.2)
print(Svm.fit.2)
# CV Logloss = 0.4987450

Svm.preds.2 <- predict(Svm.fit.2, newdata = Blood.Test, type = "prob")
Submission.Svm2 <- data.frame(X = Blood.Test$X, Made.Donation.in.March.2007 = Svm.preds.2[, 2])
colnames(Submission.Svm2) <- c("", "Made Donation in March 2007")
write.csv(Submission.Svm2, "Submission12.csv", row.names = FALSE)
# Leaderboard Logloss = 0.4682

Svm.grid.2 <- expand.grid(degree = c(2, 3, 4), scale = c(0.001, 0.01, 0.1), C = c(0.0001, 0.001, 0.01, 0.1, 1.5, 10, 100))

set.seed(1455)
formula.2 <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations + I(Months.since.First.Donation^2)
Svm.fit.3 <- train(formula.2, data = Blood.Train, method = "svmPoly", metric = "logLoss", trControl = Svm.ctrl, tuneGrid = Svm.grid.2, maximize = FALSE, verbose = FALSE)

summary(Svm.fit.3)
print(Svm.fit.3)
# CV Logloss = 0.5137325

Svm.grid.3 <- expand.grid(sigma = c(0.5, 1, 2, 3, 4), C = c(0.0001, 0.001, 0.01, 0.1, 1.5, 10, 100))

set.seed(1455)
formula.2 <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations + I(Months.since.First.Donation^2)
Svm.fit.4 <- train(formula.2, data = Blood.Train, method = "svmRadial", metric = "logLoss", trControl = Svm.ctrl, tuneGrid = Svm.grid.3, maximize = FALSE, verbose = FALSE)

summary(Svm.fit.4)
print(Svm.fit.4)
# CV Logloss = 0.4904827


## Model 8 : Averaging


Avg.ctrl <- trainControl(method = "repeatedcv",
                         repeats = 3,
                         index = createResample(Blood.Train$Made.Donation.in.March.2007, 25),
                         summaryFunction = mnLogLoss,
                         savePredictions = TRUE,
                         classProbs = TRUE)

#levels(Train$Survived) <- c("No", "Yes")

set.seed(1455)
formula.2 <- Made.Donation.in.March.2007 ~ Months.since.Last.Donation + Number.of.Donations + I(Months.since.First.Donation^2)
models.list1 <- caretList(formula.2, data = Blood.Train, metric = "logLoss", trControl = Avg.ctrl, tuneList = list(
  logit = caretModelSpec(method = "glm", family = "binomial"),
  gbm = caretModelSpec(method = "gbm", tuneGrid = expand.grid(interaction.depth = c(2, 4), n.trees = (20:60) * 50, shrinkage = c(0.001), n.minobsinnode = 10), verbose = FALSE)
  )
)

modelCor(resamples(models.list1))

Avg.fit <- caretEnsemble(models.list1)
# CV Logloss = 0.483158

Avg.preds <- predict(Avg.fit, newdata = Blood.Test)
Submission.Avg <- data.frame(X = Blood.Test$X, Made.Donation.in.March.2007 = Avg.preds)
colnames(Submission.Avg) <- c("", "Made Donation in March 2007")
write.csv(Submission.Avg, "Submission13.csv", row.names = FALSE)
# Leaderboard Logloss = 0.4406
