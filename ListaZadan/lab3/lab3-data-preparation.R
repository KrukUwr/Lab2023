
set.seed(123)

samples <- caret::createFolds(
  y = cases$CaseId, 
  k=3)

events[is.na(PaymentAmount), PaymentAmount:=0.0]
events[is.na(NumberOfPayment), NumberOfPayment:=0]

payments_12m <- events[,.(
    IfPayment = as.integer(sum(NumberOfPayment)>0), 
    SumOfPayments = sum(PaymentAmount)),
  by = .(CaseId)]

features_cols <- names(cases)
targets_cols <- names(payments_12m)

setkey(cases, CaseId)
setkey(payments_12m, CaseId)

cases_all <- cases[payments_12m]

cases_train <- cases_all[samples[[1]], .SD, .SDcols = features_cols]
cases_valid <- cases_all[samples[[2]], .SD, .SDcols = features_cols]
cases_test <- cases_all[samples[[3]], .SD, .SDcols = features_cols]

targets_train <- cases_all[samples[[1]], .SD, .SDcols = targets_cols]
targets_valid <- cases_all[samples[[2]], .SD, .SDcols = targets_cols]
targets_test <- cases_all[samples[[3]], .SD, .SDcols = targets_cols]

rm(cases, events, payments_12m, features_cols, targets_cols, samples, cases_all)
gc()

## Funkcja predict dla modelu klasy kmeans:

predict.kmeans <- function(model, newdata) {
  y <- apply(newdata, 1, function(r) {
    which.min(colSums((t(model$centers) - r)^2))
  })
  return(y)
}



