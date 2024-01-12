library(data.table)
library(caret)
library(randomForest)
library(Metrics)
library(ggplot2)

# mozliwe ze konieczne bedzie dostosowanie sciezki do pliku
source("../lab3/lab3-data-preparation.R")

source("functions.R")

# przekodowanie cech jakościowych na dummy vars (o wartościach 0 i 1)
# oraz proste uzupełnienie braków danych (NA)
preprocess_models <- preprocess(cases_train)

# zaaplikowanie zbudowanych modeli preprocesingu na naszych zbiorach
cases_train_to_model <- fitPreprocessing(preprocess_models, newdata = cases_train)
cases_valid_to_model <- fitPreprocessing(preprocess_models, newdata = cases_valid)
cases_test_to_model <- fitPreprocessing(preprocess_models, newdata = cases_test)

# factor wymagany do modelu klasyfikacyjnego
target_train <- targets_train[,factor(IfPayment)]
target_valid <- targets_valid[,factor(IfPayment)]
target_test <- targets_test[,factor(IfPayment)]

# przykład dla modelu random forest
# losowo wybrana krata hiperparametrów

hp_grid <- expand.grid(
  list(
    ntree = c(5, 25, 50, 100),
    mtry = c(5, 10, 20),
    sampsize_prop = 7:8/10,
    max_depth = 3:6,
    nodesize = c(10, 50, 100, 500, 1000)
  ))


models_summary <- data.table()
best_auc <- 0.0

for(grid_row in 1:nrow(hp_grid)) { # grid_row=1
  
  cat("hyperparameters #", grid_row, "/", nrow(hp_grid), "\n")
  print(hp_grid[grid_row,])
  
  model <- randomForest(
    x = cases_train_to_model,
    y = target_train,
    ntree = hp_grid[grid_row, "ntree"],
    # HPs:
    mtry = hp_grid[grid_row, "mtry"],
    sampsize = floor(cases_train[,.N] * hp_grid[grid_row, "sampsize_prop"]),
    maxnodes = 2^hp_grid[grid_row, "max_depth"],
    nodesize = hp_grid[grid_row, "nodesize"],
    na.action = "na.fail"
  )
  
  valid_predicted_labels <- predict(model, cases_valid_to_model, type = "response")
  valid_predicted_prob <- predict(model, cases_valid_to_model, type = "prob")
  
  acc_valid <- Metrics::accuracy(target_valid, valid_predicted_labels)
  auc_valid <- Metrics::auc(target_valid, valid_predicted_prob[,2])
  
  cat("Acc: ", acc_valid, "\n")
  cat("AUC: ", auc_valid, "\n")
  cat("\n")
  
  model_info <- data.table(
    ntree = hp_grid[grid_row, "ntree"],
    mtry = hp_grid[grid_row, "mtry"],
    sampsize = floor(cases_train[,.N] * hp_grid[grid_row, "sampsize_prop"]),
    maxnodes = 2^hp_grid[grid_row, "max_depth"],
    nodesize = hp_grid[grid_row, "nodesize"],
    
    acc_valid = acc_valid,
    auc_valid = auc_valid
  )
  
  
  if(auc_valid > best_auc) {
    best_auc <- auc_valid
    save(model, model_info, file = "models/best_model.RData")
  }
  
  models_summary <- rbindlist(list(
    models_summary,
    model_info
  ))
  
}

setorder(models_summary, -auc_valid)

save(models_summary, file = "models.RData")



# Klika wykresów --------------------------------------------------------------------------------------------------


ggplot(data = models_summary) +
  geom_point(aes(acc_valid, auc_valid)) +
  ggtitle("metrics' dependency", subtitle = "Top-right are the best") +
  labs(x = "Accuracy", y = "AUC") +
  theme_bw()


models_summary_long <- melt(
  data = models_summary, 
  measure.vars = c("ntree", "mtry", "sampsize", "maxnodes", "nodesize"), 
  value.name = c("hp_value"),
  variable.name = "hyperparam")


ggplot(data = models_summary_long) +
  geom_point(aes(hp_value, auc_valid)) +
  facet_wrap(~hyperparam, scales = "free_x") +
  ggtitle("Single HP vs AUC") +
  labs(x = "HP Value", y = "AUC") +
  theme_bw()

ggplot(data = models_summary_long) +
  geom_point(aes(hp_value, acc_valid)) +
  facet_wrap(~hyperparam, scales = "free_x") +
  ggtitle("Single HP vs ACC") +
  labs(x = "HP Value", y = "ACC") +
  theme_bw()



# Predykcje na zbiorze testowym  ----------------------------------------------------------------------------------


load("models/best_model.RData")

print(model_info)

test_labels <- predict(model, newdata=cases_test_to_model, type = "response")
test_probabilities <- predict(model, newdata=cases_test_to_model, type = "prob")

acc_test <- Metrics::accuracy(target_test, test_labels)
auc_test <- Metrics::auc(target_test, test_probabilities[,2])

cat("Accuracy on test set:\t", acc_test, "\n")
cat("AUC on test set:\t", auc_test, "\n")


