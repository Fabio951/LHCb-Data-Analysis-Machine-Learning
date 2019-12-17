######################################################
############ Best parameters to be used ##############
######################################################

try_parameters <- function(method, parameters, datas_train){
  data_train <- get_training_datas(datas_train)
  trControl <- trainControl(method = "cv",
                            number = 5,
                            search = "grid")
  random_forest_try <- train(Label ~ .,
                             data = data_train,
                             method = method,
                             metric = "Accuracy",
                             tuneGrid = parameters,
                             trControl = trControl)
  print(random_forest_try)
  plot(random_forest_try)
}