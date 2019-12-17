#################################################################
############ GradBoost - Best parameters to be used ##############
#################################################################

try_parameters_GB <- function(datas, params){
  data_train <- get_training_datas(datas)
  x <- as.matrix(data_train[,-length(data_train)])
  y <- as.numeric(data_train[,length(data_to_train)])-1
  folds <- createFolds(y,
                       k = 5,
                       list = TRUE, 
                       returnTrain = FALSE)
  
  results <- list()
  for (i in 1:nrow(params)){
    max.depth <- params[i,]$max.depth
    eta <- params[i,]$eta
    nrounds <- params[i,]$nrounds
    gamma <- params[i,]$gamma
    early_stopping_rounds <- params[i,]$early_stopping_rounds
    accuracy <- 0
    for (j in 1:length(folds)){
      x_training <- x[-folds[[j]],]
      y_training <- y[-folds[[j]]]
      x_val <- x[folds[[j]],]
      y_val <- y[folds[[j]]]
      GB <- xgboost(data = x_training,
                    label = y_training,
                    max.depth = max.depth,
                    eta = eta,
                    nrounds = nrounds,
                    gamma = gamma,
                    early_stopping_rounds = early_stopping_rounds,
                    objective = "binary:logistic",
                    verbose = 0)
      predicted <- as.integer(round(predict(GB, x_val)))
      acc <- confusionMatrix(as.factor(predicted), as.factor(y_val))$overall[[1]]
      accuracy <- accuracy + acc
    }
    results <- c(results, accuracy/length(folds))
  }
  results <- as.vector(results)
  for (i in 1:length(results)){
    cat(sprintf('max.depth: %i \t eta: %.5f \t nrounds: %i \t gamma: %.4f \t early_stopping_rounds: %i -> Accuracy: %.4f \n',
                params[i,]$max.depth, params[i,]$eta, params[i,]$nrounds, params[i,]$gamma,
                params[i,]$early_stopping_rounds, results[[i]]))
  }
  best_result <- which.max(results)
  best_max.depth <- params[best_result,]$max.depth
  best_eta <- params[best_result,]$eta
  best_nrounds <- params[best_result,]$nrounds
  best_gamma <- params[best_result,]$gamma
  best_early_stopping_rounds <- params[best_result,]$early_stopping_rounds
  
  best_parameters <- list(best_max.depth = best_max.depth,
                          best_eta = best_eta,
                          best_nrounds = best_nrounds,
                          best_gamma = best_gamma,
                          best_early_stopping_rounds = best_early_stopping_rounds)
  
  cat(sprintf('\n The best result is: \n\n Accuracy: \t %.4f \n max.depth: %i \n eta: \t %.7f \n nrounds: \t %i \n gamma: \t %.4f \n early_stopping_rounds: \t %i \n',
              results[[best_result]], best_max.depth, best_eta, best_nrounds, best_gamma, best_early_stopping_rounds))
  
  max.depth_tried <- unique(params$max.depth)
  eta_tried <- unique(params$eta)
  nrounds_tried <- unique(params$nrounds)
  gamma_tried <- unique(params$gamma)
  early_stopping_rounds_tried <- unique(params$early_stopping_rounds)
  
  parameters_tried <- list(max.depth_tried = max.depth_tried,
                           eta_tried = eta_tried,
                           nrounds_tried = nrounds_tried,
                           gamma_tried = gamma_tried,
                           early_stopping_rounds_tried = early_stopping_rounds_tried)
  
  longest_sequence <- which.max(unlist(lapply(parameters_tried, FUN=length), use.names=FALSE))
  names_params <- names(params)
  cat(sprintf('\n'))
  filter <- rep(0,length(results))
  for (i in (1:length(parameters_tried))[-longest_sequence]){
    filter <- filter + as.integer(params[[i]]==best_parameters[[i]])
    cat(sprintf('%s used for plot: \t %.7f \n', names_params[[i]], best_parameters[[i]]))
  }
  results_plot <- results[filter==(length(parameters_tried)-1)]
  plot(parameters_tried[[longest_sequence]],
       results_plot,
       type = 'l',
       col = 'red',
       xlab = names_params[longest_sequence])
}