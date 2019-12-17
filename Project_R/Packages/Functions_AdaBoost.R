##########################################
############ Get data ready ##############
##########################################


get_datas_ready_adaboost <- function(datas_train){
  datas_train$Label <- as.numeric(datas_train$Label)*2-1
  return(datas_train)
}


#################################################################
############ AdaBoost - Best parameters to be used ##############
#################################################################

try_parameters_AB <- function(datas, params){
  data_train <- get_training_datas(datas, AB=TRUE)
  x <- as.matrix(data_train[,-length(data_train)])
  y <- data_train$Label
  folds <- createFolds(y,
                       k = 4,
                       list = TRUE, 
                       returnTrain = FALSE)
  
  results <- list()
  for (i in 1:nrow(params)){
    tree_depth <- params[i,]$tree_depth
    n_rounds <- params[i,]$n_rounds
    verbose <- params[i,]$verbose
    accuracy <- 0
    for (j in 1:length(folds)){
      x_training <- x[-folds[[j]],]
      y_training <- y[-folds[[j]]]
      x_val <- x[folds[[j]],]
      y_val <- y[folds[[j]]]
      AB <- adaboost(X = x_training,
                     y = y_training,
                     tree_depth = tree_depth,
                     n_rounds = n_rounds,
                     verbose = verbose)
      predicted <- predict(AB, x_val)
      acc <- confusionMatrix(as.factor(predicted), as.factor(y_val))$overall[[1]]
      accuracy <- accuracy + acc
    }
    results <- c(results, accuracy/length(folds))
  }
  results <- as.vector(results)
  for (i in 1:length(results)){
    cat(sprintf('%i - %i -> %.4f \n', params[i,]$tree_depth, params[i,]$n_rounds, results[[i]]))
  }
  best_result <- which.max(results)
  
  cat(sprintf('\n The best result is: \n\n Accuracy: \t %.4f \n Tree depth: \t %i \n n rounds: \t %i',
              results[[best_result]], params[best_result,]$tree_depth, params[best_result,]$n_rounds))
  
  depth_tried <- unique(params$tree_depth)
  nrounds_tried <- unique(params$n_rounds)
  
  p <- matrix(1:4, byrow=T, ncol=2)
  layout(p)
  
  if (length(depth_tried)!=1){
    for (i in 1:length(nrounds_tried)){
      results_plot <- results[params$n_rounds==nrounds_tried[[i]]]
      plot(depth_tried,
           results_plot,
           type = 'l',
           col = 'red',
           main = sprintf('n_rounds = %i', nrounds_tried[[i]]))
    }
  }
  if (length(nrounds_tried)!=1){
    for (i in 1:length(depth_tried)){
      results_plot <- results[params$tree_depth==depth_tried[[i]]]
      plot(nrounds_tried,
           results_plot,
           type = 'l',
           col = 'blue',
           main = sprintf('Tree depth = %i', depth_tried[[i]]))
    }
  }
}