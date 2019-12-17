######################################################################
############ Best number of samples from the background ##############
######################################################################


get_plots_fraction_tot_samples <- function(datas, samples, method, times=1){
  data_sig_train <- datas[['data_sig']]
  data_bkg <- datas[['data_bkg']]
  accuracies <- list()
  classifiers <- list()
  
  trControl <- trainControl(method = "cv",
                            number = 5)
  tuneGrid <- expand.grid(.mtry = 5,
                          .ntree = 400,
                          .maxnodes = 150,
                          .importance = FALSE,
                          .nodesize = 150)
  
  for (time_ in 1:times){
    for (i in 1:length(samples)){
      numb_samples <- samples[[i]]
      train_set_bkg <- sample(1:nrow(data_bkg), numb_samples)
      data_bkg_train <- data_bkg[train_set_bkg, ]
      data_train <- rbind(data_bkg_train, data_sig_train)
      data_train <- data_train[sample(1:nrow(data_train), nrow(data_train)), ]
      data_train[['Label']] <- as.factor(data_train[['Label']])
      forest_clf <- train(Label ~ .,
                          data = data_train,
                          method = method,
                          metric = "Accuracy",
                          trControl = trControl,
                          tuneGrid = tuneGrid)
      accuracies[[as.character(time_)]][[as.character(samples[[i]])]] <- forest_clf$results$Accuracy
      classifiers[[as.character(time_)]][[as.character(samples[[i]])]] <- forest_clf
    }
  }
  
  y_max <- 0
  y_min <- 1
  for (time_ in 1:times){
    for (i in 1:length(samples)) {
      acc_ <- accuracies[[as.character(time_)]][[as.character(samples[[i]])]]
      if (acc_>y_max){
        y_max <- acc_
      }
      if(acc_<y_min){
        y_min <- acc_
      }
    }
  }
  
  for (time_ in 1:times){
    if (time_==1){
      plot(samples, accuracies[[as.character(time_)]],
           type = 'b',
           col = time_,
           lty = time_,
           pch = time_,
           ylab = 'Accuracy',
           ylim = c(y_min, y_max))
    }
    else{
      
      lines(samples, accuracies[[as.character(time_)]],
            type = 'b',
            col = time_,
            lty = time_,
            pch = time_)
    }
  }
  
  p <- matrix(1:4, byrow=T, ncol=2)
  layout(p)
  
  for (i in 1:length(samples)){
    if (times==1){
      classifier <- classifiers[[as.character(1)]][[as.character(samples[[i]])]]
      predicted_proba <- predict(classifier, data_train, type='prob')
      hist(predicted_proba[[1]][data_train[['Label']]==1],
           col = 'red',
           freq = FALSE,
           breaks = 20,
           density = 30,
           angle = 45,
           xlim = c(0,1),
           ylim = c(0,9),
           main = as.character(samples[[i]]),
           xlab = 'prob',
           ylab = 'frequency')
      hist(predicted_proba[[1]][data_train[['Label']]==0],
           col = 'green',
           freq = FALSE,
           breaks = 20,
           density = 30,
           add = TRUE,
           angle = -45)
      legend("top", c('Signal', 'Background'), fill=c('red', 'green'), cex=0.5)
    }
  }
  
  return(accuracies)
}