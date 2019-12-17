#######################################
############ Build Model ##############
#######################################

build_model <- function(input_shape, layers_size, regularizer='none',
                        dropout='none', activation='relu', optimizer='rmsprop', momentum=NULL){
  if (dropout=='none'){
    dropout <- NULL
  }
  
  if(regularizer=='none'){
    kernel_regularizer <- NULL
    batch_normalization <- FALSE
  }
  else if(regularizer=='bn'){
    kernel_regularizer <- NULL
    batch_normalization <- TRUE
  }
  else if(regularizer[1]=='l1'){
    kernel_regularizer <- regularizer_l1(as.numeric(regularizer[2]))
    batch_normalization <- FALSE
  }
  else if(regularizer[1]=='l2'){
    kernel_regularizer <- regularizer_l2(as.numeric(regularizer[2]))
    batch_normalization <- FALSE
  }
  
  if (optimizer=='nesterov'){
    optimizer <- optimizer_sgd(momentum = momentum, nesterov = TRUE)
  }
  else if (optimizer=='rmsprop'){
    optimizer <- optimizer_rmsprop()
  }
  else if(optimizer=='adam'){
    optimizer <- optimizer_adam()
    
    
  }
  
  model <- keras_model_sequential()
  model %>%
    layer_dense(units = layers_size[[1]], activation = activation, input_shape = input_shape,
                kernel_initializer = initializer_variance_scaling(), kernel_regularizer = kernel_regularizer)
  if (!is.null(dropout)){
    model %>%
      layer_dropout(rate=dropout)
  }
  if (batch_normalization){
    model %>%
      layer_batch_normalization()
  }
  if (length(layers_size)!=1){
    for (i in 2:length(layers_size)){
      model %>%
        layer_dense(units = layers_size[[i]], activation = activation,
                    kernel_initializer = initializer_variance_scaling(),
                    kernel_regularizer = kernel_regularizer)
      if (!is.null(dropout)){
        model %>%
          layer_dropout(rate=dropout)
      }
      if (batch_normalization){
        model %>%
          layer_batch_normalization()
      }
    }
  }
  model %>%
    layer_dense(units = 1, activation = 'sigmoid')
  model %>% compile(
    loss = 'binary_crossentropy',
    optimizer = optimizer,
    metrics = 'accuracy'
  )
}


##########################################
############ Try parameters ##############
##########################################


get_scaler <- function(x_train){
  scaler <- list()
  scaler$mean <- apply(x_train, MARGIN=2, FUN=mean)
  scaler$std <- apply(x_train, MARGIN=2, FUN=sd)
  return(scaler)
}


standardize_data <- function(x_train, scaler){
  scaled_x_train <- t((t(x_train) - unname(scaler$mean))/unname(scaler$std))
  return(scaled_x_train)
}


get_callbacks <- function(ckpt=FALSE, no_val=FALSE, patience=60){
  if (no_val){
    monitor <- 'accuracy'
  }
  else{
    monitor <- 'val_accuracy'
  }
  
  checkpoint_dir <- 'NN_checkpoints'
  filepath <- file.path(checkpoint_dir, "weights.{epoch:02d}-{accuracy:.3f}.hdf5")
  model_checkpoint <- callback_model_checkpoint(filepath, monitor = monitor, verbose = 1,
                                                save_best_only = TRUE, mode = "max")#, period = 1)
  early_stopping <- callback_early_stopping(monitor=monitor, patience=100, verbose=1, mode='max',
                                            restore_best_weights=TRUE)
  reduce_lr <- callback_reduce_lr_on_plateau(monitor=monitor, factor=0.6, patience=patience, verbose=1,#60
                                             mode='max', min_lr=0.000001)
  if (ckpt){
    return(list(model_checkpoint, early_stopping, reduce_lr))
  }
  else{
    return(list(early_stopping, reduce_lr))
  }
}


print_accuracy_parameters <- function(results, params, index){
  cat(sprintf('layers_size: \t %s \n', paste(unlist(params[index,]$layers_size), collapse=' ')))
  cat(sprintf('regularizer: \t %s \n', paste(unlist(params[index,]$regularizer), collapse=' ')))
  cat(sprintf('dropout: \t %.2f \n', params[index,]$dropout))
  cat(sprintf('activation: \t %s \n', params[index,]$activation))
  cat(sprintf('optimizer: \t %s \n', params[index,]$optimizer))
  if ('nesterov' %in% params$optimizer){
    cat(sprintf('momentum: \t %.2f \n', params[index,]$momentum))
  }
  cat(sprintf('epochs: \t %i \n', params[index,]$epochs))
  cat(sprintf('batch_size: \t %i \n', params[index,]$batch_size))
  cat(sprintf('-----> Accuracy: \t %.4f \n \n', results[[index]]))
}


try_parameters_NN <- function(datas, params){
  data_train <- get_training_datas(datas)
  x_train <- as.matrix(data_train[,-length(data_train)])
  y_train <- as.numeric(data_train[,length(data_train)])-1
  
  folds <- createFolds(y_train,
                       k = 3,
                       list = TRUE, 
                       returnTrain = FALSE)
  
  p <- matrix(1:4, byrow=T, ncol=2)
  layout(p)
  
  results <- list()
  for (i in 1:nrow(params)){
    layers_size <- unlist(params[i,]$layers_size)
    regularizer <- unlist(params[i,]$regularizer)
    dropout <- params[i,]$dropout
    activation <- params[i,]$activation
    optimizer <- params[i,]$optimizer
    momentum <- params[i,]$momentum
    epochs <- params[i,]$epochs
    batch_size <- params[i,]$batch_size
    accuracy_plot <- rep(0, epochs)
    for (j in 1:length(folds)){
      x_training <- x_train[-folds[[j]],]
      y_training <- y_train[-folds[[j]]]
      scaler <- get_scaler(x_training)
      scaled_x_training <- standardize_data(x_training, scaler)
      x_val <- x_train[folds[[j]],]
      y_val <- y_train[folds[[j]]]
      scaled_x_val <- standardize_data(x_val, scaler)
      model <- build_model(input_shape = ncol(scaled_x_training),
                           layers_size = layers_size,
                           regularizer = regularizer,
                           dropout = dropout,
                           activation = activation,
                           optimizer = optimizer,
                           momentum = momentum)
      callbacks <- get_callbacks()
      history <- model %>% fit(scaled_x_training, 
                               y_training, 
                               epochs = epochs, 
                               batch_size = batch_size,
                               validation_data = list(scaled_x_val, y_val),
                               callbacks = callbacks)
      add_to_accuracy <- history$metric$val_accuracy[!is.null(history$metric$val_accuracy)]
      index_min <- min(c(length(accuracy_plot), length(add_to_accuracy)))
      accuracy_plot <- accuracy_plot[1:index_min] + add_to_accuracy[1:index_min]
      if (j==1){
        plot(1:length(history$metric$val_accuracy), history$metric$val_accuracy,
             type='l', col=j, xlim=c(0,epochs), ylim=c(0.81,0.91))
      }
      else{
        lines(1:length(history$metric$val_accuracy), history$metric$val_accuracy, col=j)
      }
    }
    accuracy_plot <- accuracy_plot/length(folds)
    accuracy <- max(accuracy_plot)
    results <- c(results, accuracy)
  }
  results <- as.vector(results)
  for (i in 1:length(results)){
    print_accuracy_parameters(results, params, i)
  }
  
  best_result <- which.max(results)
  best_layers_size <- params[best_result,]$layers_size
  best_batch_normalization <- params[best_result,]$batch_normalization
  best_dropout <- params[best_result,]$dropout
  best_activation <- params[best_result,]$activation
  best_optimizer <- params[best_result,]$optimizer
  best_epochs <- params[best_result,]$epochs
  best_batch_size <- params[best_result,]$batch_size
  
  cat(sprintf('The best result is: \n\n'))
  print_accuracy_parameters(results, params, best_result)
}


##########################################
############ Make NN models ##############
##########################################


make_NN_models <- function(best_parameters, datas_train, final=FALSE){
  models <- list()
  scalers <- list()
  samples_bkg <- NULL
  if (final){
    samples_bkg = NULL#500
  }
  data_train <- get_training_datas(datas_train, n_samples_bkg = samples_bkg)
  x_train <- as.matrix(data_train[,-length(data_train)])
  y_train <- as.numeric(data_train[,length(data_train)])-1
  folds <- createFolds(y_train,
                       k = 4,
                       list = TRUE, 
                       returnTrain = FALSE)
  x_training <- x_train[-folds[[1]],]
  y_training <- y_train[-folds[[1]]]
  x_val <- x_train[folds[[1]],]
  y_val <- y_train[folds[[1]]]
  
  for (i in 1:length(best_parameters$best_layers_size)){
    layers_size <- best_parameters$best_layers_size[[i]]
    regularizer <- best_parameters$best_regularizer
    dropout <- best_parameters$best_dropout
    activation <- best_parameters$best_activation
    optimizer <- best_parameters$best_optimizer
    momentum <- best_parameters$best_momentum
    epochs <- best_parameters$best_epochs 
    batch_size <- best_parameters$best_batch_size
    
    scaler <- get_scaler(x_training)
    scaled_x_train <- standardize_data(x_training, scaler)
    scaled_x_val <- standardize_data(x_val, scaler)
    
    
    model <- build_model(input_shape = ncol(scaled_x_train),
                         layers_size = layers_size,
                         regularizer = regularizer,
                         dropout = dropout,
                         activation = activation,
                         optimizer = optimizer,
                         momentum = momentum)
    
    callbacks <- get_callbacks(patience=25)
    model %>% fit(scaled_x_train, 
                  y_training,
                  epochs = epochs, 
                  batch_size = batch_size,
                  callbacks = callbacks,
                  validation_data = list(scaled_x_val, y_val))
    
    scalers <- append(scalers, list(scaler))
    models <- c(models, model)
  }
  models_and_scalers <- list(models = models,
                             scalers = scalers)
  return(models_and_scalers)
}


###########################################
############ Predict NN prob ##############
###########################################


predict_NN_prob <- function(NN_models_and_scalers, x_test){
  predicted_prob <- rep(0, nrow(x_test))
  
  models <- NN_models_and_scalers[[1]]
  scalers <- NN_models_and_scalers[[2]]
  x_test <- as.matrix(x_test)
  for (i in 1:length(models)){
    model <- models[[i]]
    scaler <- scalers[[i]]
    x_test_scaled <- standardize_data(x_test, scaler)
    pred_prob <- model %>% predict(x_test_scaled)
    predicted_prob <- predicted_prob + pred_prob
  }
  predicted_prob <- predicted_prob/length(models)
  return(predicted_prob)
}