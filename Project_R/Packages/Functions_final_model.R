###########################################
############ Make Final Model ##############
###########################################


make_final_model <- function(nn_models, adaboost, gradboost, rf_all, rf_usf, rf_opt, rf_ens, x_val, y_val, drop_columns){
  predictions_prob <- list()
  
  pred_prob_rf_all <- unname(predict(rf_all, x_val, type='prob')[,2])
  x_val_usf <- clean_datas(x_val, only_useful=TRUE)
  pred_prob_rf_usf <- unname(predict(rf_usf, x_val_usf, type='prob')[,2])
  x_val_opt <- clean_datas(x_val_usf, drop_columns=drop_columns)
  pred_prob_rf_opt <- unname(predict(rf_opt, x_val_opt, type='prob')[,2])
  
  predictions_prob <- append(predictions_prob, list(pred_prob_rf_all))
  predictions_prob <- append(predictions_prob, list(pred_prob_rf_usf))
  predictions_prob <- append(predictions_prob, list(pred_prob_rf_opt))
  
  votes_proba <- matrix(0, nrow(x_val), 2)
  x_val_ensemble <- list(datas_train = x_val,
                         datas_useful = x_val_usf,
                         datas_optimized = x_val_opt)
  for (i in 1:length(rf_ens$classifiers)){
    datas_choice <- rf_ens$datas_to_be_used[[as.character(i)]]
    x_val_prepared <- x_val_ensemble[[datas_choice]]
    predicted <- predict(rf_ens$classifiers[[as.character(i)]], x_val_prepared, type='prob')
    votes_proba <- votes_proba + predicted
  }
  pred_prob_rf_ens <- (unname(votes_proba)/length(rf_ens$classifiers))[,2]
  predictions_prob <- append(predictions_prob, list(pred_prob_rf_ens))
  
  pred_prob_AB <- predict(adaboost, x_val, type='prob')
  predictions_prob <- append(predictions_prob, list(pred_prob_AB))
  
  pred_prob_GB <- predict(gradboost, as.matrix(x_val), type='prob')
  predictions_prob <- append(predictions_prob, list(pred_prob_GB))
  
  for (i in 1:length(nn_models$models)){
    model <- nn_models$models[[i]]
    scaler <- nn_models$scalers[[i]]
    x_val_scaled <- standardize_data(as.matrix(x_val), scaler)
    pred_prob_NN <- model %>% predict(x_val_scaled)
    predictions_prob <- append(predictions_prob, list(pred_prob_NN))
  }
  
  final_model <- keras_model_sequential()
  final_model %>%
    layer_dense(units = 1, input_shape = length(predictions_prob),
                activation = 'relu',
                bias_constraint = constraint_minmaxnorm(min_value=0, max_value = 0),
                kernel_constraint = constraint_nonneg(),#(min_value=0.99, max_value = 10),
                kernel_regularizer = regularizer_l1(0.1),
                kernel_initializer = initializer_constant(value=1/(length(predictions_prob))))
  final_model %>% compile(
    loss = 'binary_crossentropy',
    optimizer = 'rmsprop',
    metrics = 'accuracy'
  )
  callbacks <- get_callbacks(no_val=TRUE, patience=25)
  predictions_prob_matrix <- matrix(unlist(predictions_prob), ncol=length(predictions_prob), byrow=FALSE)
  final_model %>% fit(predictions_prob_matrix,
                     as.numeric(y_val)-1, 
                     epochs = 1000, #1000
                     batch_size = 6,#6
                     callbacks = callbacks)
  return(final_model)
}


###############################################
############ Predict Final Model ##############
###############################################


predict_final_model <- function(nn_models, adaboost, gradboost, rf_all, rf_usf, rf_opt, rf_ens, x_test, final_model, drop_columns, alpha){
  predictions_prob <- list()
  
  pred_prob_rf_all <- unname(predict(rf_all, x_test, type='prob')[,2])
  x_test_usf <- clean_datas(x_test, only_useful=TRUE)
  pred_prob_rf_usf <- unname(predict(rf_usf, x_test_usf, type='prob')[,2])
  x_test_opt <- clean_datas(x_test_usf, drop_columns=drop_columns)
  pred_prob_rf_opt <- unname(predict(rf_opt, x_test_opt, type='prob')[,2])
  
  predictions_prob <- append(predictions_prob, list(pred_prob_rf_all))
  predictions_prob <- append(predictions_prob, list(pred_prob_rf_usf))
  predictions_prob <- append(predictions_prob, list(pred_prob_rf_opt))
  
  votes_proba <- matrix(0, nrow(x_test), 2)
  x_test_ensemble <- list(datas_train = x_test,
                         datas_useful = x_test_usf,
                         datas_optimized = x_test_opt)
  for (i in 1:length(rf_ens$classifiers)){
    datas_choice <- rf_ens$datas_to_be_used[[as.character(i)]]
    x_test_prepared <- x_test_ensemble[[datas_choice]]
    predicted <- predict(rf_ens$classifiers[[as.character(i)]], x_test_prepared, type='prob')
    votes_proba <- votes_proba + predicted
  }
  pred_prob_rf_ens <- (unname(votes_proba)/length(rf_ens$classifiers))[,2]
  predictions_prob <- append(predictions_prob, list(pred_prob_rf_ens))
  
  pred_prob_AB <- predict(adaboost, x_test, type='prob')
  predictions_prob <- append(predictions_prob, list(pred_prob_AB))
  
  pred_prob_GB <- predict(gradboost, as.matrix(x_test), type='prob')
  predictions_prob <- append(predictions_prob, list(pred_prob_GB))
  
  for (i in 1:length(nn_models$models)){
    model <- nn_models$models[[i]]
    scaler <- nn_models$scalers[[i]]
    x_test_scaled <- standardize_data(as.matrix(x_test), scaler)
    pred_prob_NN <- model %>% predict(x_test_scaled)
    predictions_prob <- append(predictions_prob, list(pred_prob_NN))
  }
  
  predictions_prob_matrix <- matrix(unlist(predictions_prob), ncol=length(predictions_prob), byrow=FALSE)
  predicted_proba <- final_model %>% predict(predictions_prob_matrix)
  return(predicted_proba/alpha)
}