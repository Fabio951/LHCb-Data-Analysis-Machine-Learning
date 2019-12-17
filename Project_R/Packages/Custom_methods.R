######################################################################
############ Custom Method Random Forest #############################
######################################################################


customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree", "maxnodes", "nodesize", "importance"),
                                  class = c(rep('numeric',4), 'logical'),
                                  label = c("mtry", "ntree", "maxnodes", "nodesize", "importance"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y,
               mtry = param$mtry,
               ntree = param$ntree,
               maxnodes = param$maxnodes,
               nodesize = param$nodesize,
               importance = param$importance,
               ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes


########################################################################
############ Custom Method Ensemble Forest #############################
########################################################################
get_other_datas <- function(datas, useful=FALSE, optimized=FALSE, drop_columns=NULL){
  datas_useful <- list()
  for (i in 1:length(datas)){
    datas_useful[[names(datas[i])]] <- clean_datas(datas[[i]], only_useful=TRUE)
  }
  if (useful){
    return(datas_useful)
  }
  if (optimized){
    datas_optimized <- list()
    for (i in 1:length(datas_useful)){
      datas_optimized[[names(datas_useful[i])]] <- clean_datas(datas_useful[[i]], drop_columns=drop_columns)
    }
    return(datas_optimized)
  }
}

create_fit_EnsembleForest <- function(params_list, datas_train, nforests, same_params=FALSE, all_datas=TRUE,
                                      useful=FALSE, optimized=FALSE, drop_columns=NULL){
  classifiers <- list()
  datas_used_tot <- list()
  
  datas_useful <- get_other_datas(datas_train, useful=TRUE)
  datas_optimized <- get_other_datas(datas_train, optimized=TRUE, drop_columns=drop_columns)
  datas_to_be_used <- list(datas_train = datas_train,
                           datas_useful = datas_useful,
                           datas_optimized = datas_optimized)
  filter_datas <- c(all_datas, useful, optimized)
  datas_to_be_used <- datas_to_be_used[filter_datas]
  
  for (i in 1:nforests){
    datas_choice <- sample(1:length(datas_to_be_used), 1)
    datas <- datas_to_be_used[[datas_choice]]
    datas_used <- names(datas_to_be_used[datas_choice])
    if (datas_used=='datas_train'){
      params <- params_list$all_datas
    }
    else{
      params <- params_list$few_datas
    }
    data_to_train <- get_training_datas(datas)
    mtry <- params$mtry[sample(1:length(params$mtry), 1)]
    ntree <- params$ntree[sample(1:length(params$ntree), 1)]
    maxnodes <- params$maxnodes[sample(1:length(params$maxnodes), 1)]
    importance <- params$importance[sample(1:length(params$importance), 1)]
    nodesize <- params$nodesize[sample(1:length(params$nodesize), 1)]
    for_clf <- randomForest(Label ~ .,
                            data = data_to_train,
                            mtry = mtry,
                            ntree = ntree,
                            maxnodes = maxnodes,
                            importance = importance,
                            best_nodesize = nodesize)
    classifiers[[as.character(i)]] <- for_clf
    datas_used_tot[[as.character(i)]] <- datas_used
  }
  return_clf <- list(classifiers = classifiers,
                     datas_to_be_used = datas_used_tot)
  return(return_clf)
}

predict_ensemble_forest <- function(ensemble, x_test, drop_columns){
  votes_proba <- matrix(0, nrow(x_test), 2)
  x_test_useful <- clean_datas(x_test, only_useful=TRUE)
  x_test_optimized <- clean_datas(x_test_useful, drop_columns=drop_columns)
  x_test_all <- list(datas_train = x_test,
                     datas_useful = x_test_useful,
                     datas_optimized = x_test_optimized)
  
  for (i in 1:length(ensemble$classifiers)){
    datas_choice <- ensemble$datas_to_be_used[[as.character(i)]]
    x_test_prepared <- x_test_all[[datas_choice]]
    predicted <- predict(ensemble$classifiers[[as.character(i)]], x_test_prepared, type='prob')
    votes_proba <- votes_proba + predicted
  }
  return(votes_proba/length(ensemble$classifiers))
}