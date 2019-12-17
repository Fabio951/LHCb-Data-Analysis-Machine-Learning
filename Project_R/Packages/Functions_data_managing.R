#########################################################
############Load and manage datas - Basics###############
#########################################################


get_data <- function(file){
  data_folder <- 'Data_updated'
  if (file=="simulation_bkg"){
    file_name = file.path(data_folder,"background.txt")
  }
  else{
    if (file=="simulation_signal"){
      file_name <- file.path(data_folder,"MC_signal.txt")
    }
    else{
      if (file=="data"){ 
        file_name <- file.path(data_folder,"data_lhcb.txt")
      }
      else{
        stop("file should be 'simulation_bkg', 'simulation_signal' or 'data'.")
      }
    }
  }
  return(read.csv(file_name, row.names=1))
}


check_for_none <- function(data, file){
  check = is.null(data)
  if (check){
    cat(sprintf('File %s has some None values.', file))
    return()
  }
  else{
    return
  }
}


load_all_datas <- function(labels=TRUE, check_none=TRUE){
  files <- c('simulation_bkg', 'simulation_signal', 'data')
  datas <- list()
  if (labels==FALSE){
    for (file in files){
      datas[[file]] <- get_data(file)
    }
  }
  else{
    for (file in files){
      data <- get_data(file)
      check_for_none(data, file)
      if (file=='simulation_bkg'){
        data[['Label']] <- 0
      }
      if (file=='simulation_signal'){
        data[['Label']] <- 1
      }
      datas[[file]] <- data
    }
  }
  return(datas)
}


get_complementary_cut <- function(data){
    cut_1 <- data[["DeltaM_F"]]<360
    cut_2 <- data[["Lambda_b0_MM_F"]]>5550 
    cut_3 <- data[["Lambda_b0_MM_F"]]<5680
    data <- data[cut_1 | (cut_2 & cut_3),]
    return(data)
}


############################################################
############ Load and Manage datas - Advanced ##############
############################################################


clean_datas <- function(data, only_useful=FALSE, only_unbiased=FALSE, drop_columns=NULL){
  meaningless <- c("Lambda_b0_BKGCAT_F","lcstar_BKGCAT_F","Lambda_c_BKGCAT_F")
  biased <- c("lcstar_MM_F", "Lambda_b0_MM_F", "DeltaM_F", "pair_lcstar_F")
  useless <- c("tau_pion0_ProbNNpi_F", "tau_pion1_ProbNNpi_F", "tau_pion2_ProbNNpi_F", 
             "lcstar_pim_ProbNNpi_F", "lcstar_pip_ProbNNpi_F", "Lambda_b0_ENDVERTEX_CHI2_F", 
             "Lambda_c_ENDVERTEX_CHI2_F", "lcstar_ENDVERTEX_CHI2_F")
  drop <- meaningless
  if (only_useful){
    drop <- c(drop, useless)
    }
  if (only_unbiased){
    drop <- c(drop, biased)
  }
  if (!is.null(drop_columns)){
    drop <- c(drop, drop_columns)
  }
  
  data_cleaned <- data[ , !(names(data) %in% drop)]
  return(data_cleaned)
}


take_test_set <- function(datas, write_on_file=TRUE){
  data_sim_sign <- datas[['data_sig']]
  data_sim_sign <- data_sim_sign[sample(1:nrow(data_sim_sign), nrow(data_sim_sign)), ]
  data_bkg <- datas[['data_bkg']]
  data_bkg <- data_bkg[sample(1:nrow(data_bkg), nrow(data_bkg)), ]
  train_size = 0.7
  result <- list()
  data_sig_train <- data_sim_sign[1:as.integer(train_size*nrow(data_sim_sign)), ]
  data_bkg_train <- data_bkg[-(1:nrow(data_sig_train)), ]
  data_train <- list("data_bkg" = data_bkg_train,
                     'data_sig' = data_sig_train
                     )
  result[['data_train']] <- data_train
  
  data_sig_test <- data_sim_sign[-(1:as.integer(train_size*nrow(data_sim_sign))), ]
  data_bkg_test <- data_bkg[1:nrow(data_sig_test), ]
  data_test <- rbind(data_bkg_test, data_sig_test)
  data_test <- data_test[sample(1:nrow(data_test), nrow(data_test)), ]
  x_test <- data_test[, -length(data_test)]
  y_test <- data_test[[length(data_test)]]
  result[['x_test']] <- x_test
  result[['y_test']] <- as.factor(y_test)
  
  if(write_on_file){
    write.csv(data_test, 'Data_test/data_test.txt', row.names=FALSE)
  }
  
  return(result)
}


#################################################
############ Handle training datas ##############
#################################################


get_training_datas <- function(datas_train, n_samples_bkg=NULL, AB=FALSE){
  data_sig_train <- datas_train[['data_sig']]
  data_bkg <- datas_train[['data_bkg']]
  if (is.null(n_samples_bkg)){
    n_samples_bkg <- nrow(data_sig_train)*1100/1043
  }
  train_set_bkg <- sample(1:nrow(data_bkg), n_samples_bkg)
  data_bkg_train <- data_bkg[train_set_bkg, ]
  data_train <- rbind(data_bkg_train, data_sig_train)
  data_train <- data_train[sample(1:nrow(data_train), nrow(data_train)), ]
  if (AB){
    data_train$Label <- as.numeric(data_train$Label)*2-1
  }
  else{
    data_train[['Label']] <- as.factor(data_train[['Label']])
  }
  return(data_train)
}