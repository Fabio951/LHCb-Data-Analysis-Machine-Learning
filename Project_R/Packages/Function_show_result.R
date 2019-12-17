#######################################
############ Show result ##############
#######################################

show_result <- function(model, x_test, y_test){
  predicted <- predict(model, x_test)
  print(confusionMatrix(predicted, y_test))
  predicted_proba <- predict(model, x_test, type='prob')
  
  par(pin=c(4,4))
  hist(predicted_proba[,1][y_test==1],
       col = 'red',
       freq = FALSE,
       breaks = 20,
       density = 30,
       angle = 45,
       xlim = c(0,1),
       ylim = c(0,7.5),
       xlab = 'prob',
       ylab = 'frequency',
       main = 'Probability of being a background event')
  hist(predicted_proba[,1][y_test==0],
       col = 'green',
       freq = FALSE,
       breaks = 20,
       density = 30,
       add = TRUE,
       angle = -45)
  legend("top", c('Signal', 'Background'), fill=c('red', 'green'), cex=0.5)
}


#######################################################
############ Show result ensemble forest ##############
#######################################################


show_result_ensemble <- function(predicted_prob, y_test){
  predicted <- as.factor(as.vector(apply(predicted_prob, MARGIN=1, FUN=which.max))-1)
  print(confusionMatrix(predicted, y_test))
  
  par(pin=c(4,4))
  hist(predicted_prob[,1][y_test==1],
       col = 'red',
       freq = FALSE,
       breaks = 20,
       density = 30,
       angle = 45,
       xlim = c(0,1),
       ylim = c(0,7.5),
       xlab = 'prob',
       ylab = 'frequency',
       main = 'Probability of being a background event')
  hist(predicted_prob[,1][y_test==0],
       col = 'green',
       freq = FALSE,
       breaks = 20,
       density = 30,
       add = TRUE,
       angle = -45)
  legend("top", c('Signal', 'Background'), fill=c('red', 'green'), cex=0.5)
}


################################################
############ Show result AdaBoost ##############
################################################


show_result_AB <- function(predicted_prob, y_test){
  predicted <- as.factor(as.integer(round(predicted_prob)))
  print(confusionMatrix(predicted, y_test))
  
  par(pin=c(4,4))
  hist(predicted_prob[y_test==1],
       col = 'red',
       freq = FALSE,
       breaks = 20,
       density = 30,
       angle = 45,
       xlim = c(0,1),
       ylim = c(0,9),
       xlab = 'prob',
       ylab = 'frequency',
       main = 'Probability of being a Signal event')
  hist(predicted_prob[y_test==0],
       col = 'green',
       freq = FALSE,
       breaks = 20,
       density = 30,
       add = TRUE,
       angle = -45)
  legend("top", c('Signal', 'Background'), fill=c('red', 'green'), cex=0.5)
}