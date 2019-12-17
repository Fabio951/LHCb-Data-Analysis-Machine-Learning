The Custom_method.R file contains two things:
1_ it redefines the Random Forest type in order to improve the grid search on it and 
2_ it defines a new classifier which is an ensemble of Random Forests.

The Function_bkg_samples_number.R file plots the prediction distribution divided by label of the samples, in order to find the optimal signal/background ratio.

The Function_find_best_parameters.R file prints and plots the result the best parameters to be used when a classifier is passed.

The Function_show_result.R file contains the functions used to print and plot the results of the optimal classifier.

The Functions_AdaBoost.R file contains all the function needed by the AdaBoost classifier.

The Functions_GradBoost.R file contains all the function needed by the GradBoost classifier.

The Functions_NN.R file contains all the function needed by the Neural Networks.

The Functions_data_managing.R file contains all the functions needed to handle the raw data.

The Functions_final_model.R file contains all the functions needed to initialize and train the Final Model, which is a combination of various classifiers, as explained in the PDF presentation.

The Functions_show_datas.R file contains the functions needed to show the raw data.

The Requirements.R file contains all the required packages.
