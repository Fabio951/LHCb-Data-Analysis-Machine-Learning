# LHCb_Machine_Learning
Multivariate analysis in particle physics and separation of signal from background, using Deep Neural Networkand Machine Learning techniques.

This project is best explained in the PDF presentation.

It is done both in Python and R. However, it has been finalized, cleaned and optimized just for R, therefore the Python project is just a draft. Nonetheless, the Python project contains 90% of the R project; in particular the backbone is the same but the final model has been implemented just in the R project. The results obtained with the two programming languages are analogous.

The aim of the project is to use Machine Learning techniques in order to analyze big data (almost 500.000 samples) regarding the LHCb experiment. In particular the objective is to train a model to separate the few interesting data (7.000 out of 500.000) from the background. 

The results have been compared and discussed with the CERN Physicist who originally performed the data analysis, with interesting outcomes.

The Machine Learning techniques exploited in this project are:
- Strong Data Engineering
- Random Forests
- Ensemble of Random Forests
- Adaptive Boosting with Random Forests
- Gradient Boosting with Random Forests
- Deep Neural Networks
- Ensemble of all the previous classifier joined with a perceptron
