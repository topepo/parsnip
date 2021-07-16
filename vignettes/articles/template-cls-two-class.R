#' <h3>Classification Example</h3>

#' The example data has two predictors and an outcome with two classes. Both predictors are in the same units

#+ results = "hide", messages = FALSE
library(tidymodels)
tidymodels_prefer()
data(two_class_dat)

data_train <- two_class_dat[-(1:10), ]
data_test  <- two_class_dat[  1:10 , ]
