#' <h3>Regression Example</h3>

#' We'll model the ridership on the Chicago elevated trains as a function of the 14 day lagged ridership at two stations. The two predictors are in the same units (rides per day/1000) and do not need to be normalized.

#' All but the last week of data are used for training. The last week will be predicted after the model is fit.

#+ results = "hide", messages = FALSE
library(tidymodels)
tidymodels_prefer()
data(Chicago)

n <- nrow(Chicago)
Chicago <- Chicago %>% select(ridership, Clark_Lake, Quincy_Wells)

Chicago_train <- Chicago[1:(n - 7), ]
Chicago_test <- Chicago[(n - 6):n, ]


