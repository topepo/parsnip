#' <h3>Classification Example</h3>

#' We'll predict the island where the penguins were observed with two variables in the same unit (mm): bill length and bill depth.

#+ results = "hide", messages = FALSE
library(tidymodels)
tidymodels_prefer()
data(penguins)

penguins <- penguins %>% select(island, starts_with("bill_"))
penguins_train <- penguins[-c(21, 153, 31, 277, 1), ]
penguins_test  <- penguins[ c(21, 153, 31, 277, 1), ]
