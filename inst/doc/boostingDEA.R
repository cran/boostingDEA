## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(boostingDEA)
set.seed(1234)

## -----------------------------------------------------------------------------
data(banks)
banks

## -----------------------------------------------------------------------------
x <- 1:3
y <- 6
DEA_model <- DEA(banks,x,y)
pred_DEA <- predict(DEA_model, banks, x, y)
pred_DEA

## -----------------------------------------------------------------------------
x <- 1:3
y <- 6
FDH_model <- FDH(banks,x,y)
pred_FDH <- predict(FDH_model, banks, x, y)
pred_FDH

## ---- eval = FALSE------------------------------------------------------------
#  x <- 1:3
#  y <- 4:5
#  EATBoost_model <- EATBoost(banks, x, y,
#                             num.iterations = 4,
#                             num.leaves = 4,
#                             learning.rate = 0.6)

## ----bestEATBoost-------------------------------------------------------------
N <- nrow(banks)
x <- 1:3
y <- 4:5
selected <- sample(1:N, N * 0.8) # Training indexes
training <- banks[selected, ] # Training set
test <- banks[- selected, ] # Test set
grid_EATBoost <- bestEATBoost(training, test, x, y,
                             num.iterations = c(5,6,7),
                             learning.rate = c(0.4, 0.5, 0.6),
                             num.leaves = c(6,7,8),
                             verbose = FALSE)
head(grid_EATBoost)

## -----------------------------------------------------------------------------
EATboost_model_tuned <- EATBoost(banks, x, y,
                                 num.iterations = grid_EATBoost[1, "num.iterations"],
                                 learning.rate = grid_EATBoost[1, "learning.rate"],
                                 num.leaves = grid_EATBoost[1, "num.leaves"])
pred_EATBoost <- predict(EATboost_model_tuned, banks, x)
pred_EATBoost

## ---- eval = FALSE------------------------------------------------------------
#  x <- 1:3
#  y <- 6
#  MARSBoost_model <- MARSBoost(banks, x, y,
#                               num.iterations = 4,
#                               learning.rate = 0.6,
#                               num.terms = 4)

## ----bestMARSBoost------------------------------------------------------------
N <- nrow(banks)
x <- 1:3
y <- 6
selected <- sample(1:N, N * 0.8) # Training indexes
training <- banks[selected, ] # Training set
test <- banks[- selected, ] # Test set
grid_MARSBoost <- bestMARSBoost(training, test, x, y,
                              num.iterations = c(5,6,7),
                              learning.rate = c(0.4, 0.5, 0.6),
                              num.terms = c(6,7,8),
                              verbose = FALSE)
head(grid_MARSBoost)

## -----------------------------------------------------------------------------
MARSBoost_model_tuned <- MARSBoost(banks, x, y,
                                   num.iterations = grid_MARSBoost[1, "num.iterations"],
                                   learning.rate = grid_MARSBoost[1, "learning.rate"],
                                   num.terms = grid_MARSBoost[1, "num.terms"])
pred_MARSBoost <- predict(MARSBoost_model_tuned, banks, x)
pred_MARSBoost

## -----------------------------------------------------------------------------
x <- 1:3
y <- 6
efficiency(DEA_model, 
           measure = "rad.in",
           banks, x, y)

## -----------------------------------------------------------------------------
efficiency(FDH_model, 
           measure = "WAM",
           weights = "RAM",
           banks, x, y)

## -----------------------------------------------------------------------------
x <- 1:3
y <- 4:5
efficiency(EATboost_model_tuned, 
           measure = "Russell.out",
           heuristic = FALSE,
           banks, x, y)

## -----------------------------------------------------------------------------
efficiency(EATboost_model_tuned, 
           measure = "Russell.out",
           banks, x, y,
           heuristic = TRUE)

## -----------------------------------------------------------------------------
efficiency(MARSBoost_model_tuned, "rad.out", banks, x, 6)

