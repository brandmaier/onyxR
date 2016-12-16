
#
# Mediation model
# ---------------
# Script by Yves Rosseel
# Downloaded from: http://lavaan.ugent.be/tutorial/mediation.html
# on 2017/12/16
#

require("onyxR")

set.seed(1234)
X <- rnorm(100)
M <- 0.5*X + rnorm(100)
Y <- 0.7*M + rnorm(100)
Data <- data.frame(X = X, Y = Y, M = M)
model <- ' # direct effect
             Y ~ c*X
           # mediator
             M ~ a*X
             Y ~ b*M
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '
fit <- sem(model, data = Data)
summary(fit)


onyx(fit)