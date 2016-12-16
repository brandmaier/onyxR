#
# Factor model with means
# ---------------
# Script by Yves Rosseel
# Downloaded from: http://lavaan.ugent.be/tutorial/means.html
# on 2017/12/16
#

require(onyxR)
require(lavaan)

HS.model <- '
# three-factor model
visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
# intercepts
x1 ~ 1
x2 ~ 1
x3 ~ 1
x4 ~ 1
x5 ~ 1
x6 ~ 1
x7 ~ 1
x8 ~ 1
x9 ~ 1
'

fit <- cfa(HS.model, 
           data = HolzingerSwineford1939, 
           meanstructure = TRUE)
summary(fit)


onyx(fit)