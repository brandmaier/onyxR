#
# Growth Curve model
# ---------------
# Script by Yves Rosseel
# Downloaded from: http://lavaan.ugent.be/tutorial/growth.html
# on 2017/12/16
#

model <- ' i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
           s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 '
fit <- growth(model, data=Demo.growth)
summary(fit)

onyx(fit)