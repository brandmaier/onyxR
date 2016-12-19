#
# Multi group CFA
# ---------------
# Script by Yves Rosseel
# Downloaded from: http://lavaan.ugent.be/tutorial/groups.html
# on 2017/12/16
#
require("lavaan")

HS.model <- '  visual =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
fit <- cfa(HS.model, 
           data = HolzingerSwineford1939, 
           group = "school")
summary(fit)


onyx(fit)