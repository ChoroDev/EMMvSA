library(MASS)
library(datasets)

x = longley
x$Population < NULL
f = lm.ridge(Armed.Forces ~ ., data = x, lambda = 10^(-3 + 0.2 * seq(0, 25)))
plot(f)

