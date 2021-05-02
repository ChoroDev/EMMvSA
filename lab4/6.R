library(e1071)
mse <- function(y_pred, y_true) mean((y_pred - y_true)^2)
set.seed(0)
x = seq(0.1, 5, by = 0.05)
y = log(x) + rnorm(x, sd = 0.3)

test_model = function(eps, cost = 1) 
{
  svmModel = svm(x, y, type = "eps-regression", kernel = "radial", eps = eps, cost = cost)
  predctions = predict(svmModel, x)
  e = mse(predctions, y)
  return (e)
}
epsValues = c(0.00000001, 0.00000005, 0.0000001, 0.0000005, 0.000001, 0.000005, 0.00001, 0.00005, 0.0001, 0.0005, 0.001, 0.005, seq(0.01, 1, by = 0.005)) # , seq(0.01, 1, by = 0.005)
mseValues = list()
for (i in 1 : length(epsValues)) 
{
  eps = epsValues[i]
  e = test_model(eps)
  mseValues[[i]] = e
}
plot(epsValues, mseValues, type="l", col = "red", xlab = "eps", ylab = "mse")
grid()
plot(x, y)
svmModel = svm(x, y, type = "eps-regression", eps = 0.6, cost = 1)
points(x[svmModel$index], y[svmModel$index], col = "red")
predctions = predict(svmModel, x)
lines(x, predctions, col = "dodgerblue", lwd = 2)
lines(x, predctions + svmModel$epsilon, col = "cyan")
lines(x, predctions - svmModel$epsilon, col = "cyan")
grid()

