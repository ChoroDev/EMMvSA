library(e1071)
area.pallete = function (n = 3)
{
  cols = rainbow (n)
  cols [1:3] = c("PaleGreen", "PaleTurquoise", "Pink")
  return(cols)
}
symbols.pallete = c("SeaGreen", "Blue", "Red")
set.seed(0)
C = 1
kernel = "polynomial"
degree = 1
data = iris [c("Petal.Width", "Petal.Length", "Species")]
trainIdx = sample(nrow(data), nrow(data) / 2, replace = FALSE)
train = data[trainIdx,]
dataTest = data[-trainIdx,]
objects = data[trainIdx, c("Petal.Width", "Petal.Length")]
testObjects = data[-trainIdx, c("Petal.Width", "Petal.Length")]
test_model = function(C, kernel, degree, gamma) 
{
  cat(paste("C = ", C, "; kernel = ", kernel, "; degree = ", degree, "; gamma = ", gamma, sep = ""), "\n")
  linearModel = svm(Species ~ ., data = train, type = "C-classification", cost = C, kernel = kernel, degree = degree, gamma = gamma)
  error_count = function(t) 
  {
    return (sum(t[c(2, 3, 4, 6, 7, 8)]))
  }
  forecastsTrain = predict(linearModel, objects)
  train_table = table(train$"Species", forecastsTrain)
  forecastsTest = predict(linearModel, testObjects)
  test_table = table(dataTest$"Species", forecastsTest)
  return (c(error_count(train_table), error_count(test_table)))
}
kernels = c("polynomial", "radial", "sigmoid")
values = c(0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50)
degrees = 1:15
gammas = c(0.1, 0.25, 0.5, 0.75, 1, 2, 5, 10, 25, 50)
dataFrame = data.frame("none", 0.0, 0, 0.0, 999999, 999999, 999999)
names(dataFrame)<-c("kernel", "C", "degree", "gamma", "errors", "train_errors", "test_errors")
add_row = function(dataFrame, C, kernel, degree, gamma) 
{
  errors = test_model(C, kernel, degree, gamma)
  total_error_count = sum(errors)
   
  de <- data.frame(kernel, C, degree, gamma, total_error_count, errors[1], errors[2])
  names(de) <- c("kernel", "C", "degree", "gamma", "errors", "train_errors", "test_errors")
  return (rbind(dataFrame, de))
}
for (kernel in kernels) 
{
  for (C in values) 
  {
    for (gamma in gammas) 
    {
      if (kernel != "polynomial") 
      {
        degree = 1
        dataFrame = add_row(dataFrame, C, kernel, degree, gamma)
        next
      }
      for (degree in degrees) 
      {
        dataFrame = add_row(dataFrame, C, kernel, degree, gamma)
      }
    }
  }
}
min_error_count = min(dataFrame$test_errors)
print(dataFrame[dataFrame$test_errors == min_error_count,])

