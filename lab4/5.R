library(e1071)
area.pallete = function (n = 3)
{
  cols = rainbow (n)
  cols [1 : 3] = c("PaleGreen", "PaleTurquoise", "Pink")
  return(cols)
}
symbols.pallete = c("SeaGreen", "Blue", "Red")
set.seed(0)
C = 1
kernel = "polynomial"
degree = 1
data = iris[c("Petal.Width", "Petal.Length", "Species")]
trainIdx = sample(nrow(data), nrow(data) / 2, replace = FALSE)
train = data[trainIdx,]
dataTest = data[-trainIdx,]
objects = data[trainIdx, c("Petal.Width", "Petal.Length")]
testObjects = data[-trainIdx, c("Petal.Width", "Petal.Length")]
test_model = function(C, kernel, degree, gamma) 
{
  linearModel = svm(Species ~ ., data = train, type = "C-classification", cost = C, kernel = kernel, degree = degree, gamma = gamma)
  error_count = function(t) 
  {
    return (sum(t[c(2, 3, 4, 6, 7, 8)]))
  }
  forecastsTrain = predict(linearModel, objects)
  train_table = table(train$"Species", forecastsTrain)
  plot(linearModel, train, grid = 250, symbolPalette = symbols.pallete, color.palette = area.pallete)
  x11()
  forecastsTest = predict(linearModel, testObjects)
  test_table = table(dataTest$"Species", forecastsTest)
  plot(linearModel, dataTest, grid = 250, symbolPalette = symbols.pallete, color.palette = area.pallete)
  return (c(error_count(train_table), error_count(test_table)))
}
kernel = "radial"
C = 1
errors = test_model(C, kernel, 1, 500)
print(errors)

