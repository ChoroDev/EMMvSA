library(e1071)
area.pallete = function (n = 3)
{
  cols = rainbow (n)
  cols [1:3] = c("PaleGreen", "PaleTurquoise", "Pink")
  return(cols)
}
symbols.pallete = c("SeaGreen", "Blue", "Red")
set.seed(0)
#C = 225 #train ideal
C = 0.1 #test ideal # with two errors but they are unavoidable for linear core
kernel = "linear"
data = iris [c("Petal.Width", "Petal.Length", "Species")]
trainIdx = sample(nrow(data), nrow(data) / 2, replace = FALSE)
train = data[trainIdx,]
dataTest = data[-trainIdx,]
objects = data[trainIdx, c("Petal.Width", "Petal.Length")]
testObjects = data[-trainIdx, c("Petal.Width", "Petal.Length")]
linearModel = svm(Species ~ ., data = train, type = "C-classification", cost = C, kernel = kernel)
plot(linearModel, dataTest, grid = 250, symbolPalette = symbols.pallete, color.palette = area.pallete)
forecastsTrain = predict(linearModel, objects)
print(table(train$"Species", forecastsTrain))
forecastsTest = predict(linearModel, testObjects)
cat("\n")
print(table(dataTest$"Species", forecastsTest))

