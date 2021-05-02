library(e1071)
area.pallete = function (n = 3)
{
  cols = rainbow (n)
  cols[1:3] = c("PaleGreen", "PaleTurquoise", "Pink")
  return(cols)
}
symbols.pallete = c("SeaGreen", "Blue", "Red")
set.seed(0)
C = 1
kernel = "linear"
data = iris[c("Petal.Width", "Petal.Length", "Species")]
trainIdx = sample(nrow(data), nrow(data) / 2, replace = FALSE)
train = data[trainIdx,]
dataTest = data[-trainIdx,]
objects = data[trainIdx, c("Petal.Width", "Petal.Length")]
testObjects = data[-trainIdx, c("Petal.Width", "Petal.Length")]
linearModel = svm(Species ~ ., data = train, type = "C-classification", cost = C, kernel = kernel)
plot(linearModel, data, grid = 250, symbolPalette = symbols.pallete, color.palette = area.pallete)
predictionsTrain = predict(linearModel, objects)
print(table(train$"Species", predictionsTrain))
predictionsTest = predict(linearModel, testObjects)
cat("\n")
print(table(dataTest$"Species", predictionsTest))
cat("Support vector counts: ")
print(linearModel$nSV)

