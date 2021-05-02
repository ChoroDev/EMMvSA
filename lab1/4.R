age <- c(5, 9, 9, 10, 10, 11, 11, 12, 13, 13, 14, 14, 15, 15, 18, 18)
fiveYear <- c(70, 150, 260, 230, 255, 165, 225, 340, 305, 335, 290, 340, 225, 300, 380, 400)

lsr = function (x, y) 
{
    sampleCount = dim(x)[1]
    featureCount = dim(x)[2] + 1 # 1 is for intercept

    newX = array(c(x, rep(1, sampleCount)), c(sampleCount, featureCount))

    s = svd(newX)
    
    dPlus = array(0, c(featureCount, featureCount))
    for (i in 1 : featureCount) 
    {
        dPlus[i, i] = 1 / s$d[i]
    }

    xPlus = s$v %*% dPlus %*% t(s$u)

    w = xPlus %*% y
    return (w)
}

mod <- lm(fiveYear ~ age)

x = array(age, c(length(age), 1))
y = array(fiveYear, c(length(fiveYear), 1))

coeffs = lsr(x, y)
print(coeffs)
print(coefficients(mod))

plot(age, fiveYear, pch = 16, xlab = "Age (years)", ylab = "Five Year Growth (cm)", main = "Scatterplot of Age vs Five Year Growth")

abline(a = coeffs[2], b = coeffs[1], lwd = 2, lty = 2, col = "red")
grid()
