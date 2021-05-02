d = read.table("./reglab1.txt", header = TRUE)
f = lm(z ~ ., data = d)
summary(f)
f = lm(z ~ x + y, data = d)
summary(f)
f = lm(z ~ x * y, data = d)
summary(f)

