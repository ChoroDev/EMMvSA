t = read.table("./alligators.txt", header = TRUE)
t = t[, c("Length", "Weight")]
model = nls(Weight ~ b1 * Length^2 + b2, data = t, start = list(b1 = 6, b2 = 6))
summary(model)
plot(t$Length, predict(model, newdata = t))
lines(t$Length, t$Weight)

