data = iris[, "Petal.Length"]
set.seed(0)
ts = shapiro.test(data)
p_value = ts["p_value"]
print(ts)

