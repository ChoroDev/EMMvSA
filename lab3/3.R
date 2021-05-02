t = read.table("./cygage.txt", header = TRUE)
t = na.omit(t)
t["depthLog"] = log10(log10(t$Depth))
t["calAgeLog"] = log10(log10(t$calAge))
a0 = 0.2
a1 = 0.9
f = approx(t$calAgeLog, a0 + a1 * (t$depthLog), t$calAgeLog, method = "constant")$y
plot(f)
ks.test(t$calAgeLog, f)

