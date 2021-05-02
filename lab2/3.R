ts = read.table(file.choose(), header = TRUE)
ts = na.omit(ts)
ts = ts[ts$population > 10,]
ts["area_log"] = log10(log10(ts$area))
ts["population_log"] = log10(log10(ts$population))
b0 = 0.2
b1 = 0.9
func = approx(ts$area, b0 + b1 * (ts$area_log), ts$area, method = "constant")$y
ks.test(ts$population_log, func)

