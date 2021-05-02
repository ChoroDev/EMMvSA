n = 1000
s = rnorm(n, mean = 1, sd = 0.3)
c(sum(s) / n, mean(s))
p = 0
for(i in seq(n))
{
  p = p + (s[i] - mean(s))^2 / (n - 1)
}
c(sqrt(p), sd(s))
c(sort(s)[round(n * 0.95 + 1)], quantile(s, probs = seq(0, 1, 0.95))["95%"])
c(sort(s)[round(n * 0.99 + 1)], quantile(s, probs = seq(0, 1, 0.99))["99%"])

