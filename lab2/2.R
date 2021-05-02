kss = seq(10, 30, 5)
n = 200
alph = 0.05
norm = c()
chiss = c()
for (k in kss) 
{
  ns = rnorm(n, mean = k, sd = sqrt(k))
  chis = rchisq(n, k)
  norm = c(norm, ns)
  chiss = c(chiss, chis)
  tst = ks.test(ns, chis)
  print(c(k, tst["p.value"] > alph))
}

