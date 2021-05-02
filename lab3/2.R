t = read.table("./reglab2.txt", header = TRUE)
g = colnames(t)[2 : length(colnames(t))]

for(i in seq(length(g)))
{
  combs = combn(g, i)
  
  ii = dim(combs)[1]
  jj = dim(combs)[2]

  for(j in seq(jj))
  {
    names = c()
    for(i in seq(ii)){
    names = c(names, combs[i, j])
    }
    
    names = c("y", names)
    it = t[names]
    cat(c(names, ";", lm(y ~ ., data = it)$coefficients, "\n"))
  }
}

sm = sum(f$residuals)
