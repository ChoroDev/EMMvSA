intvls = function (x, k = length(x) / 10)
{
  minX = min(x)
  maxX = max(x)
  seqMy = seq(minX, maxX, by = (maxX - minX) / k)
  middles = seq(length(seqMy) - 1)
  counts = seq(length(seqMy) - 1)

  for (i in seq(length(seqMy) - 1))
  {
    a = seqMy[i]
    b = seqMy[i + 1]
    middles[i] = (a + b) / 2
    counts[i] = length(x[x >= a & x <= b])
  }
  plot(middles, counts)
}


x = rexp(5000, rate = 50)
intvls(x, length(x) / 100)

x11()
plot(1 : 5000, x, type = "p")

