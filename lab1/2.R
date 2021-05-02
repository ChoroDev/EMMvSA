n = 20
cur = as.integer(format(Sys.Date(), "%Y"))

rowsNum = 1 : n
names = 1 : n
birthYears = sample(1960 : 1985, n)
employYears = 1 : n
salaries = 1 : n

calcSalary = function (birthYear, employYear, currentYear = cur)
{
  if (birthYear > 1975)
  {
    return ((log(cur - employYear) + 1) * 8000)
  } 
  else
  {
    return ((log2(cur - employYear) + 1) * 8000)
  }
}

for (i in seq(n))
{
  names[i] = paste("Magomed", i, sep = "")
  employYears[i] = sample((birthYears[i] + 18) : 2006, 1)
  salaries[i] = calcSalary(birthYears[i], employYears[i])
}

f = data.frame(rowsNum, names, birthYears, employYears, salaries)
cat("Employees with salary > 15000: ", length(f[["salaries"]] > 15000), "\n")

taxes = 1 : n
for (i in seq(n))
{
  sal = 0
  for (c in employYears[i] : cur)
  {
    sal = sal + calcSalary(birthYears[i], employYears[i], c)
  }
  taxes[i] = sal * 0.13
}

f[["taxes"]] = taxes
