mosaicplot(HairEyeColor[,, "Female"], col = c("chocolate", "cornflowerblue", "salmon", "green"), main = "Female eye color vs. hair color")
chisq.test(HairEyeColor[,, "Female"], simulate.p.value = TRUE)

