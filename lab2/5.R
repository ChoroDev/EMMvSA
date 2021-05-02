t = read.table(file.choose(), header = TRUE)
group1 = t[t$LearningType == "DRA",]
group1 = group1$Score

group2 = t[t$LearningType == "SC",]
group2 = group2$Score

t.test(group1, group2)

