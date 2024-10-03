
#Questão 1. Teste de Normalidade
csv= read.csv("data/Questao01.csv")

x = as.matrix(csv)

shapiro.test(x)

hist(x)

boxplot(x)

IQR = IQR(x)

quartiles = quantile(x, probs=c(.25, .75), na.rm = FALSE)

Lower = quartiles[1] - 1.5*IQR
Upper = quartiles[2] + 1.5*IQR 

data_no_outlier = subset(x, x > Lower & x < Upper)

shapiro.test(data_no_outlier)

hist(data_no_outlier)

boxplot(data_no_outlier)
