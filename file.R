
#Questão 1. Teste de Normalidade

csv= read.csv("data/Questao01.csv")

x = as.matrix(csv)


#Discuta (ao nı́vel de 5%) a normalidade desses dados.
shapiro.test(x)

### Com o p-value 0.09564, a H0 naão é rejeitadada. Sendo assim a distribuiçao indica uma distribuição normal


#Faça um histograma e um boxplot dos dados. O resultado sobre a normalidade se alteraria
#caso os outliers fossem desconsiderados?
hist(x)

boxplot(x)

### Removendo os outliers
IQR = IQR(x)

quartiles = quantile(x, probs=c(.25, .75), na.rm = FALSE)

Lower = quartiles[1] - 1.5*IQR
Upper = quartiles[2] + 1.5*IQR 

data_no_outlier = subset(x, x > Lower & x < Upper)

shapiro.test(data_no_outlier)

hist(data_no_outlier)

boxplot(data_no_outlier)

### O novo p-value é 0.6802, o que continua indicando uma distribuição normal

csv= read.csv("data/Questao02.csv")

x = as.matrix(csv)

# Questão 2. Teste para média em amostra única

# n = 50
# media= 1.0
# significância = 0,05

t.test(x, mu=1,  alternative="greater", conf.level = 0.95)

### a media possui 95 porcento de confiança que esteja acima de 1.05, sendo assim a H0 é aceita


# Teste para proporção em amostra única

# n = 580
# sucesso = 152
# H0 = 25% das ervilhas descendentes seriam
#amarelas.
# confianca = 95


 prop.test(x= 152, n= 580, p=0.25)
 
 ### com um p-value de 0.5331 não rejeita a H0, a proporção estando em um intervaldo de 0.2271103 e 0.3002337
