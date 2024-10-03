#############################################################
####### Aula 27 de Setembro - Testes de hipoteses  ##########
#############################################################

######### TESTE PARA NORMALIDADE EM UMA AMOTRA #############

## Exemplo 1

x1 = c(10.55, 12.27,  7.78, 11.28, 10.93,  8.14, 10.58,  7.15,  9.68, 11.72,
       6.44,  7.23,  9.40, 10.28,  9.81, 13.57, 10.18,  8.93, 12.08, 9.17,
       14.09,  9.16,  6.54,  8.28,  8.11, 11.29, 10.00, 12.55, 12.59,  7.73,
       8.63, 10.91,  8.69,  9.12,  9.06, 13.30,  9.89, 12.44, 9.85, 11.21,
       11.47,  9.48,  8.87, 10.59, 10.77,  9.31,  8.74, 11.12, 10.40,  8.52)

hist(x1)

shapiro.test(x1)


## Exemplo 2

x2 = c(0.20, 0.57, 0.13, 0.44, 1.83, 0.22, 1.37, 2.99, 1.51, 2.43,
       2.32, 1.67, 0.21, 3.27, 0.36, 0.44, 1.85, 2.79, 0.42, 2.69,
       0.71, 2.58, 0.13, 0.34, 0.29, 0.11, 1.46, 1.79, 0.02, 0.58,
       0.78, 0.31, 2.73, 1.13, 0.15, 0.20, 2.79, 1.57, 0.57, 0.38,
       0.14, 0.97, 0.05, 4.56, 1.28, 0.40, 1.32, 0.68, 1.77, 0.61)

hist(x2)

shapiro.test(x2)



######### TESTE PARA MEDIA DE UMA AMOSTRA #############

## Exemplo 3

x3 = c(5.03, 5.02, 4.95, 4.96, 5.01, 4.97, 4.90, 4.91, 4.90, 4.93)

mean(x3)


t.test(x3, mu=5, alternative="two.sided", conf.level = 0.95)



## Exemplo 4

x4 = c(19.8, 18.5, 17.6, 16.7, 15.8, 15.4, 14.1, 13.6, 11.9, 11.4, 11.4, 
       8.8, 7.5, 15.4, 15.4, 19.5, 14.9, 12.7, 11.9, 11.4, 10.1, 7.9)

shapiro.test(x4)

mean(x4)

t.test(x4, mu=12, alternative="two.sided", conf.level = 0.95)




######### TESTE PARA PROPORCOES EM UMA AMOSTRA #############

## Exemplo 5
x=8
n=100

x/n

prop.test(x=x, n=n, p=0.05, alternative="greater", conf.level=0.95)


## Exemplo 6
x6=c("A", "B", "A", "A", "A", "A", "A", "A", "A", "A", 
     "B", "A", "B", "A", "B", "A", "A", "A", "A", "A", 
     "B", "B", "A", "B", "A", "A", "A", "A", "A", "A", 
     "B", "A", "A", "A", "B", "B", "A", "B", "A", "B", 
     "A", "B", "A", "B", "A", "B", "A", "A", "A", "A")

table(x6)

x=35
n=50

x/n

prop.test(x=35, n=50, p=0.6, alternative="two.sided", conf.level=0.95)



######### TESTE PARA VARIANCIAS EM UMA AMOSTRA #############

## Exemplo 7
install.packages("EnvStats")
require(EnvStats)

x7 = c(1200, 1100, 900, 1250, 1300, 1290, 1100, 1060, 1180, 1120, 1160, 1140, 1190, 1110, 1100, 1220)

mean(x7)
var(x7)

varTest(x7, sigma.squared=9000, alternative="greater")



######### TESTE PARA COMPARAR DUAS MEDIAS COM AMOSTRAS PAREADAS #############

## Exemplo 8

x2 = c(120, 104, 93, 87, 85, 98, 102, 106, 88, 90)
y2 = c(116, 102, 90, 83, 86, 97, 98, 108, 82, 85)

mean(x2)
mean(y2)

t.test(x2,y2,alternative="g", paired=TRUE)


## Exemplo 9

x3 = c(8.1, 7.9, 6.8, 7.8, 7.6, 7.9, 5.7, 8.4, 8.0, 9.5, 8.0, 6.8)
y3 = c(11.6, 8.8, 9.9, 9.5, 11.6, 9.1, 10.6, 10.8, 13.4, 10.6, 10.5, 11.4)

mean(x3)
mean(y3)

t.test(x3,y3,alternative="less",paired=TRUE)




######### TESTE PARA COMPARAR DUAS MEDIAS COM AMOSTRAS INDEPENDENTES E VARIANCIAS IGUAIS #############

## Exemplo 10

x4 = c(45, 51,	50,	62,	43,	42,	53,	50,	48,	55, 56)
y4 = c(45,	35,	43,	59,	48,	45,	41,	43,	49,	39)

mean(x4)
mean(y4)

t.test(x4, y4, var.equal = TRUE)




## Exemplo 11

x5 = c(10, 13, 9, 10, 14, 13, 10, 15, 12, 10, 9, 10, 13, 14)
y5 = c(15, 12, 18, 16, 15, 17, 17, 15, 16, 17, 11, 17, 14)

mean(x5)
mean(y5)

t.test(x5, y5, alternative="t", paired=FALSE, var.equal = TRUE)





######### TESTE PARA COMPARAR DUAS MEDIAS COM AMOSTRAS INDEPENDENTES E VARIANCIAS DIFERENTES #############

## Exemplo 12

x6 = c(9, 11, 13, 17, 9, 8, 10, 13, 12, 9, 9, 9, 8, 8, 10, 12, 10, 8, 13, 9, 14, 8, 15, 14, 12, 12, 8, 8, 8, 13, 13, 8)
y6 = c(14, 13, 14, 10, 13, 15, 16, 13, 15, 16, 17, 13, 14, 12, 12, 14, 8, 12, 14, 12, 10, 17, 15, 14)

mean(x6)
mean(y6)

var(x6)
var(y6)

t.test(x6, y6, paired=FALSE, var.equal=FALSE)





######### TESTE PARA COMPARAR DUAS VARIANCIAS #############

## Exemplo 13

x7 = c(29.9, 29.8, 29.8, 29.7, 29.9, 29.8, 29.9, 29.9, 30.1, 29.9, 30.0, 30.0, 29.6, 30.4, 29.9)
y7 = c(29.8, 29.8, 30.4, 29.8, 30.5, 29.6, 29.3, 29.4, 30.3, 29.9, 29.7, 30.3, 30.4, 29.1, 30.0)

var(x7)
var(y7)

var.test(x7,y7,alternative="t",conf.level=0.95)





######### TESTE PARA COMPARAR DUAS PROPORCOES #############

## Exemplo 14

x = c(10, 27)
n = c(120, 260)

x/n


prop.test(x,n,alternative="t")


