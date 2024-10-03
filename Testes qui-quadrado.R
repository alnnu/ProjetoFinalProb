#################################################################
############### Aula 01 de Outubro #############################
#################################################################


##### Testes Qui-QUadrado #######


### Exemplo 1 - Testes de Aderencia ###

x=c(2,5,16,42,69,51,32,23,9,1)
y=c(1.82, 6.58, 19.40, 39.92, 57.28, 57.28, 39.92, 19.40, 6.58, 1.82)


## Calcular a media e variancia dos dados observados
# Note que usamos o vetor c(10:19) dos pontos medios dos intervalos observados
# e x eh a frequencia observada em cada intervalo

media = sum(x*(10:19))/250; media
var = sum(x*(media-c(10:19))^2)/250; var

## Calcular as frequencias esperadas
# Note que o primeiro e o ultimo intervalos sao "abertos no infinito"
p = c(pnorm(q=10.5, mean=media, sd=sqrt(var)),
      pnorm(q=seq(11.5, 18.5, by=1), mean=media, sd=sqrt(var))-
      pnorm(q=seq(10.5, 17.5, by=1), mean=media, sd=sqrt(var)),
      1-pnorm(q=18.5, mean=media, sd=sqrt(var))); p

# verificar que a prob total eh 1:
sum(p)

# Calcular a frequencia absoluta esperada baseada nas probs
250*p      

## Por fim, fazer o teste qui-quadrado
chisq.test(x=x, p=p)


## Se quisessemos verificar que a estatistica observada esta na regisao de nao rejeicao
k=length(x)
alpha=0.05
chi_teor_1=qchisq(p=alpha/2, df=k-1); chi_teor_1
chi_teor_2=qchisq(p=1-alpha/2, df=k-1); chi_teor_2

chi_obs=as.numeric(chisq.test(x=x, p=p)$statistic); chi_obs

chi_obs < chi_teor_1 | chi_obs > chi_teor_2  # se TRUE rejeita H_0 ----- se FALSE não rejeita H_0




### Exemplo 2 - Testes de Aderencia ###

x=c(30, 26, 10, 5, 5, 4)

# Queremos verificar se os dados tem distribuicao Geometrica com p=0.4

## Calcular as frequencias esperadas
p = c(pgeom(q=0, prob=0.4),
      pgeom(q=seq(1,4, by=1), prob=0.4)-
        pgeom(q=seq(0,3, by=1), prob=0.4),
      1-pgeom(q=4, prob=0.4)); p

# verificar que a prob total eh 1:
sum(p)

# Calcular a frequencia absoluta esperada baseada nas probs
80*p      

## Por fim, fazer o teste qui-quadrado
chisq.test(x=x, p=p)






### Exemplo 3 - Testes para Independencia ###
 
#Notas de matematica
alta=c(56,47,14)
media=c(71,163,42)
baixa=c(12,38,85)

# construir a tabela dfe contingencia
data = data.frame(alta,media,baixa, row.names = c("alta","media","baixa"))
data

# realizar o teste
chisq.test(x=data)




### Exemplo 4 - Testes para Independencia ###

#Casos de Gripe
sim=c(27,42)
nao=c(34,47)


# construir a tabela dfe contingencia
data = data.frame(sim, nao, row.names = c("sim","nao"))
data

# realizar o teste
chisq.test(x=data)





### Exemplo 5 - Testes para Homogeneidade ###

# frequencia por tipo de filme
policial=c(45,36,39,14)
comedia=c(25,61,36,19)
romance=c(30,43,35,17)


# construir a tabela de contingencia
data=data.frame(policial,comedia,romance,row.names=c("solteiro","casado","divorciado","viuvo"))
data

# relizar o teste
chisq.test(x=data)





### Exemplo 6 - Testes para Homogeneidade ###

# frequencia opniao do atendimento
Bom=c(73,94)
Regular=c(37,61)
Ruim=c(40,45)


# construir a tabela de contingencia
data=data.frame(Bom, Regular, Ruim, row.names=c("A","B"))
data

# relizar o teste
chisq.test(x=data)

