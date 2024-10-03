###################################################
##### Aula - 24 e 27 de setembro   ################
###################################################

### Exemplo 4
alpha=0.05
z_teorico = qnorm(p=alpha, mean=0, sd=1); z_teorico  # teste unilateral "menor"

media_teste=3
dp_pop=0.8

n=200
media_amostral=2.5
dp_amostral=dp_pop/sqrt(n); dp_amostral

z_obs = (media_amostral-media_teste)/dp_amostral; z_obs

z_obs < z_teorico   # se TRUE rejeita H_0 ----- se FALSE não rejeita H_0



### Exewmplo 5
alpha=0.10
t_teorico = qt(p=1-alpha, df=9); t_teorico  # teste unilateral "maior"

media_teste=1

n=10
media_amostral=1.004
dp_pop=0.003   # aproximado pelo DP da amostra

dp_amostral=dp_pop/sqrt(n); dp_amostral

t_obs = (media_amostral-media_teste)/dp_amostral; t_obs

t_obs > t_teorico   # se TRUE rejeita H_0 ----- se FALSE não rejeita H_0


### Exemplo 6
alpha=0.03
z_teorico = abs(qnorm(p=alpha/2, mean=0, sd=1)); z_teorico  # teste bilateral

prop_teste=0.40

n=400
prop_amostral=120/n; prop_amostral
dp_amostral=sqrt(prop_teste*(1-prop_teste)/n); dp_amostral

z_obs = (prop_amostral-prop_teste)/dp_amostral; z_obs

z_obs < -z_teorico | z_obs > z_teorico  # se TRUE rejeita H_0 ----- se FALSE não rejeita H_0



### Exemplo 7
alpha=0.10
chi_teorico = qchisq(p=1-alpha/2, df=23); chi_teorico  # teste bilateral

var_teste=300

n=24
var_amostra=400

chi_obs = (n-1)*var_amostra/var_teste; chi_obs

chi_obs > chi_teorico  # se TRUE rejeita H_0 ----- se FALSE não rejeita H_0


