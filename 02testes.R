library(ggplot2)
library(gridExtra)
library(dplyr)
library(leaflet)
library(corrplot)
library(leaflet)
library(readxl)
library(car)
library(PMCMR)

pacientes <- read.table('./data/pacientes.csv', sep=',', h=T)
medicos <- read_xlsx('./data/dados_raw.xlsx', sheet = 2)

pacientes <- pacientes %>% filter(dist < 50)

shapiro.test(pacientes$Idade)
shapiro.test(pacientes$dist)

nortest::ad.test(pacientes$Idade)
nortest::pearson.test(pacientes$Idade)

#anova distacia para medico, MOIO
model<-aov(dist~Medico, pacientes)
model
summary(model)
# A partir da estatística F e seu valor-p abaixo de 0.05 podemos temos 
# embasamento estatístico para afirmar com grande confiança que as médias de 
# distancia dos pacientes difere entre os locais das UBS analisado.

#testando homostetaticidade
pacientes$Medico<-as.factor(pacientes$Medico)
leveneTest(dist~Medico, pacientes)
# A hipótese nula do Teste de Levene é de que não há diferença entre as 
# variâncias dos grupos. O valor-p menor do que 0.05 nos dá embasamento 
# estatistico para afirmar que as variâncias são de fato diferente e portanto 
# nossos dados são heterogeneos
shapiro.test(resid(model))
# A hipótese nula do Teste de Shapiro-Wilk é de que não há diferença entre a 
# nossa distribuição dos dados e a distribuição normal. O valor-p menor do que  
# 0.05 nos dá uma confiança estatística para afirmar que as distribuição dos  
# nossos resíduos se difere da distribuição normal
TukeyHSD(model)
# as medias naõ se diferem em ,b-a,d-c,e-c,f-c,g-c,e-d,f-d,g-d,f-e,g-e

#anova distacia para medico,
model<-aov(dist~categoria, pacientes)
model
summary(model)
# A partir da estatística F e seu valor-p acima de 0.05 temos 
# embasamento estatístico para afirmar com grande confiança que as médias de 
# distancia dos pacientes  não se difere entre a idade dos pacientes.
pacientes$categoria<-as.factor(pacientes$categoria)
leveneTest(dist~categoria, pacientes)
# A hipótese nula do Teste de Levene é de que não há diferença entre as 
# variâncias dos grupos. O valor-p maior do que 0.05 nos dá uma 
# confiança estatística para afirmar que as variâncias são de fato 
# iguais e portanto nossos dados são homogêneos.
shapiro.test(resid(model))
# A hipótese nula do Teste de Shapiro-Wilk é de que não há diferença entre a 
# distribuição dos dados e a distribuição normal. O valor-p menor do que 0.05 
# nossa nos dá uma confiança estatística para afirmar que as distribuição dos 
# nossos resíduos se difere da distribuição normal
TukeyHSD(model)
# Nenhuma medida se difere para a idade

# anova distacia para medico e idade
model<-aov(dist~categoria+Medico, pacientes)
model
summary(model)
# A partir da estatística F e seu valor-p acima de 0.05 temos 
# embasamento estatístico para afirmar com grande confiança que as médias de 
# distancia dos pacientes  não se difere entre a idade dos pacientes mas se 
# difere para medico.
shapiro.test(resid(model))
# A hipótese nula do Teste de Shapiro-Wilk é de que não há diferença entre a 
# nossa distribuição dos dados e a distribuição normal. O valor-p menor do que  
# 0.05 nos dá uma confiança estatística para afirmar que as distribuição dos  
# nossos resíduos se difere da distribuição normal
TukeyHSD(model)
# Nenhuma medida se difere para a idade
# as medias naõ se diferem em ,b-a,d-c,e-c,f-c,g-c,e-d,f-d,g-d,f-e,g-e
# Kruskal Wallis
kruskal.test(pacientes$dist,pacientes$Medico)
#Existe evidências para rejeitar H0 , ou seja, há
#evidências de que os medicos apresentam diferenças eentre as distancias.
kruskal.test(pacientes$dist,pacientes$categoria)
#Existe evidências para rejeitar H0 , ou seja, há
#evidências de que as categorias de idade apresentam 
# diferenças eentre as distancias.


# teste de Nemeyni
posthoc.kruskal.nemenyi.test(pacientes$dist~pacientes$Medico)
# existe diferença entre a-b,a-c,a-d,a-e,a-f,a-g,b-c,b-d,b-e,b-f,b-g,c-f,e-f,f-g
posthoc.kruskal.nemenyi.test(pacientes$dist~pacientes$categoria)
# Não existe diferença

library(FSA)

a <- dunnTest(pacientes$dist~pacientes$Medico)

a[[3]]

medias <- pacientes %>% group_by(Medico) %>% 
  summarise(media=mean(dist))

medias[order(medias[,2], decreasing = T),]

# compração com dunnTest

# medico A: A
# medico B: A
# medico G: B
# medico C: B
# medico D: BC
# medico E: BC
# medico F: C
