library(readxl)
library(dplyr)
library(stringr)
library(geosphere)

##### médicos #####

medicos <- read_xlsx('dados_raw.xlsx', sheet = 2)

str_split(medicos$Local, ' ') %>% 
  sapply(function(x) {
    return(x[2])
  }) -> medicos$Local

head(medicos)

write.table(medicos, 'medicos.csv', sep=',', row.names = F)

##### pacientes #####

pacientes <- read_xlsx('dados_raw.xlsx')
head(pacientes)

dist <- numeric(length(pacientes$Medico))

for(i in seq_along(pacientes$Medico)){
  ind <- grep(pacientes[i,3], LETTERS[1:7])
  dist[i] <- distHaversine(pacientes[i,c(2,1)], medicos[ind, c(3,2)])
}

pacientes$dist <- dist / 1000

pacientes<-pacientes[-c(103,104,561,562,563,993,994,1690,1901),]

# categorizando idade

pacientes$Categoria[pacientes$Idade < 40] = "menor que 40"
pacientes$Categoria[pacientes$Idade >= 40 & pacientes$Idade < 60] = "40 a 60"
pacientes$Categoria[pacientes$Idade >= 60] = "maior que 60"


write.table(pacientes, 'pacientes.csv', sep=',', row.names = F)

