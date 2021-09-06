library(readxl)
library(dplyr)
library(stringr)
library(geosphere)

##### médicos #####

medicos <- read_xlsx('./data/dados_raw.xlsx', sheet = 2)

str_split(medicos$Local, ' ') %>% 
  sapply(function(x) {
    return(x[2])
  }) -> medicos$Local

head(medicos)

write.table(medicos, './data/medicos.csv', sep=',', row.names = F)

##### pacientes #####

pacientes <- read_xlsx('./data/dados_raw.xlsx')
head(pacientes)

# distância
dist <- numeric(length(pacientes$Medico))

for(i in seq_along(pacientes$Medico)){
  ind <- grep(pacientes[i,3], LETTERS[1:7])
  dist[i] <- distHaversine(pacientes[i,c(2,1)], medicos[ind, c(3,2)])
}

pacientes$dist <- round(dist / 1000, 3)


# categorizando idade

pacientes$categoria <- case_when(
  pacientes$Idade <= 20 ~ 'menor que 20',
  pacientes$Idade <= 40 ~ '20 a 40',
  pacientes$Idade <= 60 ~ '40 a 60',
  TRUE ~ 'acima de 60'
)


write.table(pacientes, './data/pacientes.csv', sep=',', row.names = F)

