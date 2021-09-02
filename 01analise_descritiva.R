library(ggplot2)
library(gridExtra)
library(dplyr)
library(leaflet)
library(corrplot)

pacientes <- read.table('pacientes.csv', sep=',', h=T)
head(pacientes)

######################################################

summary(pacientes$Longitude)
summary(pacientes$Latitude)

# medidas para a idade
pacientes %>% group_by(Medico) %>% 
  summarise(min = min(Idade), 
            media = mean(Idade),
            mediana = median(Idade),
            max = max(Idade),
            variância = var(Idade)
  )

# medidas para a distância
pacientes %>% group_by(Medico) %>% 
  summarise(min = min(dist), 
            media = mean(dist),
            mediana = median(dist),
            max = max(dist),
            variância = var(dist)
  )

######################################################

fill_color <- 'royalblue'

######################################################

# frequência de atendimento por médicos
pacientes %>%
  ggplot(aes(x=Medico)) + geom_bar(fill=fill_color) + theme_minimal()


######################################################

# gráfico de histograma com boxplot
hist_boxplot <- function(var) {
  g1 <- ggplot(pacientes, aes(var)) + geom_histogram(fill=fill_color, color='white') +
    theme_minimal() + xlim(range(var))
  
  g2 <- ggplot(pacientes, aes(var)) + geom_boxplot(fill=fill_color, color='black') +
    theme_minimal() + xlim(range(var))
  
  grid.arrange(g1,g2, ncol=1)
}

# distribuição da idade
hist_boxplot(pacientes$Idade)

# distribuição da distância
hist_boxplot(pacientes$dist)

######################################################

# gráfico de boxplot de acordo com o médico
boxplot_idade <- function(var) {
  ggplot(pacientes, aes(x=Medico, y=var)) + geom_boxplot(fill= fill_color) +
    theme_minimal() 
}

# boxplot para a idade de acordo com o médico
boxplot_idade(pacientes$Idade)

# boxplot para a distância de acordo com o médico
boxplot_idade(pacientes$dist)


######################################################

# correlação
plot(pacientes[,4:5])

######################################################

# mapas