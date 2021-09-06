library(ggplot2)
library(gridExtra)
library(dplyr)
library(leaflet)
library(corrplot)
library(leaflet)

pacientes <- read.table('./data/pacientes.csv', sep=',', h=T)

######################################################

fill_color <- 'royalblue'

######################################################


######################################################

# gráfico de histograma com boxplot
hist_boxplot <- function(var) {
  g1 <- ggplot(pacientes, aes(var)) + geom_histogram(fill=fill_color, color='white') +
    theme_minimal() + xlim(range(var))
  
  g2 <- ggplot(pacientes, aes(var)) + geom_boxplot(fill=fill_color, color='black') +
    theme_minimal() + xlim(range(var))
  
  grid.arrange(g1,g2, ncol=1)
}


# gráfico de boxplot de acordo com o médico
boxplot_medico <- function(var) {
  ggplot(pacientes, aes(x=Medico, y=var)) + geom_boxplot(fill= fill_color) +
    theme_minimal() 
}



# função para fazer tabelas
make_table <- function(table, align='c', booktabs=T){
  kable(table, align = align, booktabs=booktabs)
}
