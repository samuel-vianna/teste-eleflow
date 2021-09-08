library(ggplot2)
library(gridExtra)
library(dplyr)
library(leaflet)
library(corrplot)
library(leaflet)

pacientes <- read.table('C:/Users/vitor/Desktop/lab-trabalho1/data/pacientes.csv', sep=',', h=T)
medicos <- read_xlsx('C:/Users/vitor/Desktop/lab-trabalho1/data/dados_raw.xlsx', sheet = 2)

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

# funçao mapa medico
mapa_medico<-function(){
  cof <- colorFactor(c("red", "blue", "orange",'black',"green","pink","purple"), domain=c("A", "B", "C","D","E","F","G"))
  medicos$Local <- factor(medicos$Local)
  new <- c("red3","blue","orange","black","green","pink","purple")[medicos$Local]
  
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'white',
    library = 'ion',
    markerColor = new
  )
  leaflet(pacientes)  %>% 
    addProviderTiles("Esri") %>% 
    setView(-51.931180, -23.415453, zoom = 7) %>% 
    addCircleMarkers(~Longitude, ~Latitude, weight = 3, radius=4, 
                     color=~cof(Medico), stroke = F, fillOpacity = 0.9)  %>%
    addLegend("bottomright", colors= c("red", "blue", "orange","black","green","pink","purple"), labels=c("A'", "B", "C","D","E","F","G"), title="Medico")%>%
    addAwesomeMarkers(lng = ~medicos$Longitude, lat=~medicos$Latitude, icon=icons,
                      popup = ~medicos$Local)
}

# funçao mapa medico
mapa_idade<-function(){
  cof <- colorFactor(c("red", "blue", "orange","black"), domain=c("20 a 40","40 a 60","acima de 60","menor que 20"))
  leaflet(pacientes)  %>% 
    addProviderTiles("Esri") %>% 
    setView(-51.931180, -23.415453, zoom = 7) %>% 
    addCircleMarkers(~Longitude, ~Latitude, weight = 3, radius=4, 
                     color=~cof(pacientes$categoria), stroke = F, fillOpacity = 0.9)  %>%
    addLegend("bottomright", colors= c("red", "blue", "orange"), labels=c("Menor que 40 anos","Entre 40 anos e 60 anos","Maior que 40 anos"), title="Idade")
}

# Função grafico idade pelo medico
idade_medico<-function(medico){
  dados<-filter(pacientes, Medico == medico)
  ggplot(dados,aes(x=categoria))+
    geom_bar(fill="royalblue")+
    labs(x="Categoria",y="Frequencia")
  
}

