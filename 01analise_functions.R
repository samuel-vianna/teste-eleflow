library(ggplot2)
library(gridExtra)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(corrplot)
library(leaflet)
library(readxl)
library(nortest)
library(stringr)

pacientes <- read.table('./data/pacientes.csv', sep=',', h=T)
medicos <- read_xlsx('./data/dados_raw.xlsx', sheet = 2)

######################################################

fill_color <- 'royalblue'

######################################################


######################################################

# grï¿½fico de histograma com boxplot
hist_boxplot <- function(var) {
  g1 <- ggplot(pacientes, aes(var)) + geom_histogram(fill=fill_color, color='white') +
    theme_minimal() + xlim(range(var))
  
  g2 <- ggplot(pacientes, aes(var)) + geom_boxplot(fill=fill_color, color='black') +
    theme_minimal() + xlim(range(var))
  
  grid.arrange(g1,g2, ncol=1)
}


# grï¿½fico de boxplot de acordo com o mï¿½dico
boxplot_medico <- function(var) {
  ggplot(pacientes, aes(x=Medico, y=var)) + geom_boxplot(fill= fill_color) +
    theme_minimal() 
}



# funï¿½ï¿½o para fazer tabelas
make_table <- function(table, align='c', booktabs=T){
  kable(table, align = align, booktabs=booktabs)
}

# funï¿½ao mapa medico
mapa_geral<-function(radius, blur){
  leaflet(pacientes)  %>% 
    addProviderTiles("Esri") %>% 
    setView(-51.931180, -23.415453, zoom = 12) %>% 
    addHeatmap(lng=~Longitude,lat=~Latitude,max=100,radius=radius,blur=blur)
}

# funï¿½ao mapa medico
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
    setView(-51.931180, -23.415453, zoom = 12) %>% 
    addCircleMarkers(~Longitude, ~Latitude, weight = 3, radius=4, 
                     color=~cof(Medico), stroke = F, fillOpacity = 0.9)  %>%
    addLegend("bottomright", colors= c("red", "blue", "orange","black","green","pink","purple"), labels=c("A'", "B", "C","D","E","F","G"), title="Medico")%>%
    addAwesomeMarkers(lng = ~medicos$Longitude, lat=~medicos$Latitude, icon=icons,
                      popup = ~medicos$Local)
}

# funï¿½ao mapa medico
mapa_idade<-function(){
  cof <- colorFactor(c("red", "blue", "orange","black"), domain=c("20 a 40","40 a 60","acima de 60","menor que 20"))
  leaflet(pacientes)  %>% 
    addProviderTiles("Esri") %>% 
    setView(-51.931180, -23.415453, zoom = 12) %>% 
    addCircleMarkers(~Longitude, ~Latitude, weight = 3, radius=4, 
                     color=~cof(pacientes$categoria), stroke = F, fillOpacity = 0.9)  %>%
    addLegend("bottomright", colors= c("red", "blue", "orange"), labels=c("Menor que 40 anos","Entre 40 anos e 60 anos","Maior que 40 anos"), title="Idade")
}

# Funï¿½ï¿½o grafico idade pelo medico
idade_medico<-function(medico){
  dados<-filter(pacientes, Medico == medico)
  ggplot(dados,aes(x=categoria))+
    geom_bar(fill="royalblue")+
    labs(x="Categoria",y="Frequencia") +
    theme_minimal()
  
}

# função para testar normalidade

teste_normal <- function(var){
  shapiro <- shapiro.test(var)
  ad <- ad.test(var)
  cvm <- cvm.test(var)
  lillie <- lillie.test(var)
  
  rbind(shapiro,ad,cvm, lillie) %>% as.data.frame(row.names = F) %>%
    select(-c(statistic, data.name)) %>%
    relocate(method) %>% 
    mutate(method = str_replace_all(method, 'normality test', '')) %>% 
    mutate(teste = ifelse(p.value > 0.05, 'Não rejeita H0', 'Rejeita H0')) %>%
    return()
}

teste_normal(rnorm(1000))
